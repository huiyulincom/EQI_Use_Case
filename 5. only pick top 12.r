library(sf)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)

# =========================================================
# 1. User inputs
# =========================================================
gdb_path_points   <- "C:/your_folder/first.gdb"
layer_points      <- "your_point_layer_name"      # contains Customer_Service_Requested

gdb_path_polygons <- "C:/your_folder/second.gdb"
layer_polygons    <- "your_polygon_layer_name"    # contains SCREEN

output_gdb   <- "C:/your_folder/output_maps.gdb"
output_layer <- "polygon_request_summary_top12"

field_request <- "Customer_Service_Requested"
field_screen  <- "SCREEN"

top_n <- 12

# Optional: if polygon layer already has a stable unique ID field, use it here.
# Otherwise keep NULL and the code will create polygon_id automatically.
polygon_id_field <- NULL
# polygon_id_field <- "OBJECTID"

# =========================================================
# 2. Read data
# =========================================================
points_sf   <- st_read(gdb_path_points,   layer = layer_points,   quiet = FALSE)
polygons_sf <- st_read(gdb_path_polygons, layer = layer_polygons, quiet = FALSE)

# =========================================================
# 3. Check required fields
# =========================================================
if (!(field_request %in% names(points_sf))) {
  stop(
    paste0(
      "Field '", field_request, "' was not found in point layer.\n",
      "Available fields:\n",
      paste(names(points_sf), collapse = ", ")
    )
  )
}

if (!(field_screen %in% names(polygons_sf))) {
  stop(
    paste0(
      "Field '", field_screen, "' was not found in polygon layer.\n",
      "Available fields:\n",
      paste(names(polygons_sf), collapse = ", ")
    )
  )
}

# =========================================================
# 4. Match CRS
# =========================================================
if (st_crs(points_sf) != st_crs(polygons_sf)) {
  polygons_sf <- st_transform(polygons_sf, st_crs(points_sf))
}

# =========================================================
# 5. Create polygon_id
# =========================================================
if (!is.null(polygon_id_field)) {
  if (!(polygon_id_field %in% names(polygons_sf))) {
    stop(
      paste0(
        "polygon_id_field '", polygon_id_field, "' was not found in polygon layer."
      )
    )
  }

  polygons_sf <- polygons_sf %>%
    mutate(polygon_id = as.character(.data[[polygon_id_field]]))
} else {
  polygons_sf <- polygons_sf %>%
    mutate(polygon_id = as.character(row_number()))
}

# =========================================================
# 6. Clean fields
# =========================================================
points_clean <- points_sf %>%
  select(all_of(field_request)) %>%
  mutate(
    Customer_Service_Requested = as.character(Customer_Service_Requested),
    Customer_Service_Requested = str_squish(Customer_Service_Requested),
    Customer_Service_Requested = na_if(Customer_Service_Requested, ""),
    Customer_Service_Requested = if_else(
      is.na(Customer_Service_Requested),
      "NA_Blank",
      Customer_Service_Requested
    )
  )

polygons_clean <- polygons_sf %>%
  select(polygon_id, all_of(field_screen)) %>%
  mutate(
    SCREEN = as.character(SCREEN),
    SCREEN = str_squish(SCREEN),
    SCREEN = na_if(SCREEN, ""),
    SCREEN = if_else(
      is.na(SCREEN),
      "NA_Blank",
      SCREEN
    )
  )

# =========================================================
# 7. Spatial join: assign each point to polygon
#    left = FALSE keeps only matched points inside polygons
# =========================================================
joined_sf <- st_join(points_clean, polygons_clean, left = FALSE)

# Optional QA check
cat("Matched points inside polygons:", nrow(joined_sf), "\n")

if (nrow(joined_sf) == 0) {
  stop("No points were matched to polygons. Check CRS, geometry, or spatial extent.")
}

# =========================================================
# 8. Find top 12 categories by total count
# =========================================================
category_totals_initial <- joined_sf %>%
  st_drop_geometry() %>%
  count(Customer_Service_Requested, name = "cat_total_n") %>%
  arrange(desc(cat_total_n))

top_categories <- category_totals_initial %>%
  slice_head(n = top_n) %>%
  pull(Customer_Service_Requested)

cat("\nTop categories selected:\n")
print(category_totals_initial %>% slice_head(n = top_n), n = Inf)

# =========================================================
# 9. Recode all other categories as "Other"
# =========================================================
joined_sf <- joined_sf %>%
  mutate(
    Customer_Service_Requested = if_else(
      Customer_Service_Requested %in% top_categories,
      Customer_Service_Requested,
      "Other"
    )
  )

# =========================================================
# 10. Recalculate counts after grouping into Other
# =========================================================

# Total number of all matched requests
total_requests_all <- nrow(joined_sf)

# Total requests in each polygon, all categories combined
polygon_totals <- joined_sf %>%
  st_drop_geometry() %>%
  count(polygon_id, name = "poly_total_n")

# Total requests for each category across all polygons
category_totals <- joined_sf %>%
  st_drop_geometry() %>%
  count(Customer_Service_Requested, name = "cat_total_n") %>%
  arrange(desc(cat_total_n))

# Count by polygon + category
polygon_category_counts <- joined_sf %>%
  st_drop_geometry() %>%
  count(polygon_id, Customer_Service_Requested, name = "cat_n") %>%
  left_join(category_totals, by = "Customer_Service_Requested") %>%
  mutate(
    pct_cat = cat_n / cat_total_n,
    pct_all = cat_n / total_requests_all
  )

# =========================================================
# 11. Make safe short field prefixes for categories
# =========================================================
make_safe_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "") %>%
    str_sub(1, 18)
}

category_lookup <- category_totals %>%
  mutate(cat_field = make_safe_name(Customer_Service_Requested))

make_unique_names <- function(x) {
  out <- character(length(x))
  seen <- list()

  for (i in seq_along(x)) {
    val <- x[i]
    if (is.null(seen[[val]])) {
      seen[[val]] <- 1
      out[i] <- val
    } else {
      seen[[val]] <- seen[[val]] + 1
      suffix <- paste0("_", seen[[val]])
      base_max <- 18 - nchar(suffix)
      out[i] <- paste0(substr(val, 1, base_max), suffix)
    }
  }
  out
}

category_lookup$cat_field <- make_unique_names(category_lookup$cat_field)

polygon_category_counts <- polygon_category_counts %>%
  left_join(
    category_lookup %>% select(Customer_Service_Requested, cat_field),
    by = "Customer_Service_Requested"
  )

# =========================================================
# 12. Build wide tables
#     *_n       = count in polygon
#     *_pctcat  = percent of category total
#     *_pctall  = percent of all requests
# =========================================================
wide_n <- polygon_category_counts %>%
  select(polygon_id, cat_field, cat_n) %>%
  pivot_wider(
    names_from = cat_field,
    values_from = cat_n,
    names_glue = "{cat_field}_n",
    values_fill = 0
  )

wide_pctcat <- polygon_category_counts %>%
  select(polygon_id, cat_field, pct_cat) %>%
  pivot_wider(
    names_from = cat_field,
    values_from = pct_cat,
    names_glue = "{cat_field}_pctcat",
    values_fill = 0
  )

wide_pctall <- polygon_category_counts %>%
  select(polygon_id, cat_field, pct_all) %>%
  pivot_wider(
    names_from = cat_field,
    values_from = pct_all,
    names_glue = "{cat_field}_pctall",
    values_fill = 0
  )

# =========================================================
# 13. Join wide attributes back to polygon layer
# =========================================================
polygon_output <- polygons_clean %>%
  left_join(polygon_totals, by = "polygon_id") %>%
  left_join(wide_n, by = "polygon_id") %>%
  left_join(wide_pctcat, by = "polygon_id") %>%
  left_join(wide_pctall, by = "polygon_id") %>%
  mutate(
    poly_total_n = coalesce(poly_total_n, 0L)
  )

# Replace NA in newly created numeric fields with 0
original_fields <- names(polygons_clean)
new_fields <- setdiff(names(polygon_output), original_fields)

for (fld in new_fields) {
  if (is.numeric(polygon_output[[fld]])) {
    polygon_output[[fld]][is.na(polygon_output[[fld]])] <- 0
  }
}

# =========================================================
# 14. Write one polygon feature class
# =========================================================
st_write(
  obj = polygon_output,
  dsn = output_gdb,
  layer = output_layer,
  delete_layer = TRUE,
  quiet = FALSE
)

# =========================================================
# 15. Export lookup and summary tables
# =========================================================
output_folder <- dirname(output_gdb)

lookup_csv <- file.path(output_folder, paste0(output_layer, "_category_lookup.csv"))
summary_csv <- file.path(output_folder, paste0(output_layer, "_category_totals.csv"))

write_csv(category_lookup, lookup_csv)
write_csv(category_totals, summary_csv)

cat("\nDone.\n")
cat("Output feature class:\n", file.path(output_gdb, output_layer), "\n")
cat("Category lookup CSV:\n", lookup_csv, "\n")
cat("Category totals CSV:\n", summary_csv, "\n")
