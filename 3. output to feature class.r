library(sf)
library(dplyr)
library(stringr)
library(readr)
library(purrr)

# =========================================================
# 1. User inputs
# =========================================================
gdb_path_points   <- "C:/your_folder/first.gdb"
layer_points      <- "your_point_layer_name"      # contains Customer_Service_Requested

gdb_path_polygons <- "C:/your_folder/second.gdb"
layer_polygons    <- "your_polygon_layer_name"    # contains SCREEN

# output geodatabase for polygon feature classes
output_gdb <- "C:/your_folder/output_maps.gdb"

field_request <- "Customer_Service_Requested"
field_screen  <- "SCREEN"

# Optional: polygon ID field
# If your polygon layer already has a stable unique ID field, put it here.
# If not, the code will create one automatically.
polygon_id_field <- NULL
# Example:
# polygon_id_field <- "OBJECTID"

# =========================================================
# 2. Create output geodatabase if it does not exist
# =========================================================
if (!dir.exists(output_gdb)) {
  sf::gdal_utils(
    util = "vectortranslate",
    source = character(0),
    destination = output_gdb,
    options = c("-f", "FileGDB")
  )
}

# =========================================================
# 3. Read data
# =========================================================
points_sf   <- st_read(gdb_path_points,   layer = layer_points,   quiet = FALSE)
polygons_sf <- st_read(gdb_path_polygons, layer = layer_polygons, quiet = FALSE)

# =========================================================
# 4. Check required fields
# =========================================================
if (!(field_request %in% names(points_sf))) {
  stop(
    paste0(
      "Field '", field_request, "' was not found in the point layer.\n",
      "Available fields are:\n",
      paste(names(points_sf), collapse = ", ")
    )
  )
}

if (!(field_screen %in% names(polygons_sf))) {
  stop(
    paste0(
      "Field '", field_screen, "' was not found in the polygon layer.\n",
      "Available fields are:\n",
      paste(names(polygons_sf), collapse = ", ")
    )
  )
}

# =========================================================
# 5. Match CRS
# =========================================================
if (st_crs(points_sf) != st_crs(polygons_sf)) {
  polygons_sf <- st_transform(polygons_sf, st_crs(points_sf))
}

# =========================================================
# 6. Prepare polygon ID
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
# 7. Clean point and polygon attributes
# =========================================================
points_clean <- points_sf %>%
  select(all_of(field_request), geometry) %>%
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
  select(polygon_id, all_of(field_screen), geometry) %>%
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
# 8. Spatial join: assign each point to a polygon
#    left = FALSE keeps only matched points inside polygons
# =========================================================
joined_sf <- st_join(points_clean, polygons_clean, left = FALSE)

# =========================================================
# 9. Overall point counts by polygon and by category
# =========================================================

# total points in each polygon, regardless of category
polygon_totals <- joined_sf %>%
  st_drop_geometry() %>%
  count(polygon_id, name = "polygon_total_cases")

# counts by category and polygon
category_polygon_counts <- joined_sf %>%
  st_drop_geometry() %>%
  count(Customer_Service_Requested, polygon_id, name = "category_case_count")

# total counts for each category across all polygons
category_totals <- joined_sf %>%
  st_drop_geometry() %>%
  count(Customer_Service_Requested, name = "category_total_cases")

# =========================================================
# 10. Combine summary table
# =========================================================
category_polygon_summary <- category_polygon_counts %>%
  left_join(category_totals, by = "Customer_Service_Requested") %>%
  left_join(polygon_totals, by = "polygon_id") %>%
  mutate(
    pct_of_category = category_case_count / category_total_cases,
    pct_within_polygon = category_case_count / polygon_total_cases
  )

# =========================================================
# 11. Get full list of categories
# =========================================================
all_categories <- category_totals %>%
  pull(Customer_Service_Requested) %>%
  unique() %>%
  sort()

# =========================================================
# 12. Helper function to make safe feature class names
# =========================================================
make_safe_layer_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "") %>%
    str_sub(1, 50)
}

# =========================================================
# 13. Create one polygon feature class per category
# =========================================================
for (cat_name in all_categories) {

  cat_summary <- category_polygon_summary %>%
    filter(Customer_Service_Requested == cat_name) %>%
    select(
      polygon_id,
      category_case_count,
      category_total_cases,
      polygon_total_cases,
      pct_of_category,
      pct_within_polygon
    )

  # join category summary back to full polygon layer
  cat_fc <- polygons_clean %>%
    left_join(cat_summary, by = "polygon_id") %>%
    mutate(
      category_name = cat_name,
      category_case_count = coalesce(category_case_count, 0L),
      category_total_cases = coalesce(category_total_cases, 0L),
      polygon_total_cases = coalesce(polygon_total_cases, 0L),
      pct_of_category = coalesce(pct_of_category, 0),
      pct_within_polygon = coalesce(pct_within_polygon, 0)
    )

  # optional percent labels for easy viewing in attribute table
  cat_fc <- cat_fc %>%
    mutate(
      pct_cat_lbl = paste0(round(pct_of_category * 100, 2), "%"),
      pct_poly_lbl = paste0(round(pct_within_polygon * 100, 2), "%")
    )

  # safe layer name
  layer_out <- paste0("csr_", make_safe_layer_name(cat_name))

  # write feature class
  st_write(
    obj = cat_fc,
    dsn = output_gdb,
    layer = layer_out,
    delete_layer = TRUE,
    quiet = TRUE
  )

  cat("Wrote feature class:", layer_out, "\n")
}

# =========================================================
# 14. Optional: export one master CSV summary table
# =========================================================
master_csv <- sub("\\.gdb$", "_master_summary.csv", output_gdb)

write_csv(category_polygon_summary, master_csv)

cat("\nDone.\n")
cat("Output geodatabase:\n", output_gdb, "\n")
cat("Master summary CSV:\n", master_csv, "\n")
