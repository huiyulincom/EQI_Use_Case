library(sf)
library(dplyr)
library(stringr)
library(readr)

# =========================================================
# 1. User inputs
# =========================================================
gdb_path_points   <- "C:/your_folder/first.gdb"
layer_points      <- "your_point_layer_name"      # contains Customer_Service_Requested

gdb_path_polygons <- "C:/your_folder/second.gdb"
layer_polygons    <- "your_polygon_layer_name"    # contains SCREEN

output_overall_csv   <- "C:/your_folder/customer_service_requested_summary.csv"
output_overlay_csv   <- "C:/your_folder/customer_service_requested_by_screen.csv"

field_request <- "Customer_Service_Requested"
field_screen  <- "SCREEN"

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
# 4. Make sure CRS match
# =========================================================
if (st_crs(points_sf) != st_crs(polygons_sf)) {
  polygons_sf <- st_transform(polygons_sf, st_crs(points_sf))
}

# =========================================================
# 5. Keep only needed fields and clean text
# =========================================================
points_clean <- points_sf %>%
  select(all_of(field_request)) %>%
  mutate(
    Customer_Service_Requested = as.character(Customer_Service_Requested),
    Customer_Service_Requested = str_squish(Customer_Service_Requested),
    Customer_Service_Requested = na_if(Customer_Service_Requested, ""),
    Customer_Service_Requested = if_else(
      is.na(Customer_Service_Requested),
      "NA / Blank",
      Customer_Service_Requested
    )
  )

polygons_clean <- polygons_sf %>%
  select(all_of(field_screen)) %>%
  mutate(
    SCREEN = as.character(SCREEN),
    SCREEN = str_squish(SCREEN),
    SCREEN = na_if(SCREEN, ""),
    SCREEN = if_else(
      is.na(SCREEN),
      "NA / Blank",
      SCREEN
    )
  )

# =========================================================
# 6. Step 1: overall summary of Customer_Service_Requested
# =========================================================
summary_overall <- points_clean %>%
  st_drop_geometry() %>%
  count(Customer_Service_Requested, name = "case_count") %>%
  mutate(
    total_cases = sum(case_count),
    percent_of_all_cases = case_count / total_cases,
    percent_label = paste0(round(percent_of_all_cases * 100, 2), "%")
  ) %>%
  arrange(desc(case_count))

print(summary_overall, n = Inf)

write_csv(summary_overall, output_overall_csv)

# =========================================================
# 7. Step 2: spatial join (point in polygon)
#    left = TRUE keeps all points, including those not falling
#    into any polygon
# =========================================================
joined_sf <- st_join(points_clean, polygons_clean, left = TRUE)

# Replace missing SCREEN after join
joined_sf <- joined_sf %>%
  mutate(
    SCREEN = if_else(is.na(SCREEN), "Outside polygon / No match", SCREEN)
  )

# =========================================================
# 8. Step 3: summarize by Customer_Service_Requested and SCREEN
# =========================================================
summary_by_screen <- joined_sf %>%
  st_drop_geometry() %>%
  count(Customer_Service_Requested, SCREEN, name = "case_count") %>%
  group_by(Customer_Service_Requested) %>%
  mutate(
    total_cases_in_request_category = sum(case_count),
    percent_within_request_category = case_count / total_cases_in_request_category,
    percent_label = paste0(round(percent_within_request_category * 100, 2), "%")
  ) %>%
  ungroup() %>%
  arrange(Customer_Service_Requested, desc(case_count))

print(summary_by_screen, n = Inf)

write_csv(summary_by_screen, output_overlay_csv)

# =========================================================
# 9. Optional check:
#    For each request category, percentages should sum to ~100%
# =========================================================
check_percent <- summary_by_screen %>%
  group_by(Customer_Service_Requested) %>%
  summarise(
    percent_sum = sum(percent_within_request_category),
    .groups = "drop"
  )

print(check_percent, n = Inf)

cat("\nDone.\n")
cat("Overall summary saved to:\n", output_overall_csv, "\n")
cat("Overlay summary saved to:\n", output_overlay_csv, "\n")
