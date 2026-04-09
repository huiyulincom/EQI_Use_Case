library(sf)
library(dplyr)
library(stringr)
library(readr)

# =========================
# 1. Input paths
# =========================
gdb_path <- "C:/your_folder/your_file.gdb"
layer_name <- "your_feature_class_name"

# Output folder/file
output_csv <- "C:/your_folder/customer_service_requested_summary.csv"

# =========================
# 2. Read feature class
# =========================
fc <- st_read(gdb_path, layer = layer_name, quiet = FALSE)

# =========================
# 3. Check field existence
# =========================
target_field <- "Customer_Service_Requested"

if (!(target_field %in% names(fc))) {
  stop(
    paste0(
      "Field '", target_field, "' was not found in the feature class.\n",
      "Available fields are:\n",
      paste(names(fc), collapse = ", ")
    )
  )
}

# =========================
# 4. Clean and summarize
# =========================
summary_table <- fc %>%
  st_drop_geometry() %>%
  mutate(
    Customer_Service_Requested = as.character(Customer_Service_Requested),
    Customer_Service_Requested = str_trim(Customer_Service_Requested),
    Customer_Service_Requested = na_if(Customer_Service_Requested, "")
  ) %>%
  mutate(
    Customer_Service_Requested = if_else(
      is.na(Customer_Service_Requested),
      "NA / Blank",
      Customer_Service_Requested
    )
  ) %>%
  count(Customer_Service_Requested, name = "count") %>%
  mutate(
    percent = count / sum(count),
    percent_label = paste0(round(percent * 100, 2), "%")
  ) %>%
  arrange(desc(count))

# =========================
# 5. Print results
# =========================
print(summary_table, n = Inf)

# =========================
# 6. Optional: show total records
# =========================
cat("\nTotal records:", sum(summary_table$count), "\n")

# =========================
# 7. Export to CSV
# =========================
write_csv(summary_table, output_csv)

cat("\nSummary table exported to:\n", output_csv, "\n")
