# 2026-10-13
# Process Excel file with utilization reports

# load raw utilization data from file
util_raw <- readxl::read_excel( 
  nested_here("data-raw", raw_files$utilization$filename),
  sheet =  raw_files$utilization$sheet
)

# re-import date column
col_import_types            <- rep("skip", ncol(util_raw))
date_cols                   <- grep("((date))", tolower(names(util_raw)))
col_import_types[date_cols] <- "date"
util_raw[, date_cols]       <- readxl::read_excel(
  nested_here("data-raw", raw_files$utilization$filename),
  sheet      =  raw_files$utilization$sheet,
  col_types  = col_import_types
)

# isolate relevant variables
util_raw |> 
  janitor::clean_names() |> 
  select(
    at_what_school_was_the_stock_inhaler_given,
    on_what_date_the_stock_inhaler_given
  ) |> 
  mutate(
    school_match_name = standardize_school_names(
      at_what_school_was_the_stock_inhaler_given
    ),
    event_date        = on_what_date_the_stock_inhaler_given,
    .keep             = "none"
    ) |> 
  
  write_if_diff(
    f_name  = nested_here("data", "cps_utilization_events.csv"),
    comment = paste(
      "Reported incidents by school name and date.",
      "Based on file", params$raw_data_utilization$filename,
      ", sheet", params$raw_data_utilization$sheet, ". "
    )
  )





