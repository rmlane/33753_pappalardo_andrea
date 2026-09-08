# 2026-07-29

# Process Excel file with utilization counts

# load start and end dates fo each school year
sy_dates <- box_file_attr("cps_schoolyear_dates.csv") |> 
  pluck("id") |> 
  boxr::box_read_csv() 

# load raw utilization data from file
df_raw <- boxr::box_read_excel(
  file_id   = params$raw_data_utilization$box_id,
  sheet     = params$raw_data_utilization$sheet
  )

# count events by school and schoolyear
df_raw |> 
  janitor::clean_names() |> 
  filter(!is.na(at_what_school_was_the_stock_inhaler_given)) |> 
  mutate(
    school_match_name = standardize_school_names(
      at_what_school_was_the_stock_inhaler_given
      ),
    event_date = as.Date(on_what_date_the_stock_inhaler_given),
    .keep = "none"
    ) |> 
  
  full_join(
    sy_dates |>
      mutate(event_date = as.Date(sy_start)) |> 
      select(event_date, data_schoolyear),
    by = c("event_date")
  ) |> 
  arrange(event_date) |> 
  fill(data_schoolyear, .direction = "down") |> 
  filter(!is.na(school_match_name)) |> 
  
  count(school_match_name, data_schoolyear) |> 
  
  box_write_if_diff(
    f_name  = "cps_school_utilization_counts.csv",
    comment = paste(
      "Counts of reported incidents by school name and schoolyear.",
      "Any combinations not on the list are assumed",
      "to equal 0.",
      "Based on file", params$raw_data_utilization$filename,
      ", sheet", params$raw_data_utilization$sheet, ". "
    )
  )





