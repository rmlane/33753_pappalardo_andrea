# 2026-07-29
# Process Excel file with wave assignments

# load raw data from file
df_raw <- boxr::box_read_excel(
  file_id   = params$raw_data_waves$box_id,
  sheet     = params$raw_data_waves$sheet
  )

# tidy variables
df_raw |> 
  slice(-1) |> 
  set_names(df_raw[1, ]) |>  
  janitor::clean_names() |> 
  mutate(
    wave              = na,
    short_name        = school_name,
    school_match_name = standardize_school_names(school_name),
    ) |> 
  distinct(school_id, short_name, school_match_name, wave) |>
  mutate(
    wave              = factor(
      coalesce(wave, "Fall"),
      levels = c("Pilot", "Feb", "April", "Fall"),
      labels = c("Pilot", "Feb 2024", "April 2024", "Fall 2024")
    )
  ) |> 
  box_write_if_diff(
    f_name  = "cps_school_waves.csv",
    comment = paste(
      "Wave assignments for schools in pilot, Feb 2024, and April 2024",
      "groups. Values are matched to school ID and name.",
      "Any schools not on the list are assumed",
      "to be in the Fall 2024 wave.",
      "Based on file", params$raw_data_waves$filename,
      ", sheet", params$raw_data_waves$sheet, ". "
    )
  )





