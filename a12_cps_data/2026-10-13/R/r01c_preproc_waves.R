# 2026-10-14
# Process Excel file with wave assignments

# load raw data from file
waves_raw <- openxlsx::read.xlsx(
  nested_here("data-raw", raw_files$waves$filename),
  sheet = raw_files$waves$sheet
)

# tidy variables
waves_raw |> 
  slice(-1) |> 
  set_names(waves_raw[1, ]) |>  
  janitor::clean_names() |> 
  mutate(
    wave              = na,
    short_name        = school_name,
    school_match_name = standardize_school_names(school_name),
    ) |> 
  distinct(school_id, school_match_name, wave) |>
  mutate(
    wave              = factor(
      coalesce(wave, "Fall"),
      levels = c("Pilot", "Feb", "April", "Fall"),
      labels = c("Pilot", "Feb 2024", "April 2024", "Fall 2024")
    )
  ) |> 
  write_if_diff(
    f_name  = nested_here("data", "cps_school_waves.csv"),
    comment = c(
      "Wave assignments for schools in pilot, Feb 2024, and April 2024",
      "groups. Values are matched to school ID and name.",
      "Any schools not on the list are assumed",
      "to be in the Fall 2024 wave.",
      "Based on file", raw_files$waves$filename,
      ", sheet", raw_files$waves$sheet, ". "
    )
  )





