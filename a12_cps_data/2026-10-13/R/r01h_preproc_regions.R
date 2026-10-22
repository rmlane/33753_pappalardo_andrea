# 2026-10-14
# Process Excel file with region data

# load raw data from file
regions_raw <- openxlsx::read.xlsx(
  nested_here("data-raw", raw_files$regions$filename),
  sheet = raw_files$regions$sheet
)

# tidy variables
regions_raw |> 
  janitor::clean_names() |> 
  mutate(
    school_match_name = standardize_school_names(short_name),
    region = snakecase::to_title_case(region_name)
    ) |> 
  distinct(school_id, school_match_name, region) |>
  write_if_diff(
    f_name  = nested_here("data", "cps_school_regions.csv"),
    comment = c(
      "Regions by school name and ID.",
      "Based on file", raw_files$regions$filename,
      ", sheet", raw_files$regions$sheet, ". "
    )
  )





