
read_csv(
  nested_here("data-raw", raw_files$sy_dates)
  ) |> 
  arrange(eventStartDate) |> 
  mutate(
    event_date  = anytime::anydate(eventStartDate),
    event_type  = fct_relevel(fct_collapse(
      snakecase::to_snake_case(eventTitle),
      "sy_start" = c(
        "first_day_of_school",
        "first_day_of_school_for_k_12_students"
      ),
      "sy_end"   = c(
        "end_of_quarter_4", 
        "last_day_of_school",
        "last_day_of_school_end_of_quarter_4",
        "last_day_of_school_for_k_12_students",
        "last_day_of_school_for_students"
      )
    ), "sy_start"),
    data_schoolyear = fct_inorder(case_when(
      event_type == "sy_start" ~ glue::glue("{x}-{x+1}", x = year(event_date)),
      event_type == "sy_end"   ~ glue::glue("{x-1}-{x}", x = year(event_date))
    )),
    .before = 1
  ) |> 
  filter(event_type %in% c("sy_start", "sy_end")) |> 
  distinct(event_type, event_date, data_schoolyear) |> 
  pivot_wider(
    names_from  = event_type,
    values_from = event_date,
    id_cols     = data_schoolyear
  ) |> 
  relocate(data_schoolyear, sy_start, sy_end) |> 
  mutate(
    next_sy_start = c(sy_start[-1], NA),
    sy_day20      = sy_start + 19
    ) |> 

  # (maybe) write to file
  write_if_diff(
    f_name  = nested_here("data", "cps_schoolyear_dates.csv"),
    comment = c(
      "Start and end dates for CPS schoolyears from 2020-2021 to 2026-2027.",
      "Constructed on", Sys.Date(), 
      "using data from  https://api.cps.edu/calendar/CPS/EventsList"
    )
  )
