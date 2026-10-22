# Box files: if any don't exist in local environment, download them
raw_box_files |> 
  keep(~ !file.exists(
    here::here(
      params$parent_dir,
      params$version,
      "data-raw",
      .x$filename
    )
  )) |>
  
  walk(~{
    if(as.character(.x$box_version) == "1") {v <- NULL}
    
    boxr::box_dl(
      file_id    = .x$box_id,
      local_dir  = here::here(params$parent_dir, params$version, "data-raw"),
      file_name  = .x$filename,
      version_no = v
    )
    
    write_to_log(
      "File downloaded from Box. boxr::box_dl arguments:",
      glue::glue("- file_id = {.x$box_id}"),
      glue::glue("- file_name = {.x$filename}"),
      glue::glue("- version_no = {.x$box_version}"),
      
      log_file = nested_here("data-raw", "log.txt")
    )
  })


# API for schoolyear dates
if( !file.exists(nested_here("data-raw", "cps_calendar.csv"))) {
  httr::GET(
    url            = "https://api.cps.edu/calendar/CPS/EventsList",
    query          = list(
      eventStartDate = "08/15/2020",
      eventEndDate   = "08/15/2030",
      # isKeyEvent     = "Y",
      tagID          = "17,95",
      calendarID     = "1106"
      )
    ) |> 
    pluck("content") |> 
    rawToChar() |> 
    jsonlite::fromJSON() |>
    write_csv(
      nested_here("data-raw", "cps_calendar.csv"),
      na = ""
      )
  
  write_to_log(
    "CPS school dates downloaded from https://api.cps.edu/calendar/CPS/EventsList.",
    'Query params: eventStartDate = "08/15/2020", eventEndDate   = "08/15/2030",
    tagID = "17,95", calendarID = "1106"',
    "Data written to cps_calendar.csv.",
    log_file = nested_here("data-raw", "log.txt")
  )
}

# fetch school characteristics from chicago data portal
chi_data_api |> 
  keep(~ !file.exists(nested_here("data-raw", .x$filename))) |>
  walk(~{
    
    glue::glue("https://data.cityofchicago.org/api/v3/views/{.x$endpoint}/query.json") |> 
      httr::GET() |> 
      pluck("content") |> 
      rawToChar() |> 
      jsonlite::fromJSON() |> 
      select(where(negate(is.data.frame))) |> 
      write_csv(
        nested_here("data-raw", .x$filename),
        na = ""
      )
    
    write_to_log(
      glue::glue(
        "CPS school profiles downloaded from https://data.cityofchicago.org/api/v3/views/{.x$endpoint}/query.json"
      ),
      glue::glue("Data written to {.x$filename}"),
      log_file = nested_here("data-raw", "log.txt")
    )
  })


