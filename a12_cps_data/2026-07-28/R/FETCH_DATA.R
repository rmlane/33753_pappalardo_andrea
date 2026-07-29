# load library and set box_auth
library(tidyverse)
boxr::box_auth(cache = here::here(".boxr-oauth"))

# load externally defined functions
source(here::here("a12_cps_data/2026-07-28/R/box_fx.R"))

###############################################################################
# capture starting defaults
base_defaults <- list(
  box_write_if_diff = default::default(box_write_if_diff)
  )

# set defaults for box functions
analytic_data_box_id                <- "403977851006"
default::default(box_write_if_diff) <- list(
  log_file = "data_log.csv",
  dir_id   = analytic_data_box_id
  )

chicago_data_api_endpoints <- list(
  sy_2122       = "2dem-8rq7",
  sy_2223       = "9a5f-2r4p",
  sy_2324       = "cu4u-b4d9",
  sy_2425       = "3dhs-m3w4",
  region_shapes = "spyv-p8fk"
  )

###############################################################################

# get schoolyear dates
sy_dates <- httr::GET(
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
  mutate(sy_day20 = sy_start + 19) |> 
  
  # (maybe) write to file
  box_write_if_diff(
    f_name  = "cps_schoolyear_dates.csv",
    comment = paste(
      "Start and end dates for CPS schoolyears from 2020-2021 to 2026-2027.",
      "Constructed on", Sys.Date(), 
      "using data from  https://api.cps.edu/calendar/CPS/EventsList"
      )
    )

###############################################################################

# fetch school characteristics from chicago data portal
school_chars <- chicago_data_api_endpoints |> 
  keep_at(~ grepl("^sy", .x)) |> 
  map_dfr(~{
    glue::glue("https://data.cityofchicago.org/api/v3/views/{.x}/query.json") |> 
      httr::GET() |> 
      pluck("content") |> 
      rawToChar() |> 
      jsonlite::fromJSON() |> 
      janitor::clean_names() |> 
      select(school_id, short_name, primary_category, matches("(lat)|(long)|(coord)"))
  }) |> 
  mutate(
    school_match_name = gsub("_", "", snakecase::to_screaming_snake_case(short_name)),
    grade_level       = fct_collapse(
      primary_category,
      "Elementary/Middle School" = c("ES", "MS"),
      "High School"              = c("HS")
    )
  ) |> 
  distinct()

# fetch city region boundaries from chicago data portal; merge to schools by location
school_chars <- glue::glue(
  "https://data.cityofchicago.org/api/v3/views/{x}/query.json",
  x = chicago_data_api_endpoints$region_shapes
  ) |> 
  httr2::request() |> 
  httr2::req_perform() |> 
  httr2::resp_body_json() |>
  enframe() |> 
  mutate(
    region_name = unlist(map(value, ~ pluck(.x, "region_nam"))),
    geom        = map(value, ~{
      .x |> 
        pluck("the_geom") |>
        pluck("coordinates") |> 
        unlist() |> 
        matrix(byrow = TRUE, ncol = 2) |> 
        as.data.frame() |> 
        sf::st_as_sf(coords = c("V1", "V2")) |> 
        sf::st_combine() |>  
        sf::st_cast("POLYGON")
    }),
    
    schools = map(geom, ~{
      sf::st_intersection(
        school_chars |> 
          sf::st_as_sf(coords = c("school_longitude", "school_latitude")),
        .x
      ) |> 
        sf::st_drop_geometry()
    })
  ) |> 
  select(schools, region_name) |> 
  unnest(schools) |> 
  
  # (maybe) write to file
  box_write_if_diff(
    f_name  = "cps_school_characteristics.csv",
    comment = paste(
      "School id, name, grade level, and region for all CPS schools between 2021-2022",
      " and 2024-2025. School profiles were downloaded from the Chicago ",
      "Data Portal via API. Chicago planning region shapefiles were downloaded ",
      "from the Chicago Data Portal via API and matched to schools by latitute ",
      "and longitude. Data were fetched and merged on", Sys.Date(), "."
    )
  )

# reset function defaults
default::default(box_write_if_diff)  <- base_defaults$box_write_if_diff
