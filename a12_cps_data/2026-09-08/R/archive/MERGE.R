





## Merge

# make df with all combinations of school ids and years
merge(
    read_rds(nested_here("data", "pre_school_ids.rds")) |>
        distinct(school_id),

    read_rds(nested_here("data", "pre_school_year_dates.rds")) |>
        distinct(data_schoolyear) |>
        filter(!grepl("[a-z]", data_schoolyear)),

    by = NULL
    ) |>

    write_rds(nested_here("data", "mrg_school_ids_schoolyears.rds"))

# (maybe) add school and date ids to inhaler events and write to file

# if( !file.exists(nested_here("data", "mrg_inhaler_events.rds")) |
# params$force_preproc ) {

add_school_ids(
    read_rds(nested_here("data", "pre_inhaler_events.rds")),
    id_map = read_rds(nested_here("data", "pre_school_ids.rds"))
) |>
    mutate(
        data_season = classify_dates(
            admin_date,
            syd_df = read_rds(nested_here("data", "pre_school_year_dates.rds"))
        )
    ) |>
    left_join(
        read_rds(nested_here("data", "pre_school_year_dates.rds")) |>
            select(where(is.factor)),
        by = c("data_season")
    ) |>
    structure(unit = "event", timescale = "date") |>
    droplevels() |>
    write_rds(nested_here("data", "mrg_inhaler_events.rds"))
# }


# (maybe) merge school and wave data and write to file

# if( !file.exists(nested_here("data", "mrg_school_chars.rds")) |
# params$force_preproc ) {

full_join(
    read_rds(nested_here("data", "pre_schools.rds")),
    read_rds(nested_here("data", "pre_waves.rds")),
    by = "school_id"
) |>

    mutate(
        wave = fct_na_value_to_level(wave, "Fall 2024"),
        impl_date      = as.Date(
            fct_recode(
                wave,
                "2023-09-01" = "Pilot",
                "2024-02-01" = "Feb 2024",
                "2024-04-01" = "April 2024",
                "2024-09-01" = "Fall 2024"
            )
        ),
        impl_schoolyear = fct_collapse(
            wave,
            "2023-2024" = c("Pilot", "Feb 2024", "April 2024"),
            "2024-2025" = c("Fall 2024")
        )
    ) |>
    structure(unit = "school", timescale = "invariant") |>
    write_rds(nested_here("data", "mrg_school_chars.rds"))
# }


# (maybe) add school ids to student counts and write to file

# if( !file.exists(nested_here("data", "mrg_student_counts.rds")) |
# params$force_preproc ) {

add_school_ids(
    read_rds(nested_here("data", "pre_student_counts.rds")),
    id_map = read_rds(nested_here("data", "pre_school_ids.rds"))
) |>
    structure(unit = "student group", timescale = "schoolyear") |>
    write_rds(nested_here("data", "mrg_student_counts.rds"))
# }

## Tabulate/Summarize

# Tabulate events by school and schoolyear

full_join(

    # all school_ids and data_schoolyears
    read_rds(nested_here("data", "mrg_school_ids_schoolyears.rds")),

    # inhaler events by school_ids and data_schoolyear
    read_rds(nested_here("data", "mrg_inhaler_events.rds")),
    by      = c("school_id", "data_schoolyear")
) |>

    # count events per year and school
    summarise(
        n_events = sum(!is.na(row_id)),
        .by      = c(school_id, data_schoolyear)
    ) |>

    # calculate cumulative counts
    arrange(school_id, data_schoolyear) |>
    mutate(
        n_events_cumulative = cumsum(n_events),

        any_events = factor(
            n_events >= 1,
            levels = c(FALSE, TRUE),
            labels = c("No Events", "1+ Events")
        ),
        any_events_cumulative = factor(
            n_events_cumulative >= 1,
            levels = c(FALSE, TRUE),
            labels = c("No Events", "1+ Events")
        ),
        .by                 = school_id
    ) |>

    structure(unit = "school", timescale = "schoolyear") |>
    write_rds(nested_here("data", "mrg_school_lvl_event_counts.rds"))




# summarize all student counts at the school and school-year level

list(
    tabulate_fct(
        x_fct  = "student_race",
        df     = read_rds(nested_here("data", "mrg_student_counts.rds"))
    ),

    tabulate_fct(
        x_fct  = "student_gender",
        df     = read_rds(nested_here("data", "mrg_student_counts.rds"))
    ),

    read_rds(nested_here("data", "mrg_student_counts.rds")) |>
        summarise(
            average_attendance = weighted.mean(average_attendance, student_count),
            across(c(
                "student_count",
                matches("_yes$"),
                matches("_no$"),
                "student_days_present",
                "student_days_absent"
            ), ~ sum(.x, na.rm = TRUE)),
            .by = c(school_id, data_schoolyear)
        ) |>
        mutate(across(
            ends_with("yes"),
            list(pct = ~ (.x / student_count)*100),
            .names = "pct_{gsub('_count', '',  .col)}"
        ),
        .by = c(school_id, data_schoolyear)
        )
) |>

    reduce(full_join, by = c("school_id", "data_schoolyear")) |>
    structure(unit = "school", timescale = "schoolyear") |>
    write_rds(nested_here("data", "mrg_school_lvl_student_counts.rds"))



# merge all school-level annual data
full_join(
    read_rds(nested_here("data", "mrg_school_lvl_student_counts.rds")),
    read_rds(nested_here("data", "mrg_school_lvl_event_counts.rds")),
    by = c("school_id", "data_schoolyear")
) |>
    filter(
        sum(n_events) > 0 | sum(!is.na(student_count)) > 0,
        .by = c("school_id", "data_schoolyear")
    ) |>
    droplevels() |>

    # add school chars
    left_join(
        read_rds(nested_here("data", "mrg_school_chars.rds")),
        by = "school_id"
    ) |>

    # create 3-level version of implementation variable
    mutate(across(
        all_of(c("any_events", "any_events_cumulative")),
        ~ fct_relevel(case_when(
            (data_schoolyear %in% c("2021-2022", "2022-2023", "2023-2024") &
                 impl_schoolyear == "2024-2025") ~ "N/A (Impl Group 2)",
            .default = .x
        ), c("No Events", "1+ Events")),
        .names = "{.col}_l3"
    ), .after = "any_events_cumulative") |>

    relocate(names(read_rds(nested_here("data", "mrg_school_chars.rds")))) |>
    mutate(n_schools = "total") |>  # utility variable for tables |>
    # labels
    mutate(
        data_schoolyear = structure(
            data_schoolyear,
            label = "Data describes schools in schoolyear...")
    ) |>
    arrange(data_schoolyear, school_id) |>
    structure(unit = "school", timescale = "schoolyear") |>
    write_rds(nested_here("data", "analytic_school_lvl.rds"))






# merge annual student counts with relevant school variables
if(!file.exists(nested_here("data", "analytic_student_lvl.rds"))) {
    right_join(
        read_rds(nested_here("data", "analytic_school_lvl.rds")) |> 
            select(where(negate(is.numeric)), matches("events")) |> 
            distinct_by(c("school_id", "data_schoolyear")),
        read_rds(nested_here("data", "mrg_student_counts.rds")),
        by = c("school_id", "data_schoolyear")
    ) |>
        relocate(where(negate(is.numeric)), matches("events")) |> 
        mutate(
            active_days   = difftime(data_date, impl_date, units = "days"),
            active_months = as.numeric(active_days) / 30.4,
            .after        = "data_date"
        ) |> 
        write_rds(nested_here("data", "analytic_student_lvl.rds")) 
}



