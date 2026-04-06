fetch_from_box <- function(args) {
    boxr::box_auth(cache = here::here(".boxr-oauth"))
    res <- boxr::box_read_excel(
        file_id = args$box_id,
        sheet   = args$sheet
    )
    res
}

fetch_and_preproc <- function(args) {
    raw <- fetch_from_box(args)

    if (args$clean_name == "waves") {
        res <- preproc_waves(raw)

    } else if (args$clean_name == "student_counts") {
        res <- preproc_student_counts(raw)

    } else if (args$clean_name == "schools") {
        res <- preproc_schools(raw)

    } else if (args$clean_name == "school_year_dates") {
        res <- preproc_school_year_dates(raw)

    } else if (args$clean_name == "inhaler_events") {
        res <- preproc_inhaler_events(raw)

    } else if (grepl("school_enrollment_[0-9]{2}$", args$clean_name)) {
        sy_end <- gsub("school_enrollment_([0-9]+)$", "\\1", args$clean_name)
        res    <- preproc_enrollment(raw, sy_end = as.numeric(sy_end))

    } else {
        res <- raw
    }

    attr(res, "metadata") <- rlist::list.append(
        args, "created_datetime" = Sys.time()
    )

    res
}

standardize_school_names <- function(vec) {
    v2 <- vec

    v2 <- gsub("0 Pilot", "", v2)
    v2 <- snakecase::to_snake_case(v2)
    v2 <- gsub("_", "", v2)

    case_when(
        v2 == "cant" ~ "canty",
        v2 == "kenwood" ~ "kenwoodhs",
        v2 == "lindbloom" ~ "lindblomhs",
        .default = v2
    )
}


preproc_inhaler_events <- function(raw) {
    res <- raw |>
        filter(!is.na(ID))

    for (x in names(res)) {
        attr(res[[x]], "label") <- x
    }

    res <- res[, which(colSums(!is.na(res)) > 0)] |>
        mutate(
            `row_id`               = `ID`,
            school_match_name = standardize_school_names(`At what school was the stock inhaler given?`),
            rcp_sex                = factor(`Recipient Sex`),
            `rcp_race`             = factor(`Recipient Race`),
            `rcp_eth`              = factor(`Recipient Ethnicity`),
            rcp_age                = `Recipient age (If recipient is a visitor over the age of 18, student teacher, or school personnel select "Adult")`,
            admin_date             = `On what date the stock inhaler given?`,
            admin_date             = coalesce(as.Date(admin_date), as.Date(Timestamp)),
            admin_loc            = fct_lump_lowfreq(`Location of Incident (Playground, gym, nurse's office, specific classroom number, etc.)`),
            `admin_protocol`       = `Under what protocol was the stock inhaler given?`,
            `nurse_present`        = `Was the school nurse present or notified?`,
            `exp_pollen`           = `Exposure to pollen allergen`,
            `exp_dust`             = `Exposure to dust allergen`,
            `exp_anim`             = `Exposure to animal dander`,
            `exp_smok`             = `Exposure to smoke in the environment`,
            `exp_air`              = `Exposure to air pollution in the environment`,
            `exp_heat`             = `Exposure to hot weather`,
            `exp_cold`             = `Exposure to cold weather`,
            `exp_phys`             = `Physical activity or exercise`,
            `exp_unk`              = `Unknown`,
            `exp_none`             = `No exposure`,
            `exp_other`            = `Other...29`,
            `sym_sob`              = `Shortness of breath`,
            `sym_wheeze`           = `Wheezing`,
            `sym_cough`            = `Coughing`,
            `sym_tight`            = `Chest tightness`,
            `sym_rapid`            = `Rapid breathing`,
            `sym_speak`            = `Difficulty speaking or completing sentences`,
            `sym_chpain`           = `Chest pain`,
            `sym_flare`            = `Flaring of the nostrils`,
            `sym_musc`             = `Retraction of neck and chest muscles`,
            `sym_blue`             = `Bluish or pale coloration of the lips or fingertips`,
            `sym_anx`              = `Anxiety or restlessness`,
            `sym_brdiff`           = `breathing difficulty`,
            `sym_none`             = `No symptoms`,
            `sym_other`            = `Other...46`,
            `pathway_true`         = `Correct Pathway`,
            `pathway_chosen`       = `Chosen right pathway?`,
            `inh_med`              = `What is the name of the inhaler/medication given? (HFA Albuterol, ProAir RespiClick, etc.)`,
            `spacer`               = `Was a spacer device used? (Either plastic or paper)`,
            `n_puffs_hfa`          = `How many puffs were administered using the HFA Inhaler?`,
            `pathway_chosen_compl` = `Compliant with nurse-selected pathway`,
            `pathway_true_compl`   = `compliant with correct pathway`,
            `mouthpiece`           = `Was a disposable mouthpiece used?`,
            `n_puffs_respi`        = `How many puffs were administered using the ProAir RespiClick?`,
            `rcp_role`             = `Type of person administered`,
            `rcp_dx_pre`           = `Previous Dagnosis`,
            `rcp_dx_post`          = `Has the student since received an asthma diagnosis?`,
            `hc_help`              = `Do they need assistance connecting with a healthcare provider or finding insurance coverage?`,
            `inh_use_prev`         = `Any Previous Usage`,
            `why_given`            = `Was inhaler administered to prevent symptoms or given due to breathing difficulties?`,
            `severity`             = `According to the protocol, was the individual exhibiting mild-moderate symptoms or severe symptoms?`,
            `sym_new`              = `As far as you are aware, is this the first time the individual has ever experienced these kind of symptoms at school?`,
            `hypo_disp`            = `In your professional opinion, what would have happened if the student/individual did not have access to stock inhaler medication?`,
            `hypo_disp_rcd`        = `Hypothetical_Disposition_simplified`,
            `true_disp`            = `What DID happen to the student/individual after the inhaler was given?`,
            `true_disp_rcd`        = `Actual_Disposition_simplified`,
            `hosp`                 = `Did the individual go to the hospital after emergency services came?`,
            `transport`            = `How did they get to the hospital?`,

            .before = 1, .keep = "unused"
        )

    # identify factors
    for (x in (res |> select(where(is.character)) |> names())) {
        if(length(unique(na.omit(res[[x]]))) < 10) {
            res[[x]] <- fct_infreq(as.character(res[[x]]))
        } else if(length(unique(na.omit(res[[x]]))) < 20) {
            res[[x]] <- fct_lump_lowfreq(as.character(res[[x]]))
        }
    }

    res
}

preproc_school_year_dates <- function(raw) {
    res <- raw |>
        rename(data_schoolyear = school_year) |>
        arrange(start)

    res |>
        add_case(
            data_schoolyear = glue::glue("(after {last(res$data_schoolyear)})"),
            start           = as.Date(max(res$end)) + 1,
            end             = mdy("01-01-3000"),
            .after          = nrow(res)
        ) |>
        add_case(
            data_schoolyear = glue::glue("(before {first(res$data_schoolyear)})"),
            start           = mdy("01-01-2000"),
            end             = as.Date(min(res$start)) - 1,
            .before         = 1
            ) |>
        unite(data_season,
              c(season, data_schoolyear),
              sep = " ", remove = FALSE, na.rm = TRUE) |>
        mutate(across(where(is.character), ~ fct_inorder(.x)))
    }

preproc_schools <- function(raw) {
    res <- raw |>
        janitor::clean_names() |>
        mutate(
            school_id = as.character(school_id),
            grade_level                  = fct_collapse(
                primary_category,
                "Elementary/Middle School" = c("ES", "MS"),
                "High School"              = c("HS")
                ),

            region                       = factor(snakecase::to_title_case(region_name)),
            region_3grp                  = fct_collapse(
                region,
                "North"        = c("North", "Northwest"),
                "West/Central" = c("West", "Central"),
                "South"        = c("Far South", "Southeast", "Southwest")
            ),
            region_2grp                  = fct_collapse(
                region,
                "North"        = c("North", "Northwest"),
                other_level = "Other"
                ),
            .keep = "none"
            )

    res
}

preproc_enrollment <- function(raw, sy_end) {
    sy_char  <- as.character(glue::glue("20{sy_end - 1}-20{sy_end}"))
    name_row <- which(snakecase::to_snake_case(raw[,1]) == "school_id")

    res <- raw[(name_row+1):nrow(raw),] |>
        set_names(raw[name_row,])

    res |>
        janitor::clean_names() |>
        distinct(school_id, school_name) |>
        arrange(sy_char) |>
        mutate(
            school_match_name = standardize_school_names(school_name),
            data_schoolyear   = fct_inorder(sy_char),
            .keep             = "unused"
            ) |>
        filter(!is.na(school_id))
    }

preproc_student_counts <- function(raw, cps_base_days = 176) {
    res <- raw |>
        janitor::clean_names() |>
        arrange(enrollment_year) |>
        mutate(
            school_match_name = standardize_school_names(student_current_school),

            # current year variables
            data_schoolyear        = fct_inorder(enrollment_year),
            data_date              = as.Date(
                gsub(".*(20[0-9]{2})$", "\\1-06-30", enrollment_year)
                ),

            # demographics
            asthma_status_lab = fct_recode(
                asthma_status,
                "Asthma Dx"    = "Yes",
                "No Asthma Dx" = "No"
            ),

            student_race     = fct_collapse(
                student_race,
                "Asian, Hawaiian, Pacific Islander" = c(
                    "ASIAN",
                    "HAWAIIAN OR PACIFIC ISLANDER",
                    "ASIAN OR PACIFIC ISLANDER"
                )
            ),

            across(
                all_of(c("student_race", "student_gender")),
                ~ fct_na_level_to_value(.x, extra_levels = c("N/A", "@ERR"))
                ),

            across(
                all_of(c("student_race", "student_gender")),
                ~ fct_relabel(.x, ~ snakecase::to_title_case(.x, parsing_option = 0))
                ),

            student_count    = asthma_count,
            asthma_count     = student_count*(asthma_status == "Yes")
            ) |>

        # collapse counts across equivalent rows
        summarise(
            n_rows_collapsed = n(),
            average_attendance = weighted.mean(average_attendance, student_count),
            across(matches("count"), ~ sum(.x, na.rm = TRUE)),
            .by = where(negate(is.numeric))
            ) |>

        mutate(
            across(
                matches("count"),
                list(
                    yes = ~ .x,
                    no  = ~ student_count - .x
                    )
                ),

            cps_base_days         = cps_base_days,
            student_days_expected = student_count*cps_base_days,
            student_days_present  = (student_days_expected)*(average_attendance/100),
            student_days_absent   = student_days_expected - student_days_present
            )
    res |>
        select(
            school_match_name,
            data_schoolyear,
            data_date,
            student_race,
            student_gender,
            asthma_status,
            asthma_status_lab,
            student_count,
            ends_with("yes"),
            ends_with("no"),
            average_attendance,
            cps_base_days,
            starts_with("student_days")
            ) |>
        select(-any_of(c("student_count_yes", "student_count_no")))
}


preproc_waves <- function(raw) {
    raw |>
        slice(2:n()) |> # drop extra header line
        janitor::clean_names() |>
        select(school_information, x3) |>
        set_names(c("school_id", "wave")) |>

        mutate(
            school_id = as.character(school_id),
            wave      = factor(
                coalesce(wave, "Fall"),
                levels = c("Pilot", "Feb", "April", "Fall"),
                labels = c("Pilot", "Feb 2024", "April 2024", "Fall 2024")
            )
        )
}

# function to add school ids, matching on school name and maybe year
add_school_ids <- function(df, id_map) {
    match_by <- intersect(
        c("school_match_name", "data_schoolyear"),
        names(df)
    )

    if(!("data_schoolyear" %in% match_by)) {
        id_map <- id_map |>
            distinct(school_id, school_match_name)
    }

    left_join(
        df,
        id_map,
        by = match_by
    )
}

# function to classify event dates into school year/season
classify_dates <- function(x, syd_df) {
    cut(
        as.Date(x),
        breaks = as.Date(c(first(syd_df$start), syd_df$end)),
        labels = syd_df$data_season,
        include.lowest = TRUE
    )
}


# summarize student counts by group at the school, schoolyear level
tabulate_fct <- function(x_fct, df) {
    df2 <- df

    df2[["group"]] <- fct_na_value_to_level(df2[[x_fct]], "Unknown")

    df2 |>
        summarise(
            count = sum(student_count, na.rm = TRUE),
            .by   = c(school_id, data_schoolyear, group)
        ) |>

        mutate(
            pct = (count / sum(count))*100,
            .by = school_id, data_schoolyear
            ) |>

        pivot_wider(
            names_from   = group,
            names_prefix = x_fct,
            values_from  = c(count, pct),
            values_fill  = 0,
            id_cols      = c(school_id, data_schoolyear)
        ) |>
        janitor::clean_names()
}

na_view <- function(df) {
    anti_join(
        df,
        na.omit(df),
        by = names(df)
    )
}
