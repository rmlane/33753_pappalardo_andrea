# code to pre-process data and create analytic datasets
# last updated: 2026-02-18

# Load Data ---------------------------------------------------------------

# read data from box
if(!exists("cps_raw")) {
boxr::box_auth(cache = here::here(".boxr-oauth"))
    cps_raw <- params |>
        keep(~ "file" %in% names(.x) & "box_id" %in% names(.x)) |>
        keep(~ grepl("xlsx", .x$file)) |>
        map(quietly(~{
            boxr::box_read_excel(
                file_id = .x[["box_id"]],
                sheet   = .x[["sheet"]]
            )
        }))
}

cps_raw_df <- transpose(cps_raw) |>
    pluck("result")
names(cps_raw_df) <- gsub("raw_data_", "", names(cps_raw_df))


# make xwalk of school name/id matches using CPS files
find_raw_data("all", pattern = "xlsx") |>
    map_dfr(~{
        openxlsx::read.xlsx(.x, sheet = "Schools") |>
            select(1:2) |>
            set_names(c("school_id", "short_name"))
    }) |>
    rbind(
        readxl::read_xls(
            find_raw_data("demographics_racialethnic_2022_v10272021.xls"),
            sheet = "Schools"
            ) |>
            select(2:3) |>
            set_names(c("school_id", "short_name"))
    ) |>
    mutate(
        school_match_name = gsub("_", "", snakecase::to_snake_case(short_name))
    ) |>
    distinct(school_id, school_match_name) |>
    slice(3:n()) |>
    write_rds(find_analytic_data(params$analytic_files$school_ids))

# limit cps data (annual counts) to relevant vars; add school ids
annual_student_counts <- cps_raw_df$cps |>
    janitor::clean_names() |>
    mutate(
        # pilot             = case_when(grepl("0 Pilot", student_current_school) ~ "Pilot"),
        school_match_name = gsub("_", "", snakecase::to_snake_case(
            gsub("0 Pilot", "", student_current_school)
        )),
        .before = 1
    ) |>
    left_join(
        read_analytic(params$analytic_files$school_ids),
        by = "school_match_name"
        ) |>
    relocate(school_id) |>
    select(-c(school_match_name, district_name, student_current_school))

# limit school chars to relevant vars
school_chars <- cps_raw_df$schools_metadata |>
    janitor::clean_names() |>
    distinct(school_id, primary_category, region_name) |>
    mutate(school_id = as.character(school_id))

# load wave data
impl_waves <- cps_raw_df$waves |>
    slice(2:n()) |>
    select(1, 3) |>
    set_names(c("school_id", "wave"))


# limit utilization data to relevant vars; add school ids
util_details <- cps_raw_df$utilization |>
    filter(!is.na(ID)) |>
    mutate(
        school_match_name = gsub(
            "_", "",
            snakecase::to_snake_case(`At what school was the stock inhaler given?`)
        ),
        school_match_name = case_when(
            school_match_name == "cant" ~ "canty",
            school_match_name == "kenwood" ~ "kenwoodhs",
            school_match_name == "lindbloom" ~ "lindblomhs",
            .default = school_match_name
        ),
        .before = 1
    ) |>

    left_join(
        read_analytic(params$analytic_files$school_ids),
        by = "school_match_name"
    ) |>
    relocate(school_id) |>
    select(-school_match_name)

# student counts: further recodes
fct_tidy <- function(x, na_values = NULL) {

    if(!is.null(na_values)) {x[which(x %in% na_values)] <- NA}
    fct_relabel(
        x,
        ~ gsub(",", " ", snakecase::to_title_case(.x, parsing_option = 0))
    )
}

annual_student_counts |>
    # select(-pilot) |>

    # time variables
    mutate(
        # current year variables
        data_schoolyear        = enrollment_year,
        data_date              = as.Date(
            gsub(".*(20[0-9]{2})$", "\\1-06-30", enrollment_year)
        ),
        .keep = "unused"
    ) |>

    # counts
    mutate(
        student_count    = asthma_count,
        asthma_count     = student_count*(asthma_status == "Yes")
    ) |>
    mutate(
        student_count,
        across(
            matches("count"),
            list(
                yes = ~ .x,
                no  = ~ student_count - .x
            )
        ),
        .keep = "unused"
    ) |>
    select(-any_of(c("student_count_yes", "student_count_no"))) |>

    # demographics
    mutate(
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
            ~ fct_tidy(.x, na_values = c("N/A", "@ERR"))
        ),
        .after = asthma_status
    ) |>
    relocate(school_id, where(negate(is.numeric)), data_schoolyear) |>

    summarise(
        n_rows_collapsed = n(),
        average_attendance = weighted.mean(average_attendance, student_count),
        across(matches("count"), ~ sum(.x, na.rm = TRUE)),
        .by = c(
            school_id, where(negate(is.numeric)), data_schoolyear
        )
    ) |>

    mutate(
        cps_base_days         = 176,
        student_days_expected = student_count*cps_base_days,
        student_days_present  = (student_days_expected)*(average_attendance/100),
        student_days_absent   = student_days_expected - student_days_present,
        .after                = average_attendance
    ) |>
    write_rds(find_analytic_data(params$analytic_files$student_counts))


# school chars: further recodes
full_join(
    school_chars,
    impl_waves,
    by = "school_id"
    ) |>
    mutate(
        wave      = factor(
            coalesce(wave, "Fall"),
            levels = c("Pilot", "Feb", "April", "Fall"),
            labels = c("Pilot", "Feb 2024", "April 2024", "Fall 2024")
        ),
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
        ),

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
        .keep     = "unused"
        ) |>
        mutate(across(where(is.character), factor)) |>
        group_by(school_id) |>
        fill(everything(), .direction = "downup") |>
        ungroup() |>
        write_rds(find_analytic_data(params$analytic_files$school_chars))


# make crosswalk of school term start/end dates
yaml::read_yaml(find_raw_data("school_year_dates.yaml")) |>
        map(map_dfr, data.frame, .id = "season") |>
        map_dfr(data.frame, .id = "school_year") |>
        arrange(start) |>
        mutate(
            season      = fct_inorder(season),
            school_year = fct_recode(
                school_year,
                "2023-2024" = "sy_23_24",
                "2024-2025" = "sy_24_25"
            ),
            sy_season = fct_inorder(paste(season, school_year))
        ) |>
    write_rds(find_analytic_data(params$analytic_files$school_year_dates))

for (x in names(util_details)) {
    attr(util_details[[x]], "label") <- x
    }

util_details |>
    rename(
        "row_id" = "ID",
        "rcp_sex" = "Recipient Sex",  ,
        "rcp_race" = "Recipient Race",
        "rcp_eth" = "Recipient Ethnicity",
        "rcp_age" = `Recipient age (If recipient is a visitor over the age of 18, student teacher, or school personnel select "Adult")`,
        "admin_date" = "On what date the stock inhaler given?",
        "admin_loc" = "Location of Incident (Playground, gym, nurse's office, specific classroom number, etc.)",
        "admin_protocol" = "Under what protocol was the stock inhaler given?",
        "nurse_present" = "Was the school nurse present or notified?",
        "exp_pollen" = "Exposure to pollen allergen",
        "exp_dust" = "Exposure to dust allergen",
        "exp_anim" = "Exposure to animal dander",
        "exp_smok" = "Exposure to smoke in the environment",
        "exp_air" = "Exposure to air pollution in the environment",
        "exp_heat" = "Exposure to hot weather",
        "exp_cold" = "Exposure to cold weather",
        "exp_phys" = "Physical activity or exercise",
        "exp_unk" = "Unknown",
        "exp_none" = "No exposure",
        "exp_other" = "Other...29",
        "sym_sob" = "Shortness of breath",
        "sym_wheeze" = "Wheezing",
        "sym_cough" = "Coughing",
        "sym_tight" = "Chest tightness",
        "sym_rapid" = "Rapid breathing",
        "sym_speak" = "Difficulty speaking or completing sentences",
        "sym_chpain" = "Chest pain",
        "sym_flare" = "Flaring of the nostrils",
        "sym_musc" = "Retraction of neck and chest muscles",
        "sym_blue" = "Bluish or pale coloration of the lips or fingertips",
        "sym_anx" = "Anxiety or restlessness",
        "sym_brdiff" = "breathing difficulty",
        "sym_none" = "No symptoms",
        "sym_other" = "Other...46",
        "pathway_true" = "Correct Pathway",
        "pathway_chosen" = "Chosen right pathway?",
        "inh_med" = "What is the name of the inhaler/medication given? (HFA Albuterol, ProAir RespiClick, etc.)",
        "spacer" = "Was a spacer device used? (Either plastic or paper)",
        "n_puffs_hfa" = "How many puffs were administered using the HFA Inhaler?",
        "pathway_chosen_compl" = "Compliant with nurse-selected pathway",
        "pathway_true_compl" = "compliant with correct pathway",
        "mouthpiece" = "Was a disposable mouthpiece used?",
        "n_puffs_respi" = "How many puffs were administered using the ProAir RespiClick?",
        "rcp_role" = "Type of person administered",
        "rcp_dx_pre" = "Previous Dagnosis",
        "rcp_dx_post" = "Has the student since received an asthma diagnosis?",
        "hc_help" = "Do they need assistance connecting with a healthcare provider or finding insurance coverage?",
        "inh_use_prev" = "Any Previous Usage",
        "why_given" = "Was inhaler administered to prevent symptoms or given due to breathing difficulties?",
        "severity" = "According to the protocol, was the individual exhibiting mild-moderate symptoms or severe symptoms?",
        "sym_new" = "As far as you are aware, is this the first time the individual has ever experienced these kind of symptoms at school?",
        "hypo_disp" = "In your professional opinion, what would have happened if the student/individual did not have access to stock inhaler medication?",
        "hypo_disp_rcd" = "Hypothetical_Disposition_simplified",
        "true_disp" = "What DID happen to the student/individual after the inhaler was given?",
        "true_disp_rcd" = "Actual_Disposition_simplified",
        "hosp" = "Did the individual go to the hospital after emergency services came?",
        "transport" = "How did they get to the hospital?"
    ) |>
    relocate(-matches("[^a-z_0-9]", perl = TRUE)) |>
    mutate(
        admin_date = as.Date(coalesce(as.Date(admin_date), as.Date(Timestamp)))
    ) |>
    arrange(admin_date) |>
    mutate(
        sy_season = fct_inorder(case_when(
            # start       end         sy_season
            # 2023-08-21	2023-11-30	fall 2023-2024
            admin_date < as.Date("2023-08-21") ~ "(Before SY 23-24)",
            admin_date <= as.Date("2023-11-30") ~ "fall 2023-2024",

            # 2023-12-01	2024-02-28	winter 2023-2024
            admin_date <= as.Date("2024-02-29") ~ "winter 2023-2024",

            # 2024-03-01	2024-06-06	spring 2023-2024
            admin_date <= as.Date("2024-06-06") ~ "spring 2023-2024",

            # 2024-06-07	2024-08-25	summer 2023-2024
            admin_date <= as.Date("2024-08-25") ~ "summer 2023-2024",

            # 2024-08-26	2024-11-30	fall 2024-2025
            admin_date <= as.Date("2024-11-30") ~ "fall 2024-2025",

            # 2024-12-01	2025-02-28	winter 2024-2025
            admin_date <= as.Date("2025-02-28") ~ "winter 2024-2025",

            # 2025-03-01	2025-06-12	spring 2024-2025
            admin_date <= as.Date("2025-06-12") ~ "spring 2024-2025",

            # 2025-06-13	2025-08-17	summer 2024-2025
            admin_date <= as.Date("2025-08-17") ~ "summer 2024-2025",
            admin_date <= as.Date("2026-08-17") ~ "sy 2025-2026"
        )),
        data_schoolyear = factor(
            gsub(".*\\s", "", sy_season),
            levels = c("2023-2024", "2024-2025", "2025-2026")
            ),
        season = factor(
            gsub("\\s.*", "", sy_season),
            levels = c("fall", "winter", "spring", "summer")
            )
        ) |>
    mutate(school_id = as.character(school_id)) |>
    droplevels() |>
    write_rds(find_analytic_data(params$analytic_files$inhaler_events))





# tabulate events by school and schoolyear
read_analytic(params$analytic_files$inhaler_events) |>
    summarise(
        n_events_sy = n(),
        .by         = c(school_id, data_schoolyear)
    ) |>
    right_join(
        read_analytic(params$analytic_files$school_chars) |>
            distinct(school_id) |>
            merge(
                data.frame(data_schoolyear = factor(c("2021-2022", "2022-2023", "2023-2024",
                                                      "2024-2025", "2025-2026"))),
                by  = NULL
            ), by = c("school_id", "data_schoolyear")
    ) |>

    mutate(n_events_sy = coalesce(n_events_sy, 0)) |>
    arrange(school_id, data_schoolyear) |>
    mutate(
        n_events_cum   = cumsum(n_events_sy),
        .by            = school_id
    ) |>
    mutate(
        any_events_sy  = factor(
            n_events_sy > 0,
            levels = c(FALSE, TRUE),
            labels = c("0 Events", "1+ Events")
        ),
        any_events_cum = factor(
            n_events_cum > 0,
            levels = c(FALSE, TRUE),
            labels = c("0 Events", "1+ Events")
        )
    ) |>
    droplevels() |>
    write_rds(find_analytic_data(params$analytic_files$inhaler_event_counts))






# School-Level Annual Vars ------------------------------------------------


# annual data: school level
# annual: count variables
school_annual_a <- map_dfr(c("student_gender", "student_race"), function(x) {
    read_analytic("student_counts") |>
        summarise(n = sum(student_count), .by = all_of(c("school_id", x, "data_schoolyear"))) |>
        rename(level = 2, data_schoolyear = 3) |>
        mutate(
            level = coalesce(
                as.character(level),
                "missing"
            ),
            x = x, .before = 1)
}) |>

    pivot_wider(
        names_from   = c(x, level),
        names_prefix = "n_",
        id_cols      = c(school_id, data_schoolyear),
        values_from  = n,
        values_fill  = 0
    ) |>
    janitor::clean_names()

# annual attendance
school_annual_b <- read_analytic("student_counts") |>
    summarise(
        attendance = weighted.mean(average_attendance, student_count),
        n_students = sum(student_count),
        across(matches("count_yes"), list(
            n   = ~ sum(.x, na.rm = TRUE)
            # p = ~ sum(.x, na.rm = TRUE) / sum(student_count)
        ),
        .names = "{.fn}_{gsub('_count', '', .col)}"
        ),
        .by = c(school_id, data_schoolyear)
    )


# combine counts and attendance
school_annual <- full_join(
    school_annual_a,
    school_annual_b,
    by = c("school_id", "data_schoolyear")
) |>
    mutate(
        across(
            all_of(starts_with("n_")),
            ~ (.x / n_students)*100,
            .names = "{gsub('^n_', 'p_', .col)}"
        ),
        .by = c(school_id, data_schoolyear)
    )

# combine school-level vars
right_join(
    full_join(
        read_analytic("school_chars"),
        read_analytic("inhaler_use_counts"),
        by = "school_id"
    ),
    school_annual,
    by = c("school_id", "data_schoolyear")
) |>

    mutate(
        any_events_3l = factor(
            case_when(
                wave == "Fall 2024" ~ "NA - Phase 4",
                .default = any_events_sy
            )
        )
    ) |>

    write_rds(find_analytic_data(params$analytic_files$school_counts_annual))
