
# Capture to-dos during analysis
add_todo <- function(x, create = (!exists("todos"))) {
    if(create) {
        todos <<- list()
    }
    todos <<- unique(rlist::list.append(todos, x))
}

# Create dated directories for data and output
dir_create_if <- function(dir) {
    if(!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
    }
    dir
}


# Read/Write Functions ----------------------------------------------------

# write an object to data and return it
write_data <- function(
        obj,
        name,
        dir         = file.path(get_global("project_dir"), "data"),
        # csv       = is.data.frame(obj),
        csv         = FALSE,
        datestp     = Sys.Date()
) {
    # maybe strip file extension
    name <- tools::file_path_sans_ext(name)

    # maybe datestamp
    if(is.Date(datestp)) {
        name <- paste0(name, "_", datestp)
        dir  <- file.path(dir, datestp)
    }

    # write object to rds file
    readr::write_rds(
        obj,
        file.path(
            dir_create_if(dir),
            glue::glue("{name}.rds"))
    )

    # maybe write object to csv file
    if(csv) {
        readr::write_csv(
            obj,
            file.path(dir, glue::glue("{name}.csv")),
            na = ""
        )
    }

    # return object
    obj
}

# write an object to output and return it
write_output <- function(
        obj,
        name,
        dir         = file.path(get_global("project_dir"), "output"),
        # csv       = is.data.frame(obj),
        csv         = TRUE,
        datestp     = Sys.Date()
) {
    # write and return object
    write_data(
        obj     = obj,
        name    = name,
        dir     = dir,
        csv     = csv,
        datestp = datestp
    )
}

#' Read a derived rds file in data/
#' @param name The name of the rds file (without extension)
#' @param dir The location of the file
#' @return The object
read_data <- function(
        name,
        dir     = file.path(get_global("project_dir"), "data"),
        datestp = NULL
) {

    # maybe strip file extension
    name <- tools::file_path_sans_ext(name)

    # maybe datestamp
    if(is.Date(datestp)) {
        name <- paste0(name, "_", datestp)
        dir  <- file.path(dir, datestp)
    }

    # read file
    readr::read_rds(
        file.path(dir, glue::glue("{name}.rds"))
    )
}


#' Save a plot to the output directory and return it
#' @param obj The plot (probably created by ggplot)
#' @param name The output file name
#' @param dir Location of output plot
#' @param ... Additional arguments passed to ggsave()
#' @return The plot
write_plot <- function(
        obj  = ggplot2::last_plot(),
        name,
        dir  = file.path(get_global("project_dir"), "output"),
        datestp     = Sys.Date(),
        ...
) {

    # maybe strip file extension
    name <- tools::file_path_sans_ext(name)

    # maybe datestamp
    if(is.Date(datestp)) {
        name <- paste0(name, "_", datestp)
        dir  <- dir_create_if(file.path(dir, datestp))
    }

    # save the plot
    ggplot2::ggsave(
        filename = glue::glue("{name}.png"),
        plot     = obj,
        path     = dir,
        width    = 8,
        height   = 6,
        units    = "in",
        dpi      = "retina",
        ...
    )

    # invisibly return the plot
    invisible(obj)
}


# Data Management Functions ---------------------------------------------------

set_colnames_as_labels <- function(df) {
    for(v in names(df)) {
        attr(df[[v]], "label") <- v
    }
    return(df)
}


# convert characters and numerals to factors if
# they have fewer than max_levels unique levels
detect_factors <- function(
        data, max_levels = 6,
        titlize          = TRUE,
        exclude          = NULL,
        exclude_numeric  = FALSE,
        level_labels     = NULL
) {

    if(exclude_numeric) {
        exclude <- c(exclude, data |> select(where(is.numeric)) |> names())
    }
    for (v in setdiff(names(data), c(exclude))) {
        lev <- sort(unique(na.omit(data[[v]])))
        if (length(lev) <= max_levels) {

            if (titlize) {
                lab <- snakecase::to_title_case(
                    as.character(lev),
                    parsing_option = 0
                )
            } else {lab <- lev}

            data[[v]] <- factor(data[[v]], levels = lev, labels = lab)
        }
    }
    data
}


# split a dataset by a variable and assign name
pipe_split <- function(d, x) {
    split(d, d[[x]])
}

pluck2 <- function(data, ...) {
    magrittr::extract(data, ...)
}

pipe_zip <- function(files,
                     zipfile,
                     zippath = file.path(get_global("project_dir"), "output", Sys.Date())
) {
    zip::zip(
        zipfile = file.path(zippath, zipfile),
        files   = files,
        mode    = "cherry-pick"
    )
}

# send a list of plots to a zipfile
zip_plots <- function(plot_list,
                      zip_name,
                      zip_path) {

    # Write each plot to a temp file
    tmploc <- tempdir()
    names(plot_list) <- snakecase::to_snake_case(names(plot_list))
    purrr::iwalk(plot_list, ~{
        write_plot(obj = .x, name = .y, dir = tmploc)
    })

    # write temp files to zip file
    pipe_zip(
        file.path(tmploc, paste0(names(plot_list), ".png")),
        zipfile = zip_name,
        zippath = zip_path
    )
}

fill_by <- function(data, by, ...) {
    data |>
        group_by(across(any_of(by))) |>
        fill(...) |>
        ungroup()
}

list_invert <- function(named_list) {
    names(named_list) |>
        purrr::set_names(unlist(named_list))
}

# separate a single column of select-all responses
# to multiple columns
split_select_all <- function(data, x, sep = ",", yes_val = 1, no_val = 0) {
    data |>
        # columns to rows
        pivot_longer(all_of(c(x))) |>
        relocate(name, value) |>

        mutate(value = str_split(value, sep)) |>
        unnest(value) |>

        mutate(
            value = snakecase::to_snake_case(coalesce(value, "none")),
            yn    = yes_val
        ) |>
        arrange(name, value) |>
        pivot_wider(
            names_from  = c("name", "value"),
            values_from = "yn",
            values_fill = no_val
        )
}

rename_by_label <- function(df, label, newname) {
    lab_list <- map(df, attr, "label")
    if(!(label %in% unlist(lab_list))) {return(df)}

    v_idx <- which(lab_list == label)
    names(df)[v_idx] <- newname

    df
}

# convert characters and numerals to factors if
# they have fewer than max_levels unique levels
detect_factors <- function(
        data, max_levels = 6, titlize = TRUE,
        exclude          = NULL,
        exclude_numeric  = FALSE
) {

    if(exclude_numeric) {
        exclude <- c(exclude, data |> select(where(is.numeric)) |> names())
    }
    for (v in setdiff(names(data), c(exclude))) {
        lev <- sort(unique(na.omit(data[[v]])))
        if (length(lev) <= max_levels) {

            if (titlize) {
                lab <- snakecase::to_title_case(
                    as.character(lev),
                    parsing_option = 0
                )
            } else {lab <- lev}

            data[[v]] <- factor(data[[v]], levels = lev, labels = lab)
        }
    }
    data
}

# exact, all, exhaustive, any
find_fct_by_level <- function(data, levels, match_type = "exact") {
    res <- c()
    for (x in names(data)) {
        unq_lvls <- unique(na.omit(data[[x]]))

        # number of items in search list, not in current column
        diff_1 <- length(setdiff(levels, unq_lvls))

        # number of items in current column, not in search list
        diff_2 <- length(setdiff(unq_lvls, levels))

        # exact match
        if (match_type == "exact") {
            if (diff_1 == 0 & diff_2 == 0) {
                res <- c(res, x)
            }
        } else if (match_type == "all") {
            if (diff_1 == 0) {
                res <- c(res, x)
            }
        } else if (match_type == "exhaustive") {
            if (diff_2 == 0) {
                res <- c(res, x)
            }
        } else if (match_type == "any") {
            if (diff_1 < length(levels)) {
                res <- c(res, x)
            }
        }

    }
    res
}


drop_empty_cols <- function(data) {
    data[, -which(colSums(is.na(data)) == nrow(data))]
}


df_to_list <- function(df, values, keys) {
    df <- distinct(df[, c(values, keys)])
    as.list(df[[values]]) |> set_names(df[[keys]])
}




# Formatting Functions ----------------------------------------------------

fmt_pct <- function(p, multiplier = 100, digits = 1, symbol = TRUE) {
    res <- sprintf(glue::glue("%.{digits}f"), p*multiplier)
    if(symbol) {res <- paste0(res, "%")}
    res
}

# format (vectorized) p-value(s)
fmt_pval <- function(p, nsmall = 3, stars = FALSE) {
    unlist(purrr::map(c(p), function(x) {

        if(length(na.omit(c(x))) != 1) {return("")}
        s <- ""
        if(stars) {
            s <- cut(
                x,
                breaks = c(-Inf, 0.001, 0.01, 0.05, 0.10, Inf),
                labels = c("***", "**", "*", ".", "")
            )
        }
        paste0(
            format.pval(
                x,
                digits = 1,
                nsmall = nsmall,
                eps    = as.numeric(
                    paste(c("0.", rep("0", nsmall-1), "1"), collapse = "")
                )
            ),
            s
        )
    }))
}


# format plain-text arsenal::tableby() labels
simplify_tb_labels <- function(label, space_to = "-") {
    gsub("&nbsp;", space_to, gsub("\\*+", "", label), fixed = TRUE)
}

fmt_numeric <- function(df, prop_to_pct = NULL, ...) {
    df |>
        mutate(across(
            any_of(c("p.value")),
            ~ fmt_pval(.x)
        )) |>

        mutate(across(
            any_of(c(prop_to_pct)),
            ~ .x*100
        )) |>

        mutate(across(
            where(is.numeric),
            ~ format(round(.x, 2), big.mark = ",")
        ))
}
# to match school names
standardize_text_col <- function(df, in_name, out_name = NULL, char_sub = list("_" = "")) {
    if(is.null(out_name)) {
        out_name <- in_name
    }

    in_vals <- df[[in_name]]
    out_vals <- snakecase::to_snake_case(as.character(in_vals))

    for (x in names(char_sub)) {
        out_vals <- gsub(x, char_sub[[x]], out_vals)
    }

    df[[out_name]] <- out_vals

    df |>
        relocate(
            all_of(out_name),
            .after = all_of(in_name)
        )
}

expand_impl_dates <- function(df, impl_date_name, data_date_name) {
    data_date     <- df[[data_date_name]]
    impl_date     <- df[[impl_date_name]]

    data_year     <- year(data_date)
    impl_year     <- year(impl_date)

    active_days   <- as.numeric(difftime(data_date, impl_date, units = "days"))
    active_months <- round(active_days/30.44)
    active_months[which(active_months < 0)] <- 0

    data.frame(
        df,
        impl_year     = impl_year,
        data_year     = data_year,
        active_months = active_months
    )
}

collapse_header_rows <- function(df, header_rows, start_row = max(header_rows)+1, clean = TRUE) {
    col_names <- df[header_rows,] |>
        summarise(across(everything(), ~ paste(na.omit(unlist(.x)), collapse = "; "))) |>
        unlist()
    col_names[which(str_length(col_names) == 0)] <- NA

    if(clean) {col_names <- snakecase::to_snake_case(col_names)}
    names(df) <- coalesce(col_names, paste0("x", 1:ncol(df)))

    return(df[start_row:nrow(df),])
}
