# get attributes of a file in a location on box (if it exists)
box_file_attr <- function(f_name, dir_id) {
  f_list <- boxr::box_ls(dir_id = dir_id)
  if(length(f_list) == 0) {return(NULL)}
  
  f_list |> 
    as.data.frame() |> 
    filter(name == f_name) |> 
    as.list() |> 
    compact()
}


# add a timestamped `comment` about `data_file` to the top of a `log_file` csv
# on box
box_add_to_log <- function(
    data_file, comment, ...,
    log_file,
    dir_id,
    ts       = Sys.time(),
    silent   = FALSE
  ) {
  
  # get attributes of log file (if it exists)
  log_attr <- box_file_attr(f_name = log_file, dir_id = dir_id)
  
  # read or create log file
  if(length(log_attr) == 0) {
    log_df <- data.frame(
      timestamp = character(), 
      file      = character(), 
      comment   = character()
    )
  } else {
    log_df <- boxr::box_read_csv(log_attr$id) |> 
      mutate(across(everything(), as.character))
  }
  
  # add new entry to top of log
  log_df <- full_join(
    data.frame(
      timestamp = as.character(ts),
      file      = as.character(data_file),
      comment   = as.character(comment),
      ...
    ),
    
    log_df,
    
    by = c("timestamp", "file", "comment")
  )
  
  # write to box
  boxr::box_write(
    log_df,
    file_name = log_file,
    dir_id    = dir_id
  )
  
  # (maybe) return full log
  if(!silent) {return(log_df)}
}

# compare the contents of two data frames (ignoring R class, etc)
equal_dfs <- function(a, b) {
  a <- data.frame(a) |> mutate(across(everything(), as.character)) |> type_convert()
  b <- data.frame(b) |> mutate(across(everything(), as.character)) |> type_convert()  
  all.equal(a, b)
}


# check if data exists in its current form on box; 
# if not, write new version and log the change
box_write_if_diff <- function(
    object, f_name, dir_id, 
    comment, log_file
    ) {
  
  # compare old and new data
  new      <- object
  old_attr <-  box_file_attr(f_name, dir_id)
  if(length(old_attr) == 0) {
    has_changed <- TRUE
  } else {
    old         <- boxr::box_read(old_attr$id)
    has_changed <- !isTRUE(equal_dfs(old, new))
  }
  
  # (maybe) write to file
  if(has_changed) {
    boxr::box_write(new, file_name = f_name, dir_id = dir_id)
    
    # log with comment
    box_add_to_log(
      data_file = f_name,
      comment   = comment, 
      log_file  = log_file,
      dir_id    = dir_id,
      silent    = TRUE
    )
    cat(paste0("New version uploaded to ", dir_id, "/", f_name))
    
    } else {
      cat("\nNo change from saved file.\n")
    }
  
  
  return(new)
}





