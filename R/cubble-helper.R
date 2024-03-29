find_invariant <- function(data, key) {
  key <- as_name(enquo(key))
  data <- tibble::as_tibble(data)

  if (length(key) != 1){
    cli::cli_abort("Only one key for {.fn find_invariant}.")
  }

  # remove the list-column, useful in a nested form
  data[map(data, class) == "list"] <- NULL

  key_col <- data[,key]
  nested_col <- data[,which(names(data) != key)]
  list_col <- vec_split(nested_col, key_col)$val

  if (length(list_col) > 10000){
    list_col <- list_col[1]
    cli::cli_alert_info(
      "More than 10,000 keys: only use the first key to test spatial &
      temporal variables."
      )
  }

  out <- map(list_col, function(data){
    var_length <- map_dbl(colnames(data), ~ nrow(unique(data[.x])))
    c(key, colnames(data)[var_length == 1])
  })
  invariant <- Reduce(intersect, out)
  names(invariant) <- NULL

  col_names <- names2(data)
  variant <- col_names[!col_names %in% invariant]

  list(variant = variant, invariant = invariant)
}


get_listcol <- function(data){

  if ("ts" %in% names(data)) return("ts")
  # out <- names(data)[which(names(data) == "ts")]
  # if (inherits(data[[out]], "list")) return(out)

  # if the list column is not named ts, detect the list-column
  out <- names(data)[map(data, class) == "list"]
  if (length(out) == 1){
    return(out)
  }else{
    cli::cli_abort("Can't determine the list column, please specify it")
  }
}


