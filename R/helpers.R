

get_msg_fmt__not_num_vec_z = function() {
  return(
    '"%s" must be a base-R numeric vector and contain no Inf values')
}


get_msg__not_ok_lens = function() {
  return('All vector arguments with length >1 must be of equal length')
}


get_montgomery_ref_str = function() {
  return(paste(
    "Montgomery, D. C. (2019). \\emph{Introduction to statistical quality ",
    "control} (8th ed.). Wiley. ISBN: 978-1-119-39930-8",
    collapse = ""
  ))
}


#' @name is_ok_lens
#' @description
#' Takes a number of objects and returns `TRUE` if all objects with length
#' greater than 1 (if any) are of equal length, else returns `FALSE`.
#'
#' @noRd
is_ok_lens = function(...) {
  stopifnot(...length() > 0L)

  len = lengths(list(...), use.names = FALSE)
  len = len[!(len %in% 0:1)]
  return(length(unique(len)) <= 1L)
}


#' @name is_any_len0
#' @description
#' Takes a number of objects and returns `TRUE` if any are of length 0, else
#' `FALSE`.
#'
#' @noRd
is_any_len0 = function(...) {
  stopifnot(...length() > 0L)

  return(0L %in% lengths(list(...), use.names = FALSE))
}



#' @name flag_na
#' @description
#' Takes a number of objects and returns a logical vector of values `TRUE` or
#' `FALSE` indicating which elements are `NA`, with length equal to the length
#' of the object of the greatest length (unless any has length 0, in which case
#' `logical(0L)` is returned).
#'
#' Recycles objects of length 1. So, if any length-1 object is `NA`, then a
#' vector of which all values are `TRUE` is returned.
#'
#' The lengths of all objects must satisfy `is_ok_lens()`.
#'
#' @noRd
flag_na = function(...) {
  stopifnot(...length() > 0L)

  dots = list(...)

  stopifnot(do.call(is_ok_lens, dots))

  len = lengths(dots, use.names = FALSE)
  max_len = max(len, na.rm = FALSE)

  stopifnot(exprs = {
    vek::is_int_vec_nx(len)
    vek::is_int_vec_nx1(max_len)
  })

  if (0L %in% len)
    return(logical(0L))

  if (1L %in% len) {
    is_na_ = vapply(
      X = dots[len == 1L],
      FUN = function(x) { return(is.na(x) && !is.nan(x)) },
      FUN.VALUE = logical(1L),
      USE.NAMES = FALSE
    )

    stopifnot(vek::is_lgl_vec_nx(is_na_))

    if (any(is_na_, na.rm = FALSE))
      return(rep_len(TRUE, max_len))

    if (max_len == 1L)
      return(FALSE)
  }

  na_mask = lapply(X = dots[len > 1L], FUN = function(x) {
    return(is.na(x) & !is.nan(x))
  })

  na_mask = do.call(rbind, na_mask)
  stopifnot(exprs = {
    is.matrix(na_mask)
    ncol(na_mask) == max_len
    nrow(na_mask) == sum(len > 1L, na.rm = FALSE)
  })

  na_mask = apply(X = na_mask, MARGIN = 2L, FUN = function(x) {
    return(any(x, na.rm = FALSE))
  }, simplify = TRUE)

  stopifnot(exprs = {
    vek::is_lgl_vec_nx(na_mask)
    length(na_mask) == max_len
  })

  return(na_mask)
}

