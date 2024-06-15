

#' @name C_p
#' @title \eqn{C_p}
#' @description
#' \deqn{`r pci_info["C_p", "eq_latex"]`}
#'
#' Only vectors of length 1 are recycled.
#'
#' `NA`'s take precedence over `NaN`'s, e.g. `NaN + NA` will output `NA`.
#'
#' Yields `NaN` if `sigma` equals 0.
#'
#' @param sigma `numeric`.
#' @param lsl `numeric`.
#' @param usl `numeric`.
#' @param dl `numeric`. \eqn{L}, conventionally set to 6. Must be greater than
#' 0.
#' @returns `double`.
#' @references `r get_montgomery_ref_str()`
#' @examples
#' set.seed(1L)
#' data = rnorm(n = 30L, mean = 3., sd = 1.)
#' C_p(sigma = sd(data), lsl = 0., usl = 6., dl = 6.)
#' # [1] 1.08211
#'
#' @export
C_p = function(sigma, lsl, usl, dl) {
  if (!vek::is_num_vec_z(sigma))
    stop(sprintf(get_msg_fmt__not_num_vec_z(), "sigma"))
  else if (!vek::is_num_vec_z(lsl))
    stop(sprintf(get_msg_fmt__not_num_vec_z(), "lsl"))
  else if (!vek::is_num_vec_z(usl))
    stop(sprintf(get_msg_fmt__not_num_vec_z(), "usl"))
  else if (!vek::is_num_vec_z(dl))
    stop(sprintf(get_msg_fmt__not_num_vec_z(), "dl"))
  else if (!is_ok_lens(sigma, lsl, usl, dl))
    stop(get_msg__not_ok_lens())
  else if (!all(sigma >= 0L, na.rm = TRUE))
    stop('"sigma >= 0" is false')
  else if (!all(usl > lsl, na.rm = TRUE))
    stop('"usl > lsl" is false')
  else if (!all(dl > 0L, na.rm = TRUE))
    stop('"dl > 0" is false')

  if (is_any_len0(sigma, lsl, usl, dl))
    return(double(0L))

  if (any(sigma == 0L, na.rm = TRUE))
    sigma[sigma == 0L] = NaN

  is_na_ = flag_na(sigma, lsl, usl, dl)

  val = (usl - lsl) / (dl * sigma)
  names(val) = NULL

  if (any(is_na_, na.rm = FALSE))
    val[is_na_] = NA_real_

  stopifnot(exprs = {
    vek::is_dbl_vec_nz(val)
    all(val > 0L, na.rm = TRUE)
  })

  return(val)
}
