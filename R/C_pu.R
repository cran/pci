

#' @name C_pu
#' @title \eqn{C_{pu}}
#' @description
#' \deqn{`r pci_info["C_pu", "eq_latex"]`}
#'
#' Note. This function allows for negative \eqn{C_{pu}} values.
#'
#' Only vectors of length 1 are recycled.
#'
#' `NA`'s take precedence over `NaN`'s, e.g. `NaN + NA` will output `NA`.
#'
#' Yields `NaN` if `sigma` equals 0.
#'
#' @param mu `numeric`.
#' @param sigma `numeric`.
#' @param usl `numeric`.
#' @param l `numeric`. Conventionally set to 3. Must be greater than 0.
#' @returns `double`.
#' @references `r get_montgomery_ref_str()`
#' @seealso [C_pl()], [C_pk()]
#' @examples
#' set.seed(1L)
#' data = rnorm(n = 30L, mean = 3., sd = 1.)
#' C_pu(mu = mean(data), sigma = sd(data), usl = 6., l = 3.)
#' # [1] 1.052367
#'
#' @export
C_pu = function(mu, sigma, usl, l) {
  if (!vek::is_num_vec_z(mu))
    stop(sprintf(get_msg_fmt__not_num_vec_z(), "mu"))
  else if (!vek::is_num_vec_z(sigma))
    stop(sprintf(get_msg_fmt__not_num_vec_z(), "sigma"))
  else if (!vek::is_num_vec_z(usl))
    stop(sprintf(get_msg_fmt__not_num_vec_z(), "usl"))
  else if (!vek::is_num_vec_z(l))
    stop(sprintf(get_msg_fmt__not_num_vec_z(), "l"))
  else if (!is_ok_lens(mu, sigma, usl, l))
    stop(get_msg__not_ok_lens())
  else if (!all(sigma >= 0L, na.rm = TRUE))
    stop('"sigma >= 0" is false')
  else if (!all(l > 0L, na.rm = TRUE))
    stop('"l > 0L" is false')

  if (is_any_len0(mu, sigma, usl, l))
    return(double(0L))

  if (any(sigma == 0L, na.rm = TRUE))
    sigma[sigma == 0L] = NaN

  is_na_ = flag_na(mu, sigma, usl, l)

  val = (usl - mu) / (l * sigma)
  names(val) = NULL

  if (any(is_na_, na.rm = FALSE))
    val[is_na_] = NA_real_

  stopifnot(vek::is_dbl_vec_nz(val))
  return(val)
}
