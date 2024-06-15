
expect_error(C_pl(), 'argument "mu" is missing, with no default')
expect_error(C_pl(1.5), 'argument "sigma" is missing, with no default')
expect_error(C_pl(1.5, 1.), 'argument "lsl" is missing, with no default')
expect_error(C_pl(1.5, 1., 2.),
             'argument "l" is missing, with no default')

expect_error(C_pl(1.5, 1., 2., 3., 4.), 'unused argument \\(4\\)')
expect_error(
  C_pl("1.5", 1., 2., 3.),
  '"mu" must be a base-R numeric vector and contain no Inf values'
)

expect_error(
  C_pl(1.5, "1.", 2., 3.),
  '"sigma" must be a base-R numeric vector and contain no Inf values'
)

expect_error(
  C_pl(1.5, 1., "2.", 3.),
  '"lsl" must be a base-R numeric vector and contain no Inf values'
)

expect_error(
  C_pl(1.5, 1., 2., "3."),
  '"l" must be a base-R numeric vector and contain no Inf values'
)

expect_error(C_pl(1.5, 1:2, 2:4, 3.),
             'All vector arguments with length >1 must be of equal length')

expect_error(C_pl(1.5, -1., 2., 3.), '"sigma >= 0" is false')
expect_error(C_pl(1.5, 1., 2., 0.), '"l > 0L" is false')

expect_identical(C_pl(1.5, integer(0L), 2, 3.), double(0L)) # int yields double

# Test NA over NaN.
expect_identical(C_pl(3., 1/3, NaN, NA_real_), NA_real_)
expect_identical(C_pl(3., 1/3, NA_real_, NaN), NA_real_)

expect_identical(C_pl(3., 1/3, 2., 3.), 1.) # known output
expect_identical(C_pl(3., c(NA, 1/3), 2., 3.), c(NA, 1.)) # output with NA's
expect_identical(C_pl(3., c(NaN, 1/3), 2., 3.), c(NaN, 1.)) # output with NaN's
# sigma == 0 yields NaN.
expect_identical(C_pl(3., 0., 2., 3.), NaN)
expect_identical(C_pl(3., c(1/3, 0.), 2., 3.), c(1., NaN))


# Assert that output won't carry names attribute.
expect_identical(C_pl(c(foo = 3.), 1/3, 2., 3.), 1.)
expect_identical(C_pl(3., c(foo = 1/3), 2., 3.), 1.)
expect_identical(C_pl(3., 1/3, c(foo = 2.), 3.), 1.)
expect_identical(C_pl(3., 1/3, 2., c(foo = 3.)), 1.)


# Test pci_info$expr_r.
e = str2expression(pci_info["C_pl", "expr_r"])
args1 = list(mu = 3., sigma = 1/3, lsl = 2., l = 3.)
args2 = list(mu = 3., sigma = .314, lsl = 2., l = 3.)
expect_identical(do.call(C_pl, args1), eval(e, args1))
expect_identical(do.call(C_pl, args2), eval(e, args2))
rm(e, args1, args2)

