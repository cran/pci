

expect_error(C_pk(), 'argument "mu" is missing, with no default')
expect_error(C_pk(2.5), 'argument "sigma" is missing, with no default')
expect_error(C_pk(2.5, 1.), 'argument "lsl" is missing, with no default')
expect_error(C_pk(2.5, 1., 2.), 'argument "usl" is missing, with no default')
expect_error(C_pk(2.5, 1., 2., 3.),
             'argument "dl" is missing, with no default')

expect_error(C_pk(2.5, 1., 2., 3., 6., 7.), 'unused argument \\(7\\)')
expect_error(
  C_pk("2.5", 1., 2., 3., 6.),
  '"mu" must be a base-R numeric vector and contain no Inf values'
)

expect_error(
  C_pk(2.5, "1.", 2., 3., 6.),
  '"sigma" must be a base-R numeric vector and contain no Inf values'
)

expect_error(
  C_pk(2.5, 1., "2.", 3., 6.),
  '"lsl" must be a base-R numeric vector and contain no Inf values'
)

expect_error(
  C_pk(2.5, 1., 2., "3.", 6.),
  '"usl" must be a base-R numeric vector and contain no Inf values'
)

expect_error(
  C_pk(2.5, 1., 2., 3., "6."),
  '"dl" must be a base-R numeric vector and contain no Inf values'
)

expect_error(C_pk(2.5, 1., 2:3, 3:5, 6.),
             'All vector arguments with length >1 must be of equal length')

expect_error(C_pk(2.5, -1., 2., 3., 6.), '"sigma >= 0" is false')
expect_error(C_pk(2.5, 1., 3., 2., 6.), '"usl > lsl" is false')
expect_error(C_pk(2.5, 1., 2., 2., 6.), '"usl > lsl" is false')
expect_error(C_pk(2.5, 1., 2., 3., 0.), '"dl > 0" is false')

# Test int yields double.
expect_identical(C_pk(2.5, integer(0L), 2., 3., 6.), double(0L))

# Test NA over NaN.
expect_identical(C_pk(2.5, 1/6, NaN, 3., NA_real_), NA_real_)
expect_identical(C_pk(2.5, 1/6, NA_real_, 3., NaN), NA_real_)

expect_identical(C_pk(2.5, 1/6, 2., 3., 6.), 1.) # known output
# Output with NA's.
expect_identical(C_pk(2.5, c(NA, 1/6), 2., 3., 6.), c(NA, 1.))
# Output with NaN's.
expect_identical(C_pk(2.5, c(NaN, 1/6), 2., 3., 6.), c(NaN, 1.))

# sigma == 0 yields NaN.
expect_identical(C_pk(2.5, 0., 2., 3., 6.), NaN)
expect_identical(C_pk(2.5, c(0., 1/6), 2., 3., 6.), c(NaN, 1.))


# Assert that output won't carry names attribute.
expect_identical(C_pk(c(foo = 2.5), 1/6, 2., 3., 6.), 1.)
expect_identical(C_pk(2.5, c(foo = 1/6), 2., 3., 6.), 1.)
expect_identical(C_pk(2.5, 1/6, c(foo = 2.), 3., 6.), 1.)
expect_identical(C_pk(2.5, 1/6, 2., c(foo = 3.), 6.), 1.)
expect_identical(C_pk(2.5, 1/6, 2., 3., c(foo = 6.)), 1.)


# Test special cases.
expect_identical(C_pk(2.5, 1., 2., 3., 6.), C_p(1., 2., 3., 6.))


# Assert that the output matches that of the alternative formulation.
expect_identical(
  C_pk(2.5, 1., 2., 3., 6.),
  min(C_pl(2.5, 1., 2., 3.), C_pu(2.5, 1., 3., 3.), na.rm = FALSE)
)

expect_identical(
  C_pk(2.24, 1., 2., 3., 6.),
  min(C_pl(2.24, 1., 2., 3.), C_pu(2.24, 1., 3., 3.), na.rm = FALSE)
)


# Test pci_info$expr_r.
e = str2expression(pci_info["C_pk", "expr_r"])
args1 = list(mu = 2.5, sigma = 1/6, lsl = 2., usl = 3.,
             dl = 6.)

args2 = list(mu = 2.5, sigma = .314, lsl = 2., usl = 3.,
             dl = 6.)

expect_identical(do.call(C_pk, args1), eval(e, args1))
expect_identical(do.call(C_pk, args2), eval(e, args2))
rm(e, args1, args2)
