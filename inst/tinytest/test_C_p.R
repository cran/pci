

expect_error(C_p(), 'argument "sigma" is missing, with no default')
expect_error(C_p(1.), 'argument "lsl" is missing, with no default')
expect_error(C_p(1., 2.), 'argument "usl" is missing, with no default')
expect_error(C_p(1., 2., 3.),
             'argument "dl" is missing, with no default')

expect_error(C_p(1., 2., 3., 6., 7.), 'unused argument \\(7\\)')

expect_error(
  C_p("1..", 2., 3., 6.),
  '"sigma" must be a base-R numeric vector and contain no Inf values'
)

expect_error(
  C_p(1., "2.", 3., 6.),
  '"lsl" must be a base-R numeric vector and contain no Inf values'
)

expect_error(
  C_p(1., 2., "3.", 6.),
  '"usl" must be a base-R numeric vector and contain no Inf values'
)

expect_error(
  C_p(1., 2., 3., "6."),
  '"dl" must be a base-R numeric vector and contain no Inf values'
)

expect_error(
  C_p(1., 2:3, 3:5, 6.),
  'All vector arguments with length >1 must be of equal length'
)

expect_error(C_p(-1., 2., 3., 6.), '"sigma >= 0" is false')
expect_error(C_p(1., 3., 2., 6.), '"usl > lsl" is false')
expect_error(C_p(1., 2., 2., 6.), '"usl > lsl" is false')
expect_error(C_p(1., 2., 3., 0.), '"dl > 0" is false')

expect_identical(C_p(integer(0L), 2., 3., 6.), double(0L)) # int yields double

# Test NA over NaN.
expect_identical(C_p(1/6, 1., NaN, NA_real_), NA_real_)
expect_identical(C_p(1/6, 1., c(NaN, 2.), c(NA, 6.)), c(NA_real_, 1.))
expect_identical(C_p(1/6, c(1., 1.), NaN, NA_real_), c(NA_real_, NA_real_))
expect_identical(C_p(1/6, 1., NA_real_, NaN), NA_real_)
expect_identical(C_p(1/6, 1., c(NA_real_, 2.), c(NaN, 6.)), c(NA_real_, 1.))
expect_identical(C_p(1/6, c(1., 1.), NA_real_, NaN), c(NA_real_, NA_real_))

expect_identical(C_p(1/6, 1., 2., 6.), 1.) # known output
expect_identical(C_p(c(NA, 1/6), 1., 2., 6.), c(NA, 1.)) # output with NA's
expect_identical(C_p(c(NaN, 1/6), 1., 2., 6.), c(NaN, 1.)) # output with NaN's
# sigma == 0 yields NaN.
expect_identical(C_p(0., 1., 2., 6.), NaN)
expect_identical(C_p(c(0., 1/6), 1., 2., 6.), c(NaN, 1.))


# Assert that output won't carry names attribute.
expect_identical(C_p(c(foo = 1/6), 1., 2., 6.), 1.)
expect_identical(C_p(1/6, c(foo = 1.), 2., 6.), 1.)
expect_identical(C_p(1/6, 1., c(foo = 2.), 6.), 1.)
expect_identical(C_p(1/6, 1., 2., c(foo = 6.)), 1.)


# Test known relations to other PCI's.
expect_identical(
  C_p(1/6, 1., 2., 6.),
  (C_pl(1.5, 1/6, 1., 3.) + C_pu(1.5, 1/6, 2., 3.)) / 2.
)

expect_identical(
  C_p(.314, 1., 2., 6.),
  (C_pl(1.5, .314, 1., 3.) + C_pu(1.5, .314, 2., 3.)) / 2.
)


# Test pci_info$expr_r.
e = str2expression(pci_info["C_p", "expr_r"])
args1 = list(sigma = 1/6, lsl = 1., usl = 2., dl = 6.)
args2 = list(sigma = .314, lsl = 1., usl = 2., dl = 6.)
expect_identical(do.call(C_p, args1), eval(e, args1))
expect_identical(do.call(C_p, args2), eval(e, args2))
rm(e, args1, args2)

