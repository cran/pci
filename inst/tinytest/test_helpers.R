
# is_ok_lens() -----------------------------------------------------------------

# Missing args.
expect_error(pci:::is_ok_lens(), "\\.\\.\\.length\\(\\) > 0L is not TRUE")

# Positive cases.
expect_identical(pci:::is_ok_lens(integer(0L)), TRUE)
expect_identical(pci:::is_ok_lens(1L), TRUE)
expect_identical(pci:::is_ok_lens(1:3), TRUE)

expect_identical(pci:::is_ok_lens(integer(0L), 1L), TRUE)
expect_identical(pci:::is_ok_lens(integer(0L), 1:3), TRUE)
expect_identical(pci:::is_ok_lens(1L, 1:3), TRUE)
expect_identical(pci:::is_ok_lens(1:3, 1:3), TRUE)
expect_identical(pci:::is_ok_lens(1L, 1.), TRUE)
expect_identical(pci:::is_ok_lens(integer(0L), double(0L)), TRUE)

expect_identical(pci:::is_ok_lens(integer(0L), 1L, 1:3), TRUE)
expect_identical(pci:::is_ok_lens(integer(0L), 1L, 1:3, 1:3), TRUE)

expect_identical(pci:::is_ok_lens(integer(0L), 1L, 1., 1:3, double(3L)), TRUE)
expect_identical(
  pci:::is_ok_lens(integer(0L), double(0L), 1L, 1., 1:3, double(3L)),
  TRUE
)

# Negative cases.
expect_identical(pci:::is_ok_lens(integer(0L), 1L, 1:2, 1:3), FALSE)
expect_identical(pci:::is_ok_lens(1L, 1:3, 1:4), FALSE)
expect_identical(pci:::is_ok_lens(integer(0L), 1:3, 1:4), FALSE)
expect_identical(pci:::is_ok_lens(1:3, 1:4), FALSE)
expect_identical(pci:::is_ok_lens(1:3, 1:3, 1:4), FALSE)
expect_identical(pci:::is_ok_lens(1L, 1:3, 1:3, 1:4), FALSE)
expect_identical(pci:::is_ok_lens(1L, 1L, 1:3, 1:4), FALSE)
expect_identical(pci:::is_ok_lens(integer(0L), integer(0L), 1:3, 1:4), FALSE)


# is_any_len0() ----------------------------------------------------------------

# Missing args.
expect_error(pci:::is_any_len0(), "\\.\\.\\.length\\(\\) > 0L is not TRUE")

# Positive cases.
expect_identical(pci:::is_any_len0(integer(0L)), TRUE)
expect_identical(pci:::is_any_len0(integer(0L), 1L), TRUE)
expect_identical(pci:::is_any_len0(integer(0L), 1:3), TRUE)
expect_identical(pci:::is_any_len0(integer(0L), 1L, 1:3), TRUE)

# Negative cases.
expect_identical(pci:::is_any_len0(1L), FALSE)
expect_identical(pci:::is_any_len0(1:3), FALSE)
expect_identical(pci:::is_any_len0(1L, 1:3), FALSE)
expect_identical(pci:::is_any_len0(1L, 1:3, 1:4), FALSE)


# flag_na() -------------------------------------------------------------------

# Missing args.
expect_error(pci:::flag_na(), "\\.\\.\\.length\\(\\) > 0L is not TRUE")
# Not numeric vectors.
expect_error(pci:::flag_na("1."), "all\\(is_num_vec_, na.rm = FALSE\\)")
expect_error(pci:::flag_na("1.", 1.), "all\\(is_num_vec_, na.rm = FALSE\\)")
# Bad lengths.
expect_error(pci:::flag_na(1:2, 1:3),
             "do.call\\(is_ok_lens, dots\\) is not TRUE")

expect_identical(pci:::flag_na(integer(0L)), logical(0L))
expect_identical(pci:::flag_na(1L), FALSE)
expect_identical(pci:::flag_na(1:3), rep_len(FALSE, 3L))

expect_identical(pci:::flag_na(integer(0L), 1L), logical(0L))
expect_identical(pci:::flag_na(integer(0L), 1:3), logical(0L))
expect_identical(pci:::flag_na(integer(0L), 1L, 1:3), logical(0L))

expect_identical(pci:::flag_na(integer(0L), 1L, NA_integer_), logical(0L))
expect_identical(pci:::flag_na(integer(0L), 1:3, NA_integer_), logical(0L))
expect_identical(pci:::flag_na(integer(0L), 1L, 1:3, NA_integer_), logical(0L))

expect_identical(pci:::flag_na(integer(0L), 1L, rep_len(NA_integer_, 3L)),
                 logical(0L))

expect_identical(pci:::flag_na(integer(0L), 1:3, rep_len(NA_integer_, 3L)),
                 logical(0L))

expect_identical(pci:::flag_na(integer(0L), 1L, 1:3, rep_len(NA_integer_, 3L)),
                 logical(0L))

expect_identical(pci:::flag_na(NA_integer_), TRUE)
expect_identical(pci:::flag_na(c(NA_integer_, NA_integer_)), c(TRUE, TRUE))
expect_identical(pci:::flag_na(NA_integer_, 1:2), c(TRUE, TRUE))
expect_identical(pci:::flag_na(1L, c(NA_integer_, NA_integer_)), c(TRUE, TRUE))

expect_identical(pci:::flag_na(NaN), FALSE)
expect_identical(pci:::flag_na(c(NaN, NaN)), c(FALSE, FALSE))
expect_identical(pci:::flag_na(NaN, 1:2), c(FALSE, FALSE))
expect_identical(pci:::flag_na(1L, c(NaN, NaN)), c(FALSE, FALSE))

expect_identical(pci:::flag_na(c(NaN, NA_integer_)), c(FALSE, TRUE))
expect_identical(pci:::flag_na(NaN, 1:2, NA_integer_), c(TRUE, TRUE))
expect_identical(pci:::flag_na(1L, c(NaN, NaN), NA_integer_), c(TRUE, TRUE))

expect_identical(pci:::flag_na(NaN, c(1L, NA_integer_, 3L)),
                 c(FALSE, TRUE, FALSE))

expect_identical(pci:::flag_na(
  1L, c(NA_integer_, NaN, NA_integer_, NaN, NA_integer_)),
  c(TRUE, FALSE, TRUE, FALSE, TRUE)
)

expect_identical(pci:::flag_na(1L, c(NaN, NA_integer_, NaN, NA_integer_, NaN)),
                 c(FALSE, TRUE, FALSE, TRUE, FALSE))

# With NA_real_.
expect_identical(pci:::flag_na(NA_real_), TRUE)
expect_identical(pci:::flag_na(c(NA_real_, NA_real_)), c(TRUE, TRUE))
expect_identical(pci:::flag_na(NA_real_, 1:2), c(TRUE, TRUE))
expect_identical(pci:::flag_na(1L, c(NA_real_, NA_real_)), c(TRUE, TRUE))

# With named vectors.
named_len0 = integer(0L)
names(named_len0) = character(0L)
expect_identical(pci:::flag_na(named_len0), logical(0L))
rm(named_len0)

expect_identical(pci:::flag_na(c(foo = 1L)), FALSE)
expect_identical(pci:::flag_na(c(foo = 1L, bar = 2L, baz = 3L)),
                 rep_len(FALSE, 3L))

expect_identical(pci:::flag_na(c(foo = NA_integer_)), TRUE)
expect_identical(pci:::flag_na(c(foo = NA_integer_, bar = NA_integer_)),
                 c(TRUE, TRUE))

expect_identical(pci:::flag_na(c(foo = NA_integer_), c(bar = 1L, baz = 2L)),
                 c(TRUE, TRUE))

expect_identical(pci:::flag_na(
  c(foo = 1L), c(bar = NA_integer_, baz = NA_integer_)),
  c(TRUE, TRUE)
)

