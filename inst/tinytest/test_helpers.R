
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
# Bad lengths.
expect_error(pci:::flag_na(1:2, 1:3),
             "do.call\\(is_ok_lens, dots\\) is not TRUE")

expect_identical(pci:::flag_na(integer(0L)), logical(0L))
expect_identical(pci:::flag_na(1L), FALSE)
expect_identical(pci:::flag_na(1:3), rep_len(FALSE, 3L))

expect_identical(pci:::flag_na(integer(0L), 1L), logical(0L))
expect_identical(pci:::flag_na(integer(0L), 1:3), logical(0L))
expect_identical(pci:::flag_na(integer(0L), 1L, 1:3), logical(0L))

expect_identical(pci:::flag_na(integer(0L), 1L, NA), logical(0L))
expect_identical(pci:::flag_na(integer(0L), 1:3, NA), logical(0L))
expect_identical(pci:::flag_na(integer(0L), 1L, 1:3, NA), logical(0L))

expect_identical(pci:::flag_na(integer(0L), 1L, c(NA, NA, NA)), logical(0L))
expect_identical(pci:::flag_na(integer(0L), 1:3, c(NA, NA, NA)), logical(0L))
expect_identical(pci:::flag_na(integer(0L), 1L, 1:3, c(NA, NA, NA)),
                 logical(0L))

expect_identical(pci:::flag_na(NA), TRUE)
expect_identical(pci:::flag_na(c(NA, NA)), c(TRUE, TRUE))
expect_identical(pci:::flag_na(NA, 1:2), c(TRUE, TRUE))
expect_identical(pci:::flag_na(1L, c(NA, NA)), c(TRUE, TRUE))

expect_identical(pci:::flag_na(NaN), FALSE)
expect_identical(pci:::flag_na(c(NaN, NaN)), c(FALSE, FALSE))
expect_identical(pci:::flag_na(NaN, 1:2), c(FALSE, FALSE))
expect_identical(pci:::flag_na(1L, c(NaN, NaN)), c(FALSE, FALSE))

expect_identical(pci:::flag_na(c(NaN, NA)), c(FALSE, TRUE))
expect_identical(pci:::flag_na(NaN, 1:2, NA), c(TRUE, TRUE))
expect_identical(pci:::flag_na(1L, c(NaN, NaN), NA), c(TRUE, TRUE))

expect_identical(pci:::flag_na(NaN, c(1L, NA, 3L)), c(FALSE, TRUE, FALSE))


expect_identical(pci:::flag_na(1L, c(NA, NaN, NA, NaN, NA)),
                 c(TRUE, FALSE, TRUE, FALSE, TRUE))

expect_identical(pci:::flag_na(1L, c(NaN, NA, NaN, NA, NaN)),
                 c(FALSE, TRUE, FALSE, TRUE, FALSE))
