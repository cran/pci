

is_all_unique = function(x) {
  return(length(unique(x)) == length(x))
}


expect_ok_chr_vec = function(x) {
  expect_true(vek::is_chr_vec_nxb(x))
  expect_false(any(startsWith(x, " "), na.rm = TRUE)) # no leading spaces
  expect_false(any(endsWith(x, " "), na.rm = TRUE)) # no trailing spaces
}


grepl_ = function(pattern_, x_) {
  return(grepl(
    pattern = pattern_,
    x = x_,
    ignore.case = FALSE,
    perl = FALSE,
    fixed = FALSE,
    useBytes = FALSE
  ))
}


is_valid_r_name = function(x) {
  return(grepl_("^((([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*)|[.])$", x))
}


expect_true(is.data.frame(pci_info))
expect_identical(class(pci_info), "data.frame")
expect_true(ncol(pci_info) > 0L)
expect_true(nrow(pci_info) > 0L)
expect_true(length(unique(colnames(pci_info))) == ncol(pci_info))
expect_false(anyNA(colnames(pci_info), recursive = FALSE))
expect_false(any(colnames(pci_info) == "", na.rm = TRUE))


colnames_1_to_6 = c(
  "pci_id", "name_r", "name_latex", "name_r_expr", "expr_r", "eq_latex")

expect_identical(colnames(pci_info)[1:6], colnames_1_to_6) # v1.0.0 columns
rm(colnames_1_to_6)


rownames_1_to_5 = c("C_p", "C_pl", "C_pu", "C_pk", "C_pm")  # v1.0.0 rows
expect_identical(row.names(pci_info)[1:5], rownames_1_to_5)
rm(rownames_1_to_5)


# Test column 'pci_id' ---------------------------------------------------------
expect_ok_chr_vec(pci_info$pci_id)
expect_true(all(is_valid_r_name(pci_info$pci_id), na.rm = TRUE))
expect_false(any(grepl_("\\.", pci_info$pci_id), na.rm = TRUE)) # no periods
expect_false(any(endsWith(pci_info$pci_id, "_"), na.rm = TRUE))

expect_true(is_all_unique(pci_info$pci_id))

# Assert that row names equal 'pci_id' values.
expect_true(all(row.names(pci_info) == pci_info$pci_id, na.rm = FALSE))


# Test column 'name_r' ---------------------------------------------------------
expect_ok_chr_vec(pci_info$name_r)
expect_true(all(is_valid_r_name(pci_info$name_r), na.rm = TRUE))
expect_false(any(grepl_("\\.", pci_info$name_r), na.rm = TRUE)) # no periods
expect_false(any(endsWith(pci_info$name_r, "_"), na.rm = TRUE))

expect_true(is_all_unique(pci_info$name_r))


# Test column 'name_latex' -----------------------------------------------------
expect_ok_chr_vec(pci_info$name_latex)
expect_true(is_all_unique(pci_info$name_latex))


# Test column 'expr_r' ---------------------------------------------------------
expect_ok_chr_vec(pci_info$expr_r)
expect_true(is_all_unique(pci_info$expr_r))

# Assert that each character in 'expr_r' parses to a valid R expression.
for (x in pci_info$expr_r) {
  expect_true(is.expression(str2expression(x)))
}


# Test column 'eq_latex' -------------------------------------------------------
expect_ok_chr_vec(pci_info$eq_latex)
expect_true(is_all_unique(pci_info$eq_latex))


# Test column 'name_r_expr' ----------------------------------------------------
expect_ok_chr_vec(pci_info$name_r_expr)
expect_true(is_all_unique(pci_info$name_r_expr))

# Assert that each character in 'name_r_expr' parses to a valid R expression.
for (x in pci_info$name_r_expr) {
  expect_true(is.expression(str2expression(x)))
}


# Attributes column 'attributes' -----------------------------------------------
expect_identical(class(pci_info$attributes), "AsIs")
expect_identical(typeof(pci_info$attributes), "list")

for (pci_i in 1:nrow(pci_info)) {
  pci_attr = pci_info$attributes[pci_i]

  # Assert that the PCI's attribute is an unnamed list.
  expect_true(is.null(names(pci_attr)))

  for (pci_attr_entry in pci_attr) {
    # Assert that each attribute entry must is a named list.
    expect_false(is.null(names(pci_attr_entry)))
    expect_false(any(names(pci_attr_entry) == "", na.rm = FALSE))
    expect_true(is_all_unique(names(pci_attr_entry)))

    expect_true(length(pci_attr_entry) > 1L)

    # Assert that each attribute entry has a 'type' field
    expect_true("type" %in% names(pci_attr_entry))
    expect_true(names(pci_attr_entry)[[1L]] == "type")
    expect_true(vek::is_chr_vec_nxb1(pci_attr_entry$type))
  }
}
