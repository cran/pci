% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{pci_info}
\alias{pci_info}
\title{pci_info}
\format{
\subsection{\code{pci_info}}{

\describe{
\item{pci_id}{
\code{character}. The ID of the PCI. Corresponds to the row name of the
data.frame in R.
}
\item{name_r}{
\code{character}. The variable name of the PCI, as may be referred to by other
metadata fields that contain R code.
}
\item{name_latex}{
\code{character}. The name of the PCI in LaTeX format.}
\item{name_r_expr}{
\code{character}. The name of the PCI in R expression format, useful for
working with plot labels or texts in R. To be used with
\code{str2expression(text)} in R.
}
\item{expr_r}{
\code{character}. An R expression of the PCI, in expanded form. Variable names
match parameter names of the PCI's function provided by the package. Can
be used with \code{str2expression(text)} in R.
}
\item{eq_latex}{
\code{character}. An equation of the PCI in LaTeX format.
}
\item{attributes}{
\code{AsIs}. The attributes column is essentially an unnamed \code{list}, thus
being a nested data structure. Each element belonging to a PCI is again
an unnamed \code{list}, which holds the actual attributes for that PCI. Each
attribute is a named \code{list}. Attributes detail additional technical
information about the PCI or its related concepts, e.g. the bounds of the
PCI. Note, all information pertains to the PCI's function provided by the
package. Each actual attribute has a 'type' (single \code{character}) field,
may have a 'target' (\code{character}) field, followed by fields specific to
that attribute type. The 'target' field indicates which entity the
attribute belongs to, e.g. the PCI or one of its terms.
}
}
}
}
\description{
Metadata about the process capability indices (PCIs).
}
