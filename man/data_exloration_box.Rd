% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_exploratory_module.R
\name{data_exloration_box}
\alias{data_exloration_box}
\title{Display box plot}
\usage{
data_exloration_box(tisefka = NULL, x = NULL, y = NULL,
  aggregation_metric = "Sum")
}
\arguments{
\item{tisefka}{data frame including date variable}

\item{x}{explaining variable}

\item{y}{explained variable}

\item{aggregation_metric}{aggregation metric (sum mean max min)}
}
\value{
a boxplot plotly object
}
\description{
Display box plot
}
\author{
Farid Azouaou
}
