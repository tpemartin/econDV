% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplots.R
\name{get_break_labels}
\alias{get_break_labels}
\title{Create ggplot breaks/labels/limits that make sample period easily to understand}
\usage{
get_break_labels(dateSeries, dY = 1, labeltype = "yy")
}
\arguments{
\item{dateSeries}{A date series from lubridate::ymd()}

\item{dY}{An integer. The span between two break years. Default=1}

\item{labeltype}{A character, either 'yyyy' or 'yy', specifies how you want
the year to be labels.}
}
\value{
A list of 3: breaks, labels and limits, for you to use in scale_...
in ggplot2
}
\description{
Create ggplot breaks/labels/limits that make sample period easily to understand
}
\examples{
none
}
