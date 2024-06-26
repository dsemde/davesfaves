% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/davesfaves.R
\name{scale_fill_dave_d}
\alias{scale_fill_dave_d}
\title{Discrete fill palettes}
\usage{
scale_fill_dave_d(palette, direction = "foreward")
}
\arguments{
\item{palette}{Name of the colour palette: 'thuenen_all', 'thuenen_primary', 'thuenen_secondary', 'daves_faves'}

\item{direction}{Direction of the colour palette: 'forward' or 'reverse'}
}
\value{
Colour palette
}
\description{
Apply colour palettes to plot fills
}
\examples{
ggplot(dat, aes(x, y)) + scale_fill_dave_d("daves_faves");
ggplot(dat, aes(x, y)) + scale_fill_dave_d("thuenen_primary", direction = "reverse");
}
