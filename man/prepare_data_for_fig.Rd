% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{prepare_data_for_fig}
\alias{prepare_data_for_fig}
\alias{prepare_newdata_fig_3}
\alias{prepare_data_fig_3A}
\alias{prepare_data_fig_3B}
\alias{prepare_data_fig_3C}
\alias{prepare_newdata_fig_S1}
\alias{prepare_data_fig_S1}
\alias{prepare_data_fig_S2}
\alias{prepare_data_fig_S6}
\title{Functions to prepare data for the figures}
\usage{
prepare_newdata_fig_3(birth_level_data, xaxis = c("age", "parity"))

prepare_data_fig_3A(fit_PP)

prepare_data_fig_3B(fit_IBI)

prepare_data_fig_3C(fit_twin)

prepare_newdata_fig_S1()

prepare_data_fig_S1(fit_AFB, nb_boot = 1000)

prepare_data_fig_S2(fit_PP, fit_IBI, fit_twin, mother_level_data)

prepare_data_fig_S6(simulation_obj, birth_level_data)
}
\arguments{
\item{birth_level_data}{a \code{tibble} or \code{data.frame} with birth level data (expanded or not)}

\item{xaxis}{a \code{character} indicating whether \code{"age"} or \code{"parity"} must be considered as the x-axis}

\item{fit_PP}{a fitted model for predicting the parity progression}

\item{fit_IBI}{a fitted model for predicting the interbirth interval}

\item{fit_twin}{a fitted model for predicting the per-birth probability of twinning}

\item{fit_AFB}{a fitted model for predicting the age at first birth}

\item{nb_boot}{the number of simulations for the parametric bootstrap used to
compute the intervals}

\item{mother_level_data}{a \code{tibble} or \code{data.frame} with mother level data}

\item{simulation_obj}{an object produced by \code{\link{run_simulation}}}
}
\description{
These functions are used to prepare the data required to draw the figures.
They allow for the computation of predictions averaged over the non focal fixed predictors.
}
\details{
We recommend you to look at the raw R code of these functions on GitHub (file
'/R/plots.R') to understand how they work. We commented the code to make this clear.
While you could directly look at the code of these functions while using the package, mind that
the comments will have been stripped away during the installation process.
}
\section{Functions}{
\itemize{
\item \code{prepare_newdata_fig_3}: an internal function to prepare the data used to generate the predictions to be plotted in Fig. 3

\item \code{prepare_data_fig_3A}: prepare the data to be plotted in Fig. 3A

\item \code{prepare_data_fig_3B}: prepare the data to be plotted in Fig. 3B

\item \code{prepare_data_fig_3C}: prepare the data to be plotted in Fig. 3C

\item \code{prepare_newdata_fig_S1}: an internal function to prepare the data used to generate the predictions to be plotted in Fig. S1

\item \code{prepare_data_fig_S1}: prepare the data to be plotted in Fig. S1

\item \code{prepare_data_fig_S2}: prepare the data to be plotted in Fig. S2

\item \code{prepare_data_fig_S6}: prepare the data to be plotted in Fig. S6
}}

\examples{
prepare_newdata_fig_3(expand_data(data_births_all), xaxis = "age")
prepare_newdata_fig_3(expand_data(data_births_all), xaxis = "parity")

# for more realistic use, see ?twinR

}
