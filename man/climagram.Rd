% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/climagram.R
\name{climagram}
\alias{climagram}
\title{Implementation of the so-called "climagrams" for seasonal forecasts}
\usage{
climagram(
  hindcast,
  forecast = NULL,
  obs = NULL,
  year.target = NULL,
  use.anomalies = TRUE,
  violin = TRUE,
  add.points = FALSE,
  detrend = FALSE,
  add.legend = TRUE,
  clim.time.frame = "monthly",
  add.eqc.info = TRUE,
  tercile.member.counts = FALSE
)
}
\arguments{
\item{hindcast}{A climate4R multi-member grid with the hindcast for verification.}

\item{forecast}{A climate4R multi-member grid with the forecast. Default is \code{NULL}. See details.}

\item{obs}{A climate4R grid with the reference observations. They should match the hindcast period.
Default is \code{NULL}.}

\item{year.target}{Year within the hindcast period considered as forecast. Default is NULL. See Details}

\item{use.anomalies}{Should anomalies or the original variable magnitudes be used for data representation? 
Default to \code{TRUE} (i.e., anomalies). See Details.}

\item{violin}{Logical flag indicating whether a violin plot should be added to the graph instead
of an ordinary boxplot. Default is \code{TRUE}.}

\item{add.points}{Logical flag indicating whether crosses indicating the individual forecast members
should be added to the graph. Default to \code{FALSE}.}

\item{detrend}{Logical indicating if the data should be (linearly) detrended. Default is FALSE.}

\item{add.legend}{Should legend be added to the plot. Default to \code{TRUE}}

\item{clim.time.frame}{Default to \code{"monthly"}. Temporal frame used as reference for the calculation of the climatological
normal. See Details.}

\item{add.eqc.info}{Should EQC (Evaluation and Quality Control) information be added onto the plot?.
Ignored if no \code{obs} is passed. See Details}

\item{tercile.member.counts}{Logical flag indicating whether the number of forecast members in each tercile
category should be indicated in the plot. Default to \code{FALSE}.}
}
\value{
Plots a climagram. Some degree of transparency is given
to the forecast boxes/violins to avoid a potential masking of underlying information.
}
\description{
A "climagram" is a box plot for visualization of a seasonal forecast with added EQC information from the hindcast
(and possibly the verifying observations).
}
\details{
\strong{Forecast data}

The product is conceived to display forecast information in an operative framework. However, for
testing purposes, a target year from the hindcast can be extracted via the \code{year.target} argument and 
treated as the forecast year (see last example). Note that all the hindcast statistics 
(including the optional detrending) will be computed after extracting the target year from the hindcast 
(and the observations).

\strong{Spatial averaging}

For spatial domains encompassing multiple points/grid points, the spatial average is first computed to obtain a
unique series for the whole domain via \code{\link[transformeR]{aggregateGrid}} (latitude cosine 
weighting is used).

\strong{Forecast/Hindcast representation}
  
In order to represent the forecast and hindcast information, \dQuote{violins} are used as default,
although boxplots (see \code{\link[graphics]{boxplot}}) can be obtained by setting 
argument \code{violin} to \code{FALSE}. For further details on violin construction see
the documentation of the function \code{\link[vioplot]{vioplot}}.
 
\strong{Using anomalies}

By default, anomalies are displayed. In this case, the forecast anomalies 
will be calculated considering the hindcast climatology (see next section for details). 

\strong{Climatological reference period}

The hindcast climatology is calculated by considering the annual multimember averages
 (i.e., the univariate annual time series of ensemble means).
    
By default, the climatology is computed on a monthly basis. For instance,
assuming a forecast for DJF is given, the DJF anomaly will be computed by subtracting to the 
January forecast the January climatology, to February the February climatology and so on.
If \code{clim.time.frame} is set to \code{"monthly"}, the climatology is computed using the seasonal climatology
, so the anomalies for January will be computed using the DJF climatology, and the same for the remaining months.
See argument \code{"time.frame"} of \code{\link[transformeR]{scaleGrid}}, to which the \code{clim.time.frame}
argument is passed, for more details.

\strong{EQC Information}  

EQC (Evaluation and Quality Control) information can be optionally added to the plot. This is the default behaviour when 
\code{obs} is supplied. Here the CRPS (bias-sensitive) measure is considered.
}
\note{
All inputs for this function must be at a monthly temporal resolution.

The computation of climatological quantiles requires a representative period to obtain meaningful results.
}
\examples{
\dontrun{
my_load <- function(file.url, verbose = TRUE) {
    tmpfile <- tempfile()
    download.file(url = file.url, destfile = tmpfile)
    readRDS(tmpfile)
}
hindcast <- my_load("http://meteo.unican.es/work/visualizeR/data/tas.cfs.enso34.rds")
forecast <- my_load("http://meteo.unican.es/work/visualizeR/data/tas.cfs.2016.enso34.rds")
# A seasonal forecast product, without reference obs information
climagram(hindcast = hindcast, forecast = forecast, violin = TRUE, obs = NULL)
# Violins can be replaced by ordinary boxplots:
climagram(hindcast = hindcast, forecast = forecast, violin = FALSE, obs = NULL)
# Instead of anomalies, raw values can be used: 
climagram(hindcast = hindcast, forecast = forecast, use.anomalies = FALSE)
# Observation quartiles are depicted by the backgroud shadows (median indicated by the dotted line):
# CRPS is added by default when observations are included:
obs <- my_load("http://meteo.unican.es/work/visualizeR/data/tas.ncep.enso34.rds")
climagram(hindcast = hindcast, forecast = forecast, obs = obs)
# An arbitrary year can be extracted from the hindcast to be used as forecast
climagram(hindcast = hindcast, year.target = 2001, obs = obs)
# Adding tercile counts:
climagram(hindcast = hindcast, year.target = 2001, obs = obs, tercile.member.count = TRUE)
}
}
\seealso{
Type \code{utils::RShowDoc("climagram_vignette", package = "visualizeR")} for further worked
 examples on climagram options.

Other visualization functions: 
\code{\link{bubblePlot}()},
\code{\link{cascadePlot}()},
\code{\link{reliabilityCategories}()},
\code{\link{spreadPlot}()},
\code{\link{tercileBarplot}()},
\code{\link{tercilePlot}()}
}
\author{
J. Bedia
}
\concept{visualization functions}
