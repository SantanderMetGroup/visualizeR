% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bubblePlot.R
\name{bubblePlot}
\alias{bubblePlot}
\title{Bubble plot for visualization of forecast skill of seasonal climate predictions.}
\usage{
bubblePlot(
  hindcast,
  obs,
  forecast = NULL,
  year.target = NULL,
  detrend = FALSE,
  score = TRUE,
  size.as.probability = TRUE,
  bubble.size = 1,
  score.range = NULL,
  piechart = FALSE,
  subtitle = NULL,
  t.colors = NULL,
  pie.border = NULL,
  pch.neg.score = NULL,
  pch.obs.constant = NULL,
  pch.data.nan = NULL
)
}
\arguments{
\item{hindcast}{A multi-member list with the hindcast for verification. See details.}

\item{obs}{List with the benchmarking observations for forecast verification.}

\item{forecast}{A multi-member list with the forecasts. Default is NULL.}

\item{year.target}{Year within the hindcast period considered as forecast. Default is NULL.}

\item{detrend}{Logical indicating if the data should be linear detrended. Default is FALSE.}

\item{score}{Logical indicating if the relative operating characteristic skill score (ROCSS) should be included. See 
details. Default is TRUE.}

\item{size.as.probability}{Logical indicating if the tercile probabilities (magnitude proportional to bubble radius) 
are drawn in the plot. See details. Default is TRUE.}

\item{bubble.size}{Number for the bubble or pie size. bubble.size=1 by default.}

\item{score.range}{A vector of length two used to rescale the transparency of the bubbles.
For instance, a \code{score.range = c(0.5, 0.8)} will turn ROCSS values below 0.5 completely transparent,
 while values of 0.8 or will have minimum transparency (i.e., opaque). 
 The default to \code{NULL}, that will set a transparency range between 0 and 1.}

\item{piechart}{Logical flag indicating if pie charts should be plot instead of bubbles. Default is FALSE.}

\item{subtitle}{String to include a subtitle bellow the title. Default is NULL.}

\item{t.colors}{Three element vector representing the colors for the below, normal and above categories.
Default is t.colors=c("blue", "gold", "red")}

\item{pie.border}{Color for the pie border. Default is pie.border="gray"}

\item{pch.neg.score}{pch value to highlight the negative score values. Default is NULL. Not available for piecharts.}

\item{pch.obs.constant}{pch value to highlight those whose score cannot be computed due to constant obs 
conditions (e.g. always dry). Default is NULL.}

\item{pch.data.nan}{pch value to highlight those whose score cannot be computed due to time series with all NA values in 
the observations and/or models. If score=F, highlight those grids with all forecasts NaN.}
}
\description{
Bubble plot for visualization of forecast skill of seasonal climate predictions. It provides a
 spatially-explicit representation of the skill, resolution and reliability of a probabilistic predictive 
 system in a single map. 
 This function is prepared to plot the data sets loaded from the ECOMS User Data Gateway (ECOMS-UDG). See 
 the loadeR.ECOMS R package for more details (http://meteo.unican.es/trac/wiki/udg/ecoms/RPackage).
}
\details{
First daily data are averaged to obtain a single seasonal value. The corresponding terciles 
 for each ensemble member are then computed for the hindcast period. Thus, each particular grid point, member and season,
 are categorized into three categories (above, between or below), according to their respective climatological 
 terciles. Then, a probabilistic forecast is computed year by year by considering the number of members falling 
 within each category. For instance, probabilities below 1/3 are very low, indicating that a minority of the members 
 falls in the tercile. Conversely, probabilities above 2/3 indicate a high level of member agreement (more than 66\% of members
 falling in the same tercile). Probabilities are also computed for the forecast or the selected year. Color represents the tercile 
 with the highest probability for the forecast or selected year. The bubble size indicates the probability of that tercile. This 
 option is not plotted if the size.as.probability argument is FALSE.

 Finally, the ROC Skill Score (ROCSS) is computed for the hindcast period. For each tercile, it provides a quantitative measure 
 of the forecast skill, and it is commonly used to evaluate the performance of probabilistic systems (Joliffe and Stephenson 2003). 
 The value of this score ranges from 1 (perfect forecast system) to -1 (perfectly bad forecast system). A value zero indicates no 
 skill compared with a random prediction. The transparency of the bubble is associated to the ROCSS. By default only positive values
 are plotted if the score argument is TRUE. The target year is considered as forecast and it is not included in the computation of 
 the score (operational point of view).
}
\note{
The computation of climatological terciles requires a representative period to obtain meaningful results.
}
\examples{
\dontrun{
data(tas.cfs)
data(tas.cfs.operative.2016)
data(tas.ncep)
require(transformeR)
# Select spatial domain
tas.ncep2 <- subsetGrid(tas.ncep, lonLim = c(-80, -35), latLim = c(-12, 12))
tas.cfs2 <- subsetGrid(tas.cfs, lonLim = c(-80, -35), latLim = c(-12, 12))
tas.cfs.operative2.2016 <- subsetGrid(tas.cfs.operative.2016, 
                           lonLim = c(-80, -35), latLim = c(-12, 12))
# Interpolate
tas.ncep2.int <- interpGrid(tas.ncep2, getGrid(tas.cfs2))
# Bubble plot. Only colour of the bubble is plotted indicating the most likely tercile 
bubblePlot(hindcast = tas.cfs2, obs = tas.ncep2.int, forecast = tas.cfs.operative2.2016,
           bubble.size = 1.5, size.as.probability = FALSE, score = FALSE)
# Bubble plot. Added size of the bubble indicating the probability of the most likely tercile 
bubblePlot(hindcast = tas.cfs2, obs = tas.ncep2.int, forecast = tas.cfs.operative2.2016,
           bubble.size = 1.5, score = FALSE)
# Bubble plot. Added transparency of the bubble indicating the ROC skill score (ROCSS)
bubblePlot(hindcast = tas.cfs2, obs = tas.ncep2.int, forecast = tas.cfs.operative2.2016,
           bubble.size = 1.5)
# 3-piece pie chart.
bubblePlot(hindcast = tas.cfs2, obs = tas.ncep2.int, forecast = tas.cfs.operative2.2016,
           bubble.size = 1, piechart = TRUE)
} 

}
\references{
Jolliffe, I. T. and Stephenson, D. B. 2003. Forecast Verification: A Practitioner's Guide in Atmospheric 
 Science, Wiley, NY.
 
 Slingsby A., Lowe R., Dykes J., Stephenson D. B., Wood J. and Jupp T. E. 2009. A pilot study for the collaborative 
 development of new ways of visualising seasonal climate forecasts. Proc. 17th Annu. Conf. of GIS Research UK, 
 Durham, UK, 1-3 April 2009.
}
\seealso{
Other visualization functions: 
\code{\link{cascadePlot}()},
\code{\link{climagram}()},
\code{\link{reliabilityCategories}()},
\code{\link{spreadPlot}()},
\code{\link{tercileBarplot}()},
\code{\link{tercilePlot}()}
}
\author{
M.D. Frias \email{mariadolores.frias@unican.es} and J. Fernandez based on the original diagram 
 conceived by Slingsby et al (2009).
}
\concept{visualization functions}
