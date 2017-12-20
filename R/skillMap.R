#     skillMap.R Skill maps for seasonal forecasts 
#
#     Copyright (C) 2017 Santander Meteorology Group (http://www.meteo.unican.es)
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title A wrapper of \code{spatialPlot} for the creation of verification maps for seasonal forecast systems
#' 
#' @description A wrapper of \code{spatialPlot} for the creation of verification maps for seasonal forecast systems.
#'  It provides a convenient interface for \code{\link{map.stippling}}
#' 
#' 
#' @param easyVeriGrid A climatological grid with a verification output. See details
#' @param stippling A key-value list of arguments passed to \code{map.stippling}: 
#' \code{threshold} and \code{condition}. Ignored by default, returning a map without stippling.
#' @param stippling.point.options Default to \code{NULL}. Further graphical arguments passed to points 
#' (e.g. \code{cex}, \code{pch} etc.)
#' @param backdrop.theme See \code{\link{spatialPlot}}
#' @param title Title of the plot
#' @param ... Further graphical options passed to \code{\link{spatialPlot}}.
#' 
#' @details The function applies the \code{\link{spatialPlot}} function, that in turn uses \code{lattice-methods}.
#' 
#' \strong{Graphical options}
#' 
#' Some examples of specific map graphical options are available in the help of function \code{\link[sp]{spplot}}.
#'  In addtion, fine-tuning of the resulting plots can be obtained using the arguments of \pkg{lattice} plots. For
#'  an overview, see the help of function \code{\link[lattice]{xyplot}}.
#' 
#' @seealso The bridging function \code{\link[transformeR]{easyVeri2grid}} from package \pkg{transformeR} allows for 
#' the conversion of verification outputs from package \pkg{easyVerification} to the \code{climate4R} data structure
#' used by the function.
#'  
#'  Many different aspects of the plot can be controlled passing the relevant arguments to 
#'  \code{\link[sp]{spplot}}.
#'  
#' @return A lattice plot of class \dQuote{trellis}. 
#' 
#' @author J. Bedia
#' @export
#' @examples \dontrun{
#' 
#' # The package 'easyVerification' will be used to calculate the RPSS:
#'   
#' require(easyVerification)
#' require(transformeR)
#' # First of all, a data subset is done, considering a spatial domain centered on the North Atlantic:
#' tas.cfs2 <- subsetGrid(tas.cfs, lonLim = c(-100, 40), latLim = c(-5, 75))
#' # The same is done with the reanalysis dataset:
#' tas.ncep2 <- subsetGrid(tas.ncep, lonLim = c(-100, 40), latLim = c(-5, 75))
#' # In the next step, the reanalysis data are interpolated to the hindcast grid:
#' tas.ncep2.int <- interpGrid(tas.ncep2, new.coordinates = getGrid(tas.cfs2), method = "nearest")
#' # We compute the Ranked Probabiloity SKill Score using veriApply, and a cross-validation strategy:
#' ev <- easyVerification::veriApply(verifun = "EnsRpss",
#'                                   fcst = tas.cfs2$Data,
#'                                   obs = tas.ncep2.int$Data,
#'                                   prob = 1:2/3,
#'                                   tdim = 2,
#'                                   ensdim = 1,
#'                                   parallel = TRUE,
#'                                   ncpus = 3, 
#'                                   strategy = "crossval")
#' # The bridging function 'easyVeri2grid' converts the object returned by 'veriApply' 
#' #    to a 'climate4R' climatological grid: 
#' 
#' easyVeriGrid <- easyVeri2grid(ev$skillscore,
#'                               obs.grid = tas.ncep2.int,
#'                               verifun = "EnsRpss")
#' 
#' # A basic RPSS map, using the spatialPlot defaults:
#' skillMap(easyVeriGrid = easyVeriGrid, backdrop.theme = "coastline")
#' 
#' # Stippling. Mark significant RPSS values at the 95% ci
#' thresh <- ev$skillscore.sd*qnorm(0.95)
#' 
#' skillMap(easyVeriGrid = easyVeriGrid,
#'          stippling = list(threshold = th, condition = "GT"), # GT = greater than
#'          stippling.point.options = list(pch = 19, cex = .2, col = "black"),
#'          backdrop.theme = "coastline")
#' 
#' # Further customization: For instance a more elaborated title, colorblind-friendly palette etc.:
#' 
#' cb.colors <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")))
#' skillMap(easyVeriGrid = easyVeriGrid,
#'          stippling = list(threshold = th, condition = "GT"),
#'          stippling.point.options = list(pch = 19, cex = .2, col = "black"),
#'          backdrop.theme = "coastline",
#'          at = seq(-0.7, 0.7, .05),
#'          set.max = 0.7, set.min = -0.7,
#'          scales = list(draw = TRUE, alternating = 3),
#'          colorkey = list(space = "bottom"),
#'          col.regions = cb.colors(51),
#'          title = paste("Ranked Probability Skill Score",
#'                        "Forecasting System: CFSv2 - 24 members", 
#'                        "November Initializations",
#'                        "Observing System: NCEP/NCAR Reanalysis 1",
#'                        "t2m DJF 1983-2010", sep = "\n")
#' )
#' }

skillMap <- function(easyVeriGrid,
                     stippling = list(threshold = NULL, condition = NULL),
                     stippling.point.options = NULL,
                     backdrop.theme = NULL,
                     title = NULL,
                     ...) {
  arg.list <- list(...)
  arg.list[["grid"]] <- easyVeriGrid
  if (!is.null(stippling$threshold)) {
    stippling[["clim"]] <- easyVeriGrid
    if (!is.null(stippling.point.options)) {
      stippling <- c(stippling, stippling.point.options)
    }
    stip.list <- list(do.call("map.stippling", stippling))
    if ("sp.layout" %in% names(arg.list)) {
      arg.list[["sp.layout"]] <- c(arg.list[["sp.layout"]], stip.list)
    } else {
      arg.list[["sp.layout"]] <- stip.list
    }
  }
  if (!is.null(backdrop.theme)) {
    arg.list[["backdrop.theme"]] <- backdrop.theme
  }
  if (!is.null(title)) {
    arg.list[["main"]] <- list(label = title,
                               cex = .9,
                               col = "blue",
                               font = 1,
                               just = "left", x = 0.05)
  }
  map <- do.call("spatialPlot", arg.list)
  print(map)
}
