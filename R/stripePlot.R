#     stripePlot.R Lattice plot methods for grids
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



#' @title Lattice plot methods for producing stripe plots 
#' @description A wrapper for the lattice (trellis) plot methods for grid and station data.
#' @param grid Input grid (or station data), ideally multimember and with time dimension > 1.
#' @param aggr.spatial list containing the function and the related arguments to perform spatial 
#' aggregation. Default is \code{list(FUN = mean, na.rm = TRUE)}.
#' @param color.theme A character string indicating the color theme to use in the map. 
#' Valid values are those available in the \code{\link{RColorBrewer}} themes. Additionally,
#' the \code{"jet.colors"} palette can be used (the rainbow colors, in general not advised, though),
#' for backwards compatibility. Default to the diverging, colorblind-friendly \code{"RdYlBu"} palette.
#' NOTE: the \code{color.theme} argument will be overriden if the \code{col.regions} option from \code{levelplot} is used.
#' @param rev.colors Should the chosen color theme be reversed? (default to \code{FALSE},
#' leaving the palette \dQuote{as is}).
#' @param xlabels Character string of the labels to be shown in the x axis (Default to NULL)
#' @param ylabels Character string of the labels to be shown in the y axis (Default to NULL)
#' @param ... Additional arguments as passed to function \code{levelplot}. 
#' @details The function applies the \code{\link[lattice]{levelplot}} method after computing spatial aggregation 
#' (parameter \code{aggr.spatial}) to the imput grids (or station data).
#'  
#' This function is appropriate for multimember grids, each row represents a member.
#'  
#'  \strong{Controlling graphical parameters}
#'  
#'  Many different aspects of the plot can be controlled passing the relevant arguments to 
#'  \code{levelplot}. Fine control of graphical parameters for the trellis display can
#'  be also controlled using \code{\link[lattice]{trellis.par.set}}.
#'  
#'   
#
#' @return A lattice plot of class \dQuote{trellis}. 
#' 
#'    
#'@author M. Iturbide
#'@export
#'@import lattice 
#'@importFrom grDevices col2rgb rgb colors
#'@import latticeExtra
#'@importFrom transformeR aggregateGrid
#'@importFrom RColorBrewer brewer.pal
#' @examples \donttest{
#' require(climate4R.datasets)
#' data("CFS_Iberia_pr")
#' stripePlot(CFS_Iberia_pr)
#' }


stripePlot <- function(grid, 
                       aggr.spatial = list(FUN = "mean", na.rm = TRUE),
                       color.theme = "RdBu",
                       rev.colors = FALSE,
                       xlabels = NULL,
                       ylabels = NULL,
                       ...){ 
  lp <- list(...)
  reg.mean <- aggregateGrid(grid, aggr.spatial = aggr.spatial) 
  mat <- t(reg.mean$Data)
  if (is.null(xlabels)) { 
    rownames(mat) <- substring(grid$Dates$start, first = 1, last = 4)
  } else {
    if(length(xlabels) != nrow(mat)) stop("xlabels must have the same length as the time dimension in grid")
    rownames(mat) <- xlabels    
  }
  if (is.null(ylabels)) { 
    colnames(mat) <- grid$Members
  } else {
    if(length(ylabels) != ncol(mat)) stop("ylabels must have the same length as the member dimension in grid")
    colnames(mat) <- ylabels
  }
  # PLOT
  col <- brewer.pal(n = 9, name = color.theme)
  if (rev.colors) col <- rev(col)
  lp[["x"]] <- mat
  if (is.null(lp[["col.regions"]])) lp[["col.regions"]] <- colorRampPalette(col)
  if (is.null(lp[["xlab"]])) lp[["xlab"]] <- ""
  if (is.null(lp[["ylab"]])) lp[["ylab"]] <- ""
  if(nrow(mat) > 100) { xscales <- seq(1, nrow(mat), 100)
  } else {
    xscales <- seq(1, nrow(mat))
  }
  if (is.null(lp[["scales"]])) lp[["scales"]] <- list(x = list(at = xscales, rot = 45, cex = 0.7), y = list(rot = 0, cex = 0.7))
  #if (is.null(lp[["scale"]])) lp[["scale"]] <- list(alternating = 1)
  if (is.null(lp[["colorkey"]])) lp[["colorkey"]] <- list(width = 0.9, labels = list(cex = 0.7))
  if (is.null(lp[["aspect"]])) lp[["aspect"]] <-  "fill"
  do.call("levelplot", lp)
}

#end
