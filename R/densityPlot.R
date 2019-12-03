#     densityPlot.R Lattice plot methods for climatological grids
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



#' @title Density plots for climatological series 
#' @description A wrapper for the lattice density plot methods for grid and station data.
#' @param ... Input grids (or station data). It can be a list of grids too.
#' @param group.index Character or numeric passed to argument group in density plot.
#' @param h.lines Numeric sequence indicating the position of dashed horizontal lines.
#' @param v.lines Numeric sequence indicating the position of dashed vertical lines. The default (NULL),
#' plots vertical lines in the mean value of the probability density function. 
#' @param lonLim Vector of length = 2, with minimum and maximum longitude coordinates, 
#' in decimal degrees, of the bounding box selected. If NULL (default), the whole 
#' longitudinal range is selected 
#' @param latLim Same as lonLim, but for the selection of the latitudinal range.
#' @param dplot.custom List of arguments as passed to \code{\link[lattice]{densityplot}}. Arguments \code{panel} cannot 
#' be modified, thus, if specified, it will be ignored. 
#'  
#'  \strong{Controlling graphical parameters}
#'  
#'  Many different aspects of the plot can be controlled passing the relevant arguments to 
#'  \code{dplot}.
#'  
#' @return A lattice plot of class \dQuote{trellis}. 
#' @author M. Iturbide
#' @export
#' @import lattice 
#' @importFrom RColorBrewer brewer.pal
#' @import latticeExtra
#' @examples
densityPlot <- function(...,
                        h.lines = NULL,
                        v.lines = NULL,
                        group.index = NULL,
                        lonLim = NULL,
                        latLim = NULL,
                        dplot.custom = list()) {
  obj.list <- list(...)
  nlists <- length(obj.list)
  if (gridDepth(obj.list) > 3) obj.list <- unlist(obj.list, recursive = FALSE)
  obj.list <- lapply(obj.list, function(x) subsetGrid(x, lonLim = lonLim, latLim = latLim))
  obj.list <- lapply(obj.list, FUN = redim)
  mg <- FALSE
  if (is.null(group.index)) {
    if (isMultigrid(obj.list[[1]])) {
      mg <- TRUE
      vnames <- getVarNames(obj.list[[1]])
      group.index <- rep(vnames,length(obj.list))
      obj.list <- lapply(obj.list, function(x) { 
        lapply(getVarNames(x), FUN = function(xx) subsetGrid(x, var = xx))
      }) %>%  unlist(recursive = FALSE)
    } else {
      vnames <- group.index <- rep("", length(obj.list))
    }
    dplot.custom[["strip"]] <- TRUE
  }
  obj.list <- lapply(obj.list, FUN = redim)
  
  if (is.null(names(obj.list))) {
    if (mg) {
      names(obj.list) <- sapply(1:nlists, FUN = function(z) paste0("grid",z) %>% rep(length(obj.list)/nlists)) %>% as.vector()
    } else {
      nmes <- as.character(as.list(substitute(list(...)))[-1L])
      if (length(nmes) < length(obj.list)) nmes <- paste0(nmes, 1:length(obj.list))
      names(obj.list) <- nmes
    }
  }
  
  data <- lapply(obj.list, "[[", "Data")
  # bind to data frames
  df <- lapply(1:length(obj.list), function(x){
    df0 <- data.frame(as.vector(data[[x]]),
                      rep(names(data)[x], length(as.vector(data[[x]]))),
                      rep(x, length(as.vector(data[[x]]))),
                      rep(group.index[x], length(as.vector(data[[x]]))))
    colnames(df0) <- c("Value", "mini", "nini", "index")
    return(df0)
  })
  dff <- do.call("rbind", df)
  
  if (is.null(dplot.custom[["plot.points"]])) dplot.custom[["plot.points"]] <- FALSE
  if (is.null(dplot.custom[["col"]])) {
    dplot.custom[["auto.key"]] <- TRUE
  } else {
    cols <- rep(dplot.custom[["col"]],length(vnames))
    cols <- sapply(1:length(dplot.custom[["col"]]), FUN = function(z) rep(dplot.custom[["col"]][z],length(vnames))) %>% as.vector()
    dplot.custom[["col"]] <- cols
  }
  if (is.null(dplot.custom[["ylim"]])) dplot.custom[["ylim"]] <- c(0,1)
  if (is.null(dplot.custom[["lwd"]])) dplot.custom[["lwd"]] <- 1
  if (is.null(dplot.custom[["layout"]])) dplot.custom[["layout"]] <- c(1, length(unique(group.index)))
  if (is.null(dplot.custom[["xlab"]])) dplot.custom[["xlab"]] <- ""
  dplot.custom[["x"]] <- mini ~ Value | index
  dplot.custom[["data"]] <- dff
  dplot.custom[["groups"]] <- dff$nini
  
  ylim <- dplot.custom[["ylim"]]
  digs <- 0
  if ((max(ylim) - min(ylim)) < 1) {
    digs <- 2
  } else if ((max(ylim) - min(ylim)) < 5) {
    digs <- 1
  }
  if (is.null(h.lines)) h.lines <- seq(ylim[1], ylim[2],round((ylim[2] - ylim[1])/10, digits = digs))
  if (is.null(v.lines)) v.lines <- sapply(unique(dff$nini), FUN = function(z) mean(dff$Value[which(dff$nini == z)],na.rm = TRUE))
  
  dplot.custom[["panel"]] <- function(...) {
    panel.superpose(..., panel.groups = function(...) {
      panel.densityplot(...)
    })
    panel.abline(h = h.lines,
                 col = "gray65", lwd = 0.5, lty = 2)
    # panel.text(0,0,labels=MyText[panel.number()])
    # panel.abline(v = v.lines,
    #              col = dplot.custom[["col"]], lwd = 0.7, lty = 2)
  }
  
  output <- do.call("densityplot", dplot.custom) 
  return(output)
}

