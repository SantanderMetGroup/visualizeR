#     violinPlot.R Lattice plot methods for climatological grids
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



#' @title Lattice plot methods for climatological series 
#' @description A wrapper for the lattice (trellis) plot methods for grid and station data.
#' @param ... Input grids (or station data). It can be a list of grids too.
#' @param group.index Character or numeric passed to argument group in bwplot.
#' @param violin (default is TRUE). If FALSE, boxplots are returned.
#' @param fill Logical indicating if the violins/boxes are filled with color. if TRUE, arguments
#' \code{color.fun}, \code{color.theme}, \code{color.cuts} and \code{rev.colors} are used.
#' Alternatively a vector with colors can be passed, in which case the mentioned arguments
#' are ignored.
#' @param color.fun list containing the function and the related arguments to perform spatial 
#' aggregation. The resulting values are used to fill with color the violins.
#'  Default is \code{list(FUN = mean, na.rm = TRUE)}.
#' @param color.theme A character string indicating the color theme to use in the map. 
#' Valid values are those available in the \code{\link{RColorBrewer}} themes. Additionally,
#' the \code{"jet.colors"} palette can be used (the rainbow colors, in general not advised, though),
#' for backwards compatibility. Default to the diverging, colorblind-friendly \code{"RdYlBu"} palette.
#' @param color.cuts Numeric sequence indicating the color cuts.
#' @param rev.colors Default to FALSE. If TRUE the reversed version of the color palette
#' is used.
#' @param box.col Character or numeric of the color to be given to the box borders.
#' @param h.lines Numeric sequence indicating the position of dashed horizontal lines.
#' @param v.lines Logical for drawing vertical lines (Default is FALSE). 
#' @param lonLim Vector of length = 2, with minimum and maximum longitude coordinates, 
#' in decimal degrees, of the bounding box selected. If NULL (default), the whole 
#' longitudinal range is selected 
#' @param latLim Same as lonLim, but for the selection of the latitudinal range.
#' @param bwplot.custom List of arguments as passed to \code{\link[lattice]{bwplot}}. Arguments \code{panel} cannot 
#' be modified, thus, if specified, it will be ignored. 
#'  
#'  \strong{Controlling graphical parameters}
#'  
#'  Many different aspects of the plot can be controlled passing the relevant arguments to 
#'  \code{bwplot}. Fine control of graphical parameters for the trellis display can
#'  be also controlled using \code{\link[lattice]{trellis.par.set}}.
#'  
#'   
#'   
#
#' @return A lattice plot of class \dQuote{trellis}. 
#' 
#'    
#' @author M. Iturbide
#' @export
#' @import lattice 
#' @importFrom RColorBrewer brewer.pal
#' @importFrom transformeR gridDepth
#' @importFrom stats sd
#' @import latticeExtra
#' @examples \donttest{
#' require(climate4R.datasets)
#' data("EOBS_Iberia_tas")
#' data("EOBS_Iberia_pr")
#' data("CORDEX_Iberia_pr")
#' violinPlot("one" = climatology(EOBS_Iberia_pr),
#'            "two" = climatology(EOBS_Iberia_tas),
#'            "three" = climatology(CORDEX_Iberia_pr),
#'             h.lines = seq(0, 15, 5),
#'             color.cuts = seq(0, 3, 0.2),
#'             bwplot.custom = list(ylim = c(0, 20),
#'                                  ylab = "pr and tas"))
#' 
#' ## With grouping:
#' data("EOBS_Iberia_tas")
#' data("EOBS_Iberia_pr")
#' data("CORDEX_Iberia_pr")
#' data("CORDEX_Iberia_tas")
#' violinPlot("pr" = climatology(EOBS_Iberia_pr),
#'            "tas" = climatology(EOBS_Iberia_tas),
#'            "pr" = climatology(CORDEX_Iberia_pr),
#'            "tas" = climatology(CORDEX_Iberia_tas),
#'             group.index = c("Measure1", "Measure1", "Measure2", "Measure2"),
#'             h.lines = seq(0, 15, 5),
#'             v.lines = TRUE,
#'             color.cuts = seq(0, 3, 0.2),
#'             bwplot.custom = list(ylim = c(0, 20),
#'                                  ylab = "pr and tas",
#'                                  as.table = TRUE))
#' 
#' ## Boxplot:
#' data("EOBS_Iberia_tas")
#' data("EOBS_Iberia_pr")
#' data("CORDEX_Iberia_pr")
#' data("CORDEX_Iberia_tas")
#' a <- violinPlot("pr" = climatology(EOBS_Iberia_pr),
#'                 "tas" = climatology(EOBS_Iberia_tas),
#'                 violin = FALSE,
#'                 fill = FALSE,
#'                 h.lines = seq(0, 15, 5),
#'                 v.lines = TRUE,
#'                 box.col = "blue",
#'                 bwplot.custom = list(ylim = c(0, 20),
#'                                      ylab = "pr and tas",
#'                                      as.table = TRUE,
#'                                      do.out = FALSE))
#' b <- violinPlot("pr" = climatology(CORDEX_Iberia_pr),
#'                 "tas" = climatology(CORDEX_Iberia_tas),
#'                 violin = FALSE,
#'                 fill = FALSE,
#'                 h.lines = seq(0, 15, 5),
#'                 v.lines = TRUE,
#'                 box.col = "green",
#'                 bwplot.custom = list(ylim = c(0, 20),
#'                                      ylab = "pr and tas",
#'                                      as.table = TRUE,
#'                                      do.out = FALSE))
#' a + b
#' }


violinPlot <- function(..., 
                       group.index = NULL,
                       violin = TRUE,
                       fill = TRUE,
                       color.fun = list(FUN = mean, na.rm = TRUE),
                       color.theme = "RdYlBu",
                       color.cuts = NULL,
                       rev.colors = FALSE,
                       box.col = NULL,
                       h.lines = NULL,
                       v.lines = FALSE,
                       lonLim = NULL,
                       latLim = NULL,
                       bwplot.custom = list()) {
  obj.list <- list(...)
  if (gridDepth(obj.list) > 3) obj.list <- unlist(obj.list, recursive = FALSE)
  if (is.null(names(obj.list))) {
    nmes <- as.character(as.list(substitute(list(...)))[-1L])
    if (length(nmes) < length(obj.list)) nmes <- paste0(nmes, 1:length(obj.list))
    names(obj.list) <- nmes
  }
  # timeshape <- unlist(lapply(lapply(obj.list, redim), function(x) getShape(x)[["time"]]))
  # if (any(timeshape != 1L)) stop("Time dimension length > 1. Use a function that aggregates time dimension first (e.g. f climatology)")
  obj.list <- lapply(obj.list, function(x) subsetGrid(x, lonLim = lonLim, latLim = latLim))
  obj.list <- lapply(obj.list, FUN = redim)
  if (is.null(group.index)) {
    group.index <- rep("", length(obj.list))
    bwplot.custom[["strip"]] <- FALSE
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
  # define graphical parameters
  ylim <- round(range(unlist(data), na.rm = TRUE), digits = 2)
  ## Colorbar 
  # spatial aggregation
  color.fun[["MARGIN"]] <- c(1,2)
  color.data <- unlist(lapply(1:length(obj.list), function(x){
    color.fun[["X"]] <- obj.list[[x]]$Data
    do.call("apply", color.fun)
  }))
  if (color.theme == "jet.colors") {
    coltheme <- c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
  } else {
    coltheme <- brewer.pal(name = color.theme, n = brewer.pal.info[color.theme, ]$maxcolors)
  }
  if (isTRUE(rev.colors)) coltheme <- rev(coltheme)
  colorpal <- colorRampPalette(coltheme)
  if (is.null(color.cuts)) color.cuts <- seq(min(color.data), max(color.data), (max(color.data) - min(color.data))/10)
  cols.full <- colorpal(length(color.cuts))
  cols <- cols.full[sapply(color.data, function(x) which.min(abs(color.cuts - x)))]
  #bwplot.custom
  if (is.null(box.col)) box.col <- "black"
  bwplot.custom[["par.settings"]][["box.umbrella"]] <-  list(col = box.col)
  bwplot.custom[["par.settings"]][["box.dot"]] <-  list(col = box.col) 
  bwplot.custom[["par.settings"]][["box.rectangle"]] <-  list(col = box.col) 
  if (is.null(bwplot.custom[["x"]])) bwplot.custom[["x"]] <- Value ~ mini | index
  if (is.null(bwplot.custom[["ylim"]])) bwplot.custom[["ylim"]] <- ylim
  if (is.null(bwplot.custom[["horizontal"]])) bwplot.custom[["horizontal"]] <- FALSE
  if (is.null(bwplot.custom[["lwd"]])) bwplot.custom[["lwd"]] <- 1
  if (is.null(bwplot.custom[["ylab"]])) bwplot.custom[["ylab"]] <- ""
  if (is.null(bwplot.custom[["layout"]])) bwplot.custom[["layout"]] <- c(1, length(unique(group.index)))
  ylim <- bwplot.custom[["ylim"]]
  digs <- 0
  if ((max(ylim) - min(ylim)) < 1) {
    digs <- 2
  } else if ((max(ylim) - min(ylim)) < 5) {
    digs <- 1
  }
  # if (is.null(bwplot.custom[["scales"]])) bwplot.custom[["scales"]] <- list(x = list(at = seq(xlim[1], xlim[2],(xlim[2] - xlim[1])/10),
  #                                                                                    labels = seq(xlim[1], xlim[2],(xlim[2] - xlim[1])/10), rot = 45),
  #                                                                           y = list(at = seq(ylim[1], ylim[2],round((ylim[2] - ylim[1])/10, digits = digs)),
  #                                                                                    labels = seq(ylim[1], ylim[2],round((ylim[2] - ylim[1])/10, digits = digs))),
  #                                                                           cex = .6, col = "black")
  if (is.null(h.lines)) h.lines <- seq(ylim[1], ylim[2],round((ylim[2] - ylim[1])/10, digits = digs))
  if (is.null(bwplot.custom[["key"]]) & isTRUE(violin)) bwplot.custom[["key"]] <- list(space = "right", rectangles = list(col = rev(cols.full), border = FALSE), 
                                                                      text = list(as.character(rev(round(color.cuts, digits = 2))), cex = .8))
  bwplot.custom[["data"]] <- dff
  bwplot.custom[["groups"]] <- as.factor(dff$nini)
  if (is.logical(fill)) {
    if (isFALSE(fill)) cols <- rgb(0,0,0,0) 
  } else {
    cols <- fill
  }
  if (violin) {
    bwplot.custom[["col"]] <- cols
    bwplot.custom[["panel"]] <- function(...) {
      panel.superpose(...)
      panel.abline(h = h.lines,
                   col = "gray65", lwd = 0.5, lty = 2)
      if (isTRUE(v.lines)) panel.abline(v = 1:length(obj.list),
                                        col = "gray65", lwd = 0.5, lty = 2)
      
    }
    bwplot.custom[["panel.groups"]] <- panel.violin
  } else {
    bwplot.custom[["fill"]] <- cols
    bwplot.custom[["col"]] <- box.col
    bwplot.custom[["panel"]] <- function(...) {
      panel.superpose(..., panel.groups = function(...) {
        panel.bwplot(..., stats = nboxplot.stats)}
      )
      panel.abline(h = h.lines,
                   col = "gray65", lwd = 0.5, lty = 2)
      if (isTRUE(v.lines)) panel.abline(v = 1:length(obj.list),
                                        col = "gray65", lwd = 0.5, lty = 2)
      # panel.bwplot(..., stats = nboxplot.stats)
    }
  }
  # crate trellis object
  output <- do.call("bwplot", bwplot.custom) 
  return(output)
}





#' @title boxplot.stats with set probs
#' @description boxplot.stats with set probs
#' @param x numeric vector
#' @param coef see boxplot.stats
#' @param do.conf see boxplot.stats
#' @param do.out see boxplot.stats
#' @param probs numeric with probabilites between 0 and 1
#' @author M. Iturbide


nboxplot.stats <- function (x, coef = 1.5, do.conf = TRUE, do.out = FALSE, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)) 
{
  if (coef < 0) 
    stop("'coef' must not be negative")
  nna <- !is.na(x)
  n <- sum(nna)
  stats <- unname(quantile(x, probs=probs, na.rm = TRUE))
  iqr <- diff(stats[c(2, 4)])
  if (coef == 0) 
    do.out <- FALSE
  else {
    out <- if (!is.na(iqr)) {
      x < (stats[2L] - coef * iqr) | x > (stats[4L] + coef * 
                                            iqr)
    }
    else !is.finite(x)
    if (any(out[nna], na.rm = TRUE)) 
      stats <- range(x[!out], na.rm = TRUE)
  }
  stats <- unname(quantile(x, probs=probs, na.rm = TRUE))
  conf <- if (do.conf) 
    stats[3L] + c(-1.58, 1.58) * iqr/sqrt(n)
  l <- list(stats = stats, n = n, conf = conf, out = if (do.out) x[out & 
                                                                     nna] else numeric())
  l[["out"]] <- as.integer(l[["out"]])
  l
}
