#' @title Map hatching
#' @description Utility for map hatching, following IPCC convention or other user-defined specifications.
#' @param clim A climate4R \code{\link[transformeR]{climatology}}
#' @param threshold Pixel threshold value to differentiate hatched/unhatched areas
#' @param condition Inequality to be applied as condition. Accepted values are \code{"GT", "GE", "LT", "LE"}, meaning respectively 
#' (strictly) greater than, greater or equal than, (strictly) lower than and lower or equal than.
#' @param invert Default to \code{FALSE} and unused. For convenience, this flag inverts the selection
#' so the specified map values are NOT hatched, and the other way round.
#' @param density Density of the hatching pattern. This parameter is related to the spatial resolution of the input grid. Default to 4,
#'  so hatching lines are sepparated in the X axis by a distance of four times the X-resolution of the input grid. Passed to \code{times}
#'  argument in function \code{\link[transformeR]{upscaleGrid}}.
#' @param upscaling.aggr.fun Function performing the spatial aggregation when upscaling according to parameter \code{density}. 
#' The default uses the average value.
#' @param angle Angle of the hatching lines w.r.t. the horizontal line, as character string. Possible values are \code{"-45"} (default) and \code{"45"}
#'  degrees. 
#' @param ... Graphical parameters affecting the appearance of the hatching lines. See details and examples.
#' @return A graphical parameter list to be superposed onto the climatological map via \code{sp.layout}
#' @note The function assumes that a valid CRS is in use, so no mismatches between the hatched area and
#'  the base climatology happen. See \code{\link[geoprocessoR]{projectGrid}} for details
#' @details 
#' 
#' The function follows the same philosophy as other graphical helpers implemented in \pkg{visualizeR}, namely 
#' \code{\link[visualizeR]{map.stippling}} and \code{\link[visualizeR]{map.lines}}, thus allowing for a flexible definition
#' of graphical layers to be added onto a climatological map using the \pkg{lattice} plot methods used by \code{\link[visualizeR]{spatialPlot}}.
#' 
#' Several details on its application are illustrated in the examples below.
#' 
#' @examples 
#' 
#' library(visualizeR)
#' data(tas.ncep)
#' 
#' # The climatology of DJF 2m air temperature (NCEP reanalysis 1983-2010) is calculated
#' # as example:
#' 
#' clim <- climatology(tas.ncep)
#' spatialPlot(clim, backdrop.theme = "coastline", rev.colors = TRUE)
#' 
#' # Basic usage
#' # In this example, the hatching will apply over all world areas whose temperature
#' # is above 27.5:
#' 
#' l1 <- map.hatching(clim, threshold = 25.5, condition = "GT")
#' spatialPlot(clim, backdrop.theme = "coastline", rev.colors = TRUE, sp.layout = list(l1))
#' 
#' # Inverted selection:
#' 
#' l2 <- map.hatching(clim, threshold = 25.5, condition = "GT", invert = TRUE)
#' spatialPlot(clim, backdrop.theme = "coastline", rev.colors = TRUE,
#'             sp.layout = list(l2), invert = TRUE)
#' 
#' # Hatching density
#' # The argument 'density' controls the line density. It is proportional to the spatial
#' # resolution of the underlying climatology. For instance, density = 4 (the default value) 
#' # means that lines are horizontally sepparated by a distance of four pixels. 
#' 
#' l3 <- map.hatching(clim, threshold = 25.5, condition = "GT", density = 2)
#' spatialPlot(clim, backdrop.theme = "coastline", rev.colors = TRUE, sp.layout = list(l3))
#' 
#' # Hatching orientation
#' # The hatching lines ca be drawn following 45 instead of -45 degrees: 
#' 
#' l4 <- map.hatching(clim, threshold = 25.5, condition = "GT", density = 2, angle = "45")
#' spatialPlot(clim, backdrop.theme = "coastline", rev.colors = TRUE, sp.layout = list(l4))
#' 
#' # Controlling hatching lines appearance:
#' 
#' # These are typically related to line type (lty), line width (lwd), line color (col) and so on.
#' # These are to map.hatching via ellipsis:
#' 
#' l5 <- map.hatching(clim, threshold = 25.5, condition = "GT",
#'                    density = 2, angle = "45", lwd = 1.5, col = "green", lty = 2)
#' spatialPlot(clim, backdrop.theme = "coastline", rev.colors = TRUE, sp.layout = list(l5))
#' 
#' @importFrom sp SpatialLines SpatialPolygonsDataFrame Lines Line
#' @importFrom transformeR upscaleGrid grid2sp getGrid
#' 
#' @export
#'
#' @author J. Bedia, M. Iturbide

map.hatching <- function(clim, threshold = 0.05, condition = "LT",
                         invert = FALSE,
                         density = 4,
                         angle = "-45",
                         upscaling.aggr.fun = list(FUN = "mean", na.rm = TRUE),
                         ...) {
  if (!("climatology:fun" %in% names(attributes(clim$Data)))) {
    stop("Input grid was not recognized as a climatology")
  }
  stopifnot(is.logical(invert))
  condition <- match.arg(condition, choices = c("GT", "GE", "LT", "LE"))
  angle <- match.arg(angle, choices = c("-45", "45", "90", "0"))
  ineq <- if (isTRUE(invert)) {
    switch(condition,
           "GT" = ">",
           "GE" = ">=",
           "LT" = "<",
           "LE" = "<=")
  } else {
    switch(condition,
           "GT" = "<=",
           "GE" = "<",
           "LT" = ">=",
           "LE" = ">")
  }
  arg.list <- list(...)
  # Binary data
  eval(parse(text = paste("clim$Data[which(clim$Data", ineq, "threshold)] <- NA")))
  
  # Upscaling
  # refgrid <- getGrid(clim)
  # newresX <- attr(refgrid, "resX") * density
  # newresY <- attr(refgrid, "resY") * density
  # x.list <- seq(refgrid$x[1], refgrid$x[2], by = newresX)
  # y.list <- seq(refgrid$y[1], refgrid$y[2], by = newresY)
  # clim <- suppressMessages(interpGrid(clim,
  #                                     new.coordinates = list(x = x.list, y = y.list),
  #                                     method = "nearest"))
  if (density > 1) clim1 <- upscaleGrid(clim, times = density, aggr.fun = upscaling.aggr.fun)
  if (all(is.na(clim$Data))) {
    message("NOTE: Empty selection. No hatching will be used")
    l1 <- list("sp.points", sp::SpatialPoints(coords = matrix(c(0,0), ncol = 2)), cex = 0)
  } else {
    sp.polys <- as(grid2sp(clim), "SpatialPolygonsDataFrame")
    # Hatching lines
    if (angle == "-45") {
      a <- 2
      b <- 4
    } else if (angle == "45") {
      a <- 1
      b <- 3
    } else if (angle == "90") {
      a <- 1.5
      b <- 3.5
    } else if (angle == "0") {
      a <- 2.5
      b <- 4.5
    }
    coords <- lapply(sp.polys@polygons, function(x) {
      if (angle == "0" | angle == "90") {
          bat <- x@Polygons[[1]]@coords[c(floor(a), floor(b)),]
          bi <- x@Polygons[[1]]@coords[c(ceiling(a), ceiling(b)),]
          k <- (bat + bi)/2
        } else {
          k <- x@Polygons[[1]]@coords[c(a, b),]
      }
      sp::Line(k)
    })
    hatchlines <- sp::SpatialLines(list(sp::Lines(coords, ID = "hatch")))
    l1 <- c(arg.list, hatchlines)
  }
  return(l1)
}
