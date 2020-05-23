#' @title Land borders
#' @description Add land borders to a map
#' @param ... Graphical parameters passed to \code{\link{lines}}.
#' @return Draws a simplied land border areas as lines onto the map
#' @details The function loads a built-in world segments dataset created ad hoc to avoid dependencies on other packages (i.e. 'maps').
#' Geographical lonlat coordinates in wgs84.
#' This function was formerly in transformeR v<2.0.0.
#' 
#' @source Postprocessed from the original shapefile from Natural Earth (http://www.naturalearthdata.com/downloads/110m-physical-vectors/)
#' @author J. Bedia
#' @keywords internal
#' @importFrom graphics lines
#' @export

draw.world.lines <- function(...) {
    load(system.file(package = "visualizeR","wrl.Rda"), envir = environment())
    for (i in 1:length(node.list)) {
        lines(node.list[[i]][,1], node.list[[i]][,2], ...)            
    }
}
