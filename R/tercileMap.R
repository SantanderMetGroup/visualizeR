#     temporalPlot.R Lattice plot methods for climatological grids
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



#' @title Tercile map for visualization of forecast skill of seasonal climate predictions.
#' 
#' @description Tercile map for the visualization of forecast skill of seasonal climate predictions.
#'  This function is prepared to plot the data sets loaded from the ECOMS User Data Gateway (ECOMS-UDG). See 
#'  the loadeR.ECOMS R package for more details (http://meteo.unican.es/trac/wiki/udg/ecoms/RPackage).
#' 
#' @param hindcast A multi-member list with the hindcast for verification. See details.
#' @param forecast A multi-member list with the forecasts. Default is NULL. 
#' @param ... Optional arguments passed to \code{\link{spatialPlot}}
#' 
#' @details The function applies the \code{\link{spatialPlot}} function, that in turn uses \code{lattice-methods}.
#'  
#'  Many different aspects of the plot can be controlled passing the relevant arguments to 
#'  \code{\link[sp]{spplot}}.
#'  
#' @return A lattice plot of class \dQuote{trellis}. 
#' 
#'    
#'@author M. Iturbide
#'@export
#'@importFrom grDevices colorRampPalette
#'@importFrom transformeR aggregateGrid subsetGrid getShape redim climatology
#' @examples
#' library(transformeR)
#' hindcast <- subsetGrid(CFS_Iberia_tas, years = 1983:2001)
#' forecast <- subsetGrid(CFS_Iberia_tas, years = 2002)
#' tercileMap(hindcast, forecast)

tercileMap <- function(hindcast, forecast, ...) {
      custom <- list(...)
      message("[", Sys.time(), "]  Preparing data...")
      suppressMessages(hindyear <- aggregateGrid(hindcast, aggr.y = list(FUN = mean, na.rm = TRUE)))
      suppressMessages(foreyear <- aggregateGrid(forecast, aggr.y = list(FUN = mean, na.rm = TRUE)))
      nmem <- getShape(hindyear)["member"]
      # hind <- redim(downscaleR:::flatMemberDim(hindyear), drop = TRUE)
      hind <- lapply(1:nmem, function(x){
        subsetGrid(hindyear, members = x)
      })
      hind <- do.call("bindGrid.time", hind)
      hind <- redim(hind, drop = TRUE)
      message("[", Sys.time(), "] Calculating terciles...")
      a <- apply(hind$Data, MARGIN = c(2,3), FUN = quantile, probs = c(1/3, 2/3))
      nmem <- getShape(forecast)["member"]
      nlon <- getShape(forecast)["lon"]
      nlat <- getShape(forecast)["lat"]
      co <- expand.grid(1:nlat, 1:nlon)
      indtercile <- array(dim = c(nmem, nlat, nlon))
      for (m in 1:nmem) {
            formem <- subsetGrid(foreyear, members = m)$Data
            for (i in 1:nrow(co)) {
                  indtercile[m, co[i,1],co[i,2]] <- which(order(c(formem[co[i,1],co[i,2]], a[,co[i,1],co[i,2]])) == 1)
            }
      }
      message("[", Sys.time(), "] Calculating probabilities...")
      probtercile <- array(dim = c(nlat, nlon))
      for (i in 1:nrow(co)) {
           z <- table(indtercile[,co[i,1],co[i,2]])
                  tercile <- which(z == max(z))[1]
                  probtercile[co[i,1],co[i,2]] <- round(z[tercile]/sum(z)*100, digits = 2)
                  if(tercile == 2) {
                        probtercile[co[i,1],co[i,2]] <- 0
                  } else if (tercile == 1) {
                        probtercile[co[i,1],co[i,2]] <- (-1)*probtercile[co[i,1],co[i,2]]
                  }
      }
      suppressMessages(output <- aggregateGrid(foreyear, aggr.mem = list(FUN = mean, na.rm = TRUE)))
      output$Data <- probtercile
      attr(output$Data, "dimensions") <- c("lat", "lon")
      jet.colors <- c("#00007F", "blue", "#007FFF", "cyan", "white", "yellow", "#FF7F00", "red", "red3")
      suppressMessages(
        suppressWarnings(custom[["grid"]] <- climatology(redim(output, member = FALSE)))
      )
      if(is.null(custom[["backdrop.theme"]])) custom[["backdrop.theme"]] <- "coastline"
      if(is.null(custom[["at"]])) custom[["at"]] <- c(-100, -70, -60, -50, -40, 40, 50, 60, 70, 100)
      if(is.null(custom[["col.regions"]])) custom[["col.regions"]] <- jet.colors
      if(is.null(custom[["colorkey"]])) custom[["colorkey"]] <- FALSE
      if(is.null(custom[["scales"]])) custom[["scales"]] <- list(draw = TRUE, alternating = 3, cex = .6)
      if(is.null(custom[["key"]])) custom[["key"]] <- list(space = "top", between = 0.3, between.columns = 0.5,
                                                           columns = 9, points = list( pch = 22, 
                                                                                       col = "black",
                                                                                       fill = custom[["col.regions"]],
                                                                                       cex = 1.3),
                                                           text = list(c("100..70%", "70..60%", "60..50%", "50..40%",
                                                                         "other",
                                                                         "40..50%", "50..60%", "60..70%", "70..100%"), cex = .65))
      
      message("[", Sys.time(), "] Done.")
      suppressWarnings(pl <- do.call("spatialPlot", custom))
      return(pl)
}

# end



