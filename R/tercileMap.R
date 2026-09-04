#     tercileMap.R Tercile maps for seasonal forecasts based on C3S style
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
#' @note The function is based on the tercile maps delivered by
#' C3S seasonal forecasts (\url{http://climate.copernicus.eu/s/charts/c3s_seasonal/}).
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
#'@importFrom transformeR aggregateGrid subsetGrid getShape redim climatology bindGrid
#' @examples \donttest{
#' require(climate4R.datasets)
#' data(CFS_Iberia_tas)
#' library(transformeR)
#' hindcast <- subsetGrid(CFS_Iberia_tas, years = 1983:2001)
#' forecast <- subsetGrid(CFS_Iberia_tas, years = 2002)
#' tercileMap(hindcast, forecast)
#' }

tercileMap <- function(hindcast, forecast, ...) {
  
  custom <- list(...)
  
  message("[", Sys.time(), "] Preparing data...")
  
  suppressMessages(
    hindyear <- aggregateGrid(
      hindcast,
      aggr.y = list(FUN = mean, na.rm = TRUE)
    )
  )
  
  suppressMessages(
    foreyear <- aggregateGrid(
      forecast,
      aggr.y = list(FUN = mean, na.rm = TRUE)
    )
  )
  
  # mean(..., na.rm = TRUE) returns NaN if all values are NA.
  # Convert these back to NA so masked areas (e.g. sea) remain missing.
  hindyear$Data[is.nan(hindyear$Data)] <- NA
  foreyear$Data[is.nan(foreyear$Data)] <- NA
  
  
  # ------------------------------------------------------------------
  # Rearrange hindcast members into the time dimension
  # ------------------------------------------------------------------
  
  nmem <- getShape(hindyear)["member"]
  
  hind <- lapply(seq_len(nmem), function(x) {
    subsetGrid(hindyear, members = x)
  })
  
  hind <- do.call(
    "bindGrid",
    c(hind, dimension = "time")
  )
  
  hind <- redim(hind, drop = TRUE)
  
  
  # ------------------------------------------------------------------
  # Calculate hindcast tercile thresholds
  # ------------------------------------------------------------------
  
  message("[", Sys.time(), "] Calculating terciles...")
  
  a <- apply(
    hind$Data,
    MARGIN = c(2, 3),
    FUN = quantile,
    probs = c(1 / 3, 2 / 3),
    na.rm = TRUE
  )
  
  # quantile(..., na.rm = TRUE) can also produce problematic values
  # where no valid hindcast data exist.
  a[is.nan(a)] <- NA
  
  
  # ------------------------------------------------------------------
  # Dimensions
  # ------------------------------------------------------------------
  
  nmem <- getShape(forecast)["member"]
  nlon <- getShape(forecast)["lon"]
  nlat <- getShape(forecast)["lat"]
  
  co <- expand.grid(
    lat = seq_len(nlat),
    lon = seq_len(nlon)
  )
  
  
  # ------------------------------------------------------------------
  # Classify each forecast member:
  #
  #   1 = lower tercile
  #   2 = middle tercile
  #   3 = upper tercile
  #
  # Missing forecast / threshold values remain NA.
  # ------------------------------------------------------------------
  
  indtercile <- array(
    NA_real_,
    dim = c(nmem, nlat, nlon)
  )
  
  for (m in seq_len(nmem)) {
    
    formem <- subsetGrid(
      foreyear,
      members = m
    )$Data
    
    for (i in seq_len(nrow(co))) {
      
      lat <- co$lat[i]
      lon <- co$lon[i]
      
      x  <- formem[lat, lon]
      q1 <- a[1, lat, lon]
      q2 <- a[2, lat, lon]
      
      # Preserve masked / missing grid cells
      if (is.na(x) || is.na(q1) || is.na(q2)) {
        
        indtercile[m, lat, lon] <- NA
        
      } else if (x <= q1) {
        
        indtercile[m, lat, lon] <- 1
        
      } else if (x <= q2) {
        
        indtercile[m, lat, lon] <- 2
        
      } else {
        
        indtercile[m, lat, lon] <- 3
      }
    }
  }
  
  
  # ------------------------------------------------------------------
  # Calculate probability of most likely tercile
  #
  # lower tercile  -> negative probability
  # middle tercile -> 0
  # upper tercile  -> positive probability
  # no data        -> NA
  # ------------------------------------------------------------------
  
  message("[", Sys.time(), "] Calculating probabilities...")
  
  probtercile <- array(
    NA_real_,
    dim = c(nlat, nlon)
  )
  
  for (i in seq_len(nrow(co))) {
    
    lat <- co$lat[i]
    lon <- co$lon[i]
    
    vals <- indtercile[, lat, lon]
    
    # If all ensemble members are missing,
    # keep this grid cell as NA.
    if (all(is.na(vals))) {
      probtercile[lat, lon] <- NA
      next
    }
    
    # Count only valid tercile classifications
    z <- table(vals, useNA = "no")
    
    # Actual tercile number (1, 2, or 3), not position in table
    tercile <- as.numeric(
      names(z)[which.max(z)]
    )
    
    probability <- round(
      max(z) / sum(z) * 100,
      digits = 2
    )
    
    if (tercile == 1) {
      
      # Lower tercile
      probtercile[lat, lon] <- -probability
      
    } else if (tercile == 2) {
      
      # Middle tercile
      probtercile[lat, lon] <- 0
      
    } else if (tercile == 3) {
      
      # Upper tercile
      probtercile[lat, lon] <- probability
    }
  }
  
  
  # ------------------------------------------------------------------
  # Create output grid
  # ------------------------------------------------------------------
  
  suppressMessages(
    output <- aggregateGrid(
      foreyear,
      aggr.mem = list(
        FUN = mean,
        na.rm = TRUE
      )
    )
  )
  
  output$Data <- probtercile
  
  attr(
    output$Data,
    "dimensions"
  ) <- c("lat", "lon")
  
  
  # ------------------------------------------------------------------
  # Plot settings
  # ------------------------------------------------------------------
  
  jet.colors <- c(
    "#00007F",
    "blue",
    "#007FFF",
    "cyan",
    "white",
    "yellow",
    "#FF7F00",
    "red",
    "red3"
  )
  
  suppressMessages(
    suppressWarnings(
      custom[["grid"]] <- climatology(
        redim(
          output,
          member = FALSE
        )
      )
    )
  )
  
  if (is.null(custom[["backdrop.theme"]])) {
    custom[["backdrop.theme"]] <- "coastline"
  }
  
  if (is.null(custom[["at"]])) {
    custom[["at"]] <- c(
      -100, -70, -60, -50, -40,
      40,  50,  60,  70, 100
    )
  }
  
  if (is.null(custom[["col.regions"]])) {
    custom[["col.regions"]] <- jet.colors
  }
  
  if (is.null(custom[["colorkey"]])) {
    custom[["colorkey"]] <- FALSE
  }
  
  if (is.null(custom[["scales"]])) {
    custom[["scales"]] <- list(
      draw = TRUE,
      alternating = 3,
      cex = 0.6
    )
  }
  
  if (is.null(custom[["key"]])) {
    
    custom[["key"]] <- list(
      space = "top",
      between = 0.3,
      between.columns = 0.5,
      columns = 9,
      
      points = list(
        pch = 22,
        col = "black",
        fill = custom[["col.regions"]],
        cex = 1.3
      ),
      
      text = list(
        c(
          "100..70%",
          "70..60%",
          "60..50%",
          "50..40%",
          "other",
          "40..50%",
          "50..60%",
          "60..70%",
          "70..100%"
        ),
        cex = 0.65
      )
    )
  }
  
  
  # ------------------------------------------------------------------
  # Plot
  # ------------------------------------------------------------------
  
  message("[", Sys.time(), "] Done.")
  
  suppressWarnings(
    pl <- do.call(
      "spatialPlot",
      custom
    )
  )
  
  return(pl)
}
# end



