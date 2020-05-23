##     reliabilityCategories Reliability plot with related reliability categories of probabilistic predictions. 
##
##     Copyright (C) 2016 Santander Meteorology Group (http://www.meteo.unican.es)
##
##     This program is free software: you can redistribute it and/or modify
##     it under the terms of the GNU General Public License as published by
##     the Free Software Foundation, either version 3 of the License, or
##     (at your option) any later version.
## 
##     This program is distributed in the hope that it will be useful,
##     but WITHOUT ANY WARRANTY; without even the implied warranty of
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##     GNU General Public License for more details.
## 
##     You should have received a copy of the GNU General Public License
##     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Reliability categories of a probabilistic forecast.
#' 
#' @description Calculates (and draws) 
#' reliability diagrams and the related reliability categories, according to Weisheimer et al. 2014 and Manzanas et al. 2017.
#' 
#' @param hindcast Grid of forecast data
#' @param obs Grid of observations
#' @param regions SpatialPolygons* object \code{\link[sp]{SpatialPolygons}}. delimiting the regions for which
#' the relaiability is calculated. Default is NULL (See details).
#' @param n.events Number of events considered. Default is 3 (terciles)
#' @param labels Character of the names to be given at the events defined in \code{n.events} 
#' (e.g. c("lower", "middle", "upper")). If NULL (default) numbered events are returned 
#' (Event 1 corresponds to the lowest values).
#' @param n.bins (optional): number of probability bins considered. By default n.bins = 10
#' @param n.boot number of samples considered for bootstrapping. By default n.boot = 100
#' @param conf.level Confidence interval for the reliability line. By default \code{conf.level} = 0.75 (two sided), as in Weisheimer et al. 2014
#' @param na.rate Allowed proportion of NA values in each region. Regions with proportions higher than na.rate
#' are excuded from the analysis. Default is 0.75.
#' @param diagrams Logical (default = TRUE). Plotting results.  
#' @param cex0 numeric (default is 0.5). Minimum size of the points shown in the reliability diagrams, i.e. size of the point 
#' for the minimum n frequency (n = 1) (see parameter \code{n.bins}.  The sizes for points that correspond to n > 1 
#' are reescaled according to parameter \code{cex.scale}.
#' @param cex.scale numeric (default is 20). Scaling factor for points sizes in the reliability diagrams (see parameter \code{cex0}) 
#' @param layout integer (default = c(1, n.events)). Sets the layout of panels (rows,cols)
#' @param backdrop.theme Reference geographical lines to be added to the plot. Default is \code{"countries"} 
#' (See \code{\link{spatialPlot}} for other options).
#' @param return.diagrams Logical. Available when \code{diagrams = TRUE}. If TRUE a trellis object for plotting diagrams is returned.
# @param nod Required if diagrams = TRUE. m*2 matrix of coordinates (m=locations, column1=latitude, column2=longitude)
# @param xlim Required if diagrams = TRUE. Limits for maps
# @param ylim Required if diagrams = TRUE. Limits for maps

#' @details If parameter regions is NULL (default) the whole region in \code{obs} and \code{hindcast} is considered 
#' for computing reliability. A subregion will be considered If the corresponding SpatialPolygons* object
#' is provided. In these cases, if parameter \code{diagrams = TRUE}, reliability diagrams are plotted for 
#' each specified event. If a SpatialPolygons* object of multiple subregions is provided, reliability is computed 
#' separately for each region and reliability maps are plotted instead. 
#' 
#' A SpartialPolygons* object is easily obtained by reading a shapefile with function 
#' \code{\link[rgdal]{readOGR}}.
#' 
#' @return Grid of reliability categories with an additional data dimension for categories
#' and an additional slot (\code{$ReliabilityCategories}) containing the following elements:
#' catname: reliability categories.
#' slope: \code{$slope} slope of the reliability line; \code{$lower} lower bound confidence for slope (according to "sigboot"); 
#' \code{$upper} upper bound confidence for slope (according to "sigboot").
#' 
#' If \code{return.diagrams} is TRUE, a list of two objects is returned, the grid object (\code{$grid}) and a trellis class object (\code{$plot}). 
#' 
#' 
#' @export
#' @author R. Manzanas \& M.Iturbide
# @import verification
# @import plot3D
#' @importFrom transformeR getGrid redim getDim bindGrid isRegular
#' @importFrom sp SpatialPoints SpatialPolygons Polygons Polygon over
#' @importFrom graphics plot.new abline polygon text grid title
#' @importFrom stats na.omit
#' @importFrom verification verify
#' @import lattice
#' 
#' @family visualization functions
#' 
#' @references Weisheimer, A., Palmer, T.N., 2014. On the reliability of seasonal climate forecasts. Journal of The Royal Society Interface 11, 20131162. doi:10.1098/rsif.2013.1162
#' @references Manzanas, R., Lucero, A., Weisheimer, A., Guti\'errez, J.M., 2017. Can bias correction and statistical downscaling methods improve the skill of seasonal precipitation forecasts? Climate Dynamics, pg 1-16, doi:10.1007/s00382-017-3668-z
#' @examples \dontrun{
#' data("tas.cfs")
#' data("tas.ncep")
#' data("PRUDENCEregions")
#' require(transformeR)
#' #select spatio-temporal domain
#' tas.ncep2 <- subsetGrid(tas.ncep, lonLim = c(-10, 35), latLim = c(35,70), years = 1983:2009)
#' tas.cfs2 <- subsetGrid(tas.cfs, lonLim = c(-10, 35), latLim = c(35,70), years = 1983:2009)
#' #interpolate
#' tas.cfs2.int <- interpGrid(tas.cfs2, getGrid(tas.ncep2))
#' #calculate reliability
#' rel.reg <- reliabilityCategories(hindcast = tas.cfs2.int, obs = tas.ncep2,
#'                                  n.bins = 5, n.boot = 10,
#'                                  regions = PRUDENCEregions,
#'                                  return.diagrams = TRUE)
#' rel <- reliabilityCategories(hindcast = tas.cfs2.int, obs = tas.ncep2,
#'                              n.bins = 5, n.boot = 10)
#' # Irregular grids
#' require(climate4R.datasets) 
#' data("NCEP_Iberia_tas")
#' data("CFS_Iberia_tas")
#' obs <- aggregateGrid(VALUE_Iberia_tas, aggr.y = list(FUN= mean))
#' hind <- aggregateGrid(interpGrid(NCEP_Iberia_tas, getGrid(obs)), aggr.y = list(FUN= mean))
#' rel <- reliabilityCategories(hindcast = hind, obs = obs,
#'                              n.bins = 7, n.boot = 10)
#' }


reliabilityCategories <- function(hindcast,
                                  obs,
                                  regions = NULL,
                                  n.events = 3,
                                  labels = NULL,
                                  n.bins = 10,
                                  n.boot = 100,
                                  conf.level = 0.75, 
                                  na.rate = 0.75,
                                  diagrams = TRUE,
                                  cex0 = 0.5,
                                  cex.scale = 20,
                                  layout = c(1,n.events),
                                  backdrop.theme = "countries",
                                  return.diagrams = FALSE){ 
  if (!identical(getGrid(obs)$y, getGrid(hindcast)$y) | !identical(getGrid(obs)$x, getGrid(hindcast)$x)) {
    stop("obs and hindcast are not spatially consistent. Consider using function 'interpGrid' from package transformeR")
  }
  sigboot <- 1 - conf.level
  layout <- rev(layout)
  if (is.null(labels)) labels <- paste("Event", 1:n.events)
  red <- rgb(1, 0, 0, 1, names = "red", maxColorValue = 1)
  orange <- rgb(1, 0.65, 0.3, 1, names = "orange", maxColorValue = 1)
  yellow <- rgb(1, 1, 0, 1, names = "yellow", maxColorValue = 1)
  darkyellow <- rgb(0.8, 0.8, 0, 1, names = "dark_yellow", maxColorValue = 1)
  cyan <- rgb(0, 1, 1, 1, names = "cyan", maxColorValue = 1)
  green <- rgb(0, 1, 0, 1, names = "green", maxColorValue = 1)
  
  regs <- regions
  if (is.null(regs)) {
    bbox <- getGrid(obs) 
    esquinas <- matrix(ncol = 2, nrow = 5)
    esquinas[1,] <- c(min(bbox$x), min(bbox$y))
    esquinas[2,] <- c(min(bbox$x), max(bbox$y))
    esquinas[3,] <- c(max(bbox$x), max(bbox$y))
    esquinas[4,] <- c(max(bbox$x), min(bbox$y))
    esquinas[5,] <- c(min(bbox$x), min(bbox$y))
    regs <- sp::SpatialPolygons(list(sp::Polygons(list(Polygon(list(esquinas))), ID = "region")))
  }
  if (class(regs) == "SpatialPolygonsDataFrame") {
    regs <-  SpatialPolygons(regs@polygons)
  }
  if (isRegular(obs)) {
    hindcast <- redim(hindcast)
  } else {
    hindcast <- redim(hindcast, loc = TRUE)
  }
  memind <- which(getDim(hindcast) == "member")
  timeind <- which(getDim(hindcast) == "time")
  nmem <- dim(hindcast$Data)[memind]
  ntime <- dim(hindcast$Data)[timeind]
  coords <- obs$xyCoords
  if (isRegular(obs)) {
    coordinates <- expand.grid(obs$xyCoords$y, obs$xyCoords$x)
    coords <- as.data.frame(cbind(coordinates[,2], coordinates[,1]))
  }
  spoints <- SpatialPoints(coords)
  suppressMessages(
    obs.fin <- transformeR::climatology(obs)
  )
  atrsobsfin <- attributes(obs.fin$Data)$dimensions
  ob.full <- obs$Data
  if (isRegular(obs)) ob.full <- array3Dto2Dmat(obs$Data)
  ob.clim <- array(dim = c(n.events, dim(ob.full)[-1]))
  lower <- array(dim = c(length(regs), (n.events)))
  rownames(lower) <- names(regs)
  upper <- array(dim = c(length(regs), (n.events)))
  rownames(upper) <- names(regs)
  sl <- array(dim = c(length(regs), (n.events)))
  rownames(sl) <- names(regs)
  ob.slope <- list("sl" = sl, "lower" = lower, "upper" = upper)
  ob.catname  <- array(dim = c(length(regs), (n.events)))
  rownames(ob.catname) <- names(regs)
  for (l in 1:length(regs)) {
    o <- over(spoints, regs[l,])
    w <- which(!is.na(o))
    if (length(w) > 0) {
      ob <- ob.full[, w, drop = F]
      naregion <- length(which(is.na(ob)))/length(ob)
      if (naregion <= na.rate) {
        se <- array(dim = c(nmem, ntime, length(w)))
        if (isRegular(obs)) {
          for (i in 1:nmem) {
            se[i,,] <- array3Dto2Dmat(subsetGrid(hindcast, members = i)$Data)[,w, drop = FALSE]
          }
        } else {
          se <- hindcast$Data[,,w, drop = FALSE]
        }
        #remove empty pixels
        naind.obs <- which(is.na(ob[1,]))
        naind.hindcast <- which(is.na(se[1,1,]))
        naind <- unique(c(naind.obs, naind.hindcast))
        if (length(naind) != 0) {
          obna <- ob[,-naind, drop = FALSE]
          sena <- se[,,-naind, drop = FALSE]
        } else {
          obna <- ob
          sena <- se
        }
        if (!any(dim(obna) == 0)) {
          message("[", Sys.time(), "] Calculating categories for region ", l, " out of ", length(regs))
          sl <- calculateReliability(obs = obna, hindcast = sena, n.events = n.events, n.bins = n.bins, n.boot = n.boot)
          
          n <- sl$n
          nyear <- sl$nyear
          npoint <- sl$npoint
          slope <- sl$slope
          slope_boot <- sl$slope_boot
          hindcastprob <- sl$hindcastprob
          obsfreq <- sl$obsfreq
          hindcastfreq <- sl$hindcastfreq
          
          cat <- rep(NA, n.events)
          catcol <- rep(NA, n.events)
          catname <- rep("", n.events)
          
          for (ibins in 1:n.events) {
            aux <- quantile(slope_boot[, ibins], c(sigboot/2, 1-sigboot/2), na.rm = T)
            slope_lower <- aux[[1]]
            slope_upper <- aux[[2]]
            rm(aux)
            if (!is.na(slope[ibins])) {
              if (slope[ibins] >= 0.5 & slope_lower >= 0.5 & slope_lower <= 1 & slope_upper >= 1) {  
                cat[ibins] <- 5 
                catcol[ibins] <- green
                catname[ibins] <- "perfect"
              } else if ((slope[ibins] >= 0.5 & slope_lower >= 0.5 & slope_upper <= 1) | 
                         (slope[ibins] >= 1 & slope_lower >= 1 & slope_upper >= 1)) {
                cat[ibins] <- 4  
                catcol[ibins] <- cyan
                catname[ibins] <- "still useful"
                ## OJO: nueva categoria! 
              } else if (slope[ibins] >= 0.5 & slope_lower > 0) {         
                cat[ibins] <- 3.5  
                catcol[ibins] <- darkyellow 
                catname[ibins] <- "marginally useful +"
              } else if (slope[ibins] > 0 & slope_lower > 0) {
                cat[ibins] <- 3  
                catcol[ibins] <- yellow 
                catname[ibins] <- "marginally useful"
              } else if (slope[ibins] > 0 & slope_lower < 0) {
                cat[ibins] <- 2  
                catcol[ibins] <- orange
                catname[ibins] <- "not useful"
              } else if (slope[ibins] < 0) {
                cat[ibins] <- 1  
                catcol[ibins] <- red 
                catname[ibins] <- "dangerously useless"
              } 
            }
            ob.clim[ibins, w] <- cat[ibins]
            ob.slope$lower[l , ibins] <- slope_lower
            ob.slope$upper[l , ibins] <- slope_upper
            ob.slope$sl[l ,] <- slope
            ob.catname[l ,] <- catname
          }
        }
      }
    }}
  l.obs.fin <- list()
  for (i in 1:n.events) {
    if (isRegular(obs)) {
      obs.fin$Data <- mat2Dto3Darray(as.matrix(ob.clim[i, , drop = F]), x = obs$xyCoords$x, y = obs$xyCoords$y)
    } else {
      obs.fin$Data <- as.matrix(ob.clim[i, , drop = F])
    }
    attr(obs.fin$Data, "dimensions") <- atrsobsfin
    suppressMessages(
      cats <- transformeR::climatology(obs.fin)
    )
    l.obs.fin[[i]] <- cats
  }
  mg <- bindGrid(l.obs.fin, dimension = "member")
  mg$Members <- gsub(pattern = " ", replacement = "_", x = labels)
  if (diagrams) {
    if (length(regs) > 1) {
      pc <- spatialPlot(mg,  backdrop.theme = backdrop.theme, at = c(0.5,1.5,2.5,3.05,3.55,4.5,5.5), 
                                         col.regions = c(red, orange, yellow, darkyellow, cyan, green),
                                         layout = layout,
                        sp.layout = list(list(regs, first = F, col = "gray")),
                                         colorkey = list(labels = list( 
                                           cex = 1,
                                           at = c(1, 2, 2.75, 3.25, 4, 5), 
                                           labels = c("dangerously useless", "not useful","marginally useful",
                                                      "marginally useful +","still useful","perfect"))))
    }else{
      x1 <- 1/n.events
      y1 <- 1/n.events
      x2 <- 1
      y2 <- (1/n.events) + (0.5*(1-(1/n.events)))
      a <- (y2 - y1)/(x2 - x1)
      b <- y1 - ((x1*(y2 - y1))/(x2 - y1))
      
      y <- unlist(obsfreq)
      ylimcat <- 0.6
      x <- unlist(hindcastprob)
      z <- rep(1:n.events, each = n.bins)
      # w <- rep(labels, each = n.bins)
      
      a_lower <- ob.slope$lower #slope_lower 
      b_lower <- (1 - ob.slope$lower)/n.events
      ## upper bound
      a_upper <- ob.slope$upper
      b_upper <- (1 - ob.slope$upper)/n.events
      
      
      # Customized Lattice Example
      pc <- xyplot(y~x|z, par.strip = list(lines = 1), ylim = c(0,1), strip = strip.custom(fg = rgb(0,0,0,0), strip.names = c(T,F), strip.levels = c(F,T), factor.levels = labels), 
                   scales=list(x = list(at = seq(0,1,round(1/n.bins, digits = 2)),
                                        labels = seq(0,1,round(1/n.bins, digits = 2))),
                               y = list(at = seq(0,1,round(1/n.bins, digits = 2)),
                                        labels = seq(0,1,round(1/n.bins, digits = 2))),
                               
                               cex = .8, col = "black"),
                   panel=function(x, y, z, ...) {
                     # panel.locfit(...)
                     
                     panel.grid(h = -1, v = -1)
                     panel.polygon(c(0, 1/n.events, 1/n.events, 0), c(0, 0, 1/n.events, b),
                                   border = NA, col = "lightgray")
                     panel.polygon(c(1/n.events, 1, 1, 1/n.events), c(1/n.events, y2, 1, 1),
                                   border = NA, col = "lightgray")
                     for(i in 1:n.events){
                       if(packet.number() == i){
                         panel.polygon(c(0, 1/n.events, 0, 0), c(b_lower[,i], 1/n.events, b_upper[,i], b_lower[,i]),
                                       border = NA, col = catcol[i])
                         panel.polygon(c(1/n.events, 1, 1, 1/n.events), c(1/n.events, a_lower[,i]+b_lower[,i], a_upper[,i]+b_upper[,i], 1/n.events),
                                       border = NA, col = catcol[i])
                         # panel.abline(b_lower[,i], a_lower[,i], col = "gray40", lty = 2, lwd = 1)
                         # panel.abline(b_upper[,i], a_upper[,i], col = "gray40", lty = 2, lwd = 1)
                         panel.abline((1-slope[i])/n.events, slope[i], col = "black", lty = 1, lwd = 1.5)
                         panel.text(0.35, ylimcat, catname[i])
                         
                         panel.xyplot(x, y, pch = 16, col = "black", 
                                      cex = na.omit(((((hindcastfreq[[i]]*nyear*npoint)-cex0)*(cex0*cex.scale-cex0)) / ((nyear*npoint)-cex0)) + cex0))
                         # panel.xyplot(0.45,0.2, pch = 16, col = "black", 
                         #              cex = min(hindcastfreq[[i]]) * cex.scale)
                         # panel.text(0.68, 0.2, paste0("min: n = ", min(hindcastfreq[[i]])*nyear*npoint))
                       }
                     }
                     if(packet.number() == 1){
                       panel.xyplot(0.68,0.08, pch = 16, col = "black", cex = cex0)
                       panel.text(0.8, 0.08, "n = 1")
                     }
                     panel.abline(c(0, 1),  col = "black", lty = 3, lwd = 1.5)
                     panel.abline(h = 1/n.events, col = "black", lty = 3, lwd = 1.5)
                     panel.abline(coef = c(b, a), lty = 3, lwd = 1.5)
                     
                     
                   },
                   
                   layout = layout,
                   xlab = "Predicted probability", ylab= "Observed frequency",
                   main= list(cex = 1, font = 1, label = sprintf("n = %d years x %d points", nyear, npoint)))
      
      # update(xyp, par.settings = list(fontsize = list(text = 8, points = 10)))
      
      #########################################################
      
      # 
      #                   par(mfrow = c(1, n.events), pty="s", mgp=c(2,1,0), mar=c(1,3,2,2), oma=c(2,0,2,0))
      #                   for(i in 1:n.events){
      #                         ## reliability diagram
      #                         x1 <- 1/n.events
      #                         y1 <- 1/n.events
      #                         x2 <- 1
      #                         y2 <- (1/n.events) + (0.5*(1-(1/n.events)))
      #                         a <- (y2-y1)/(x2-x1)
      #                         b <- y1-((x1*(y2-y1))/(x2-y1))
      # 
      #                         plot(b, a, col = "black", lty = 3, typ = "l",
      #                              xlim = c(0,1), ylim = c(0,1),
      #                              xlab = "hindcast prob.", ylab = "obs. freq.",
      #                              main = labels[i],
      #                              sub = list(catname[i], cex = 1.2),
      #                              font.sub=4)
      #                         abline(b, a, col = "black", lty = 3)
      #                         polygon(c(0, 1/n.events, 1/n.events, 0), c(0, 0, 1/n.events, b),
      #                                 border = NA, col = "lightgray")
      #                         polygon(c(1/n.events, 1, 1, 1/n.events), c(1/n.events, y2, 1, 1),
      #                                 border = NA, col = "lightgray")
      #                         abline(0, 1,  col = "black", lty = 3, lwd = 1.5)
      #                         abline(h = 1/n.events, col = "black", lty = 3)
      #                         abline(v = 1/n.events, col = "black", lty = 3)
      # 
      #                         ## intervalo de confianza para la pendiente
      #                         ## lower bound
      #                         a_lower <- ob.slope$lower[ , i] #<- #slope_lower
      #                         b_lower <- (1-ob.slope$lower[ , i])/n.events
      #                         ## upper bound
      #                         a_upper <- ob.slope$upper[ , i]
      #                         b_upper <- (1-ob.slope$upper[ , i])/n.events
      #                         polygon(c(0, 1/n.events, 0, 0), c(b_lower, 1/n.events, b_upper, b_lower),
      #                                 border = NA, col = catcol[i])
      #                         polygon(c(1/n.events, 1, 1, 1/n.events), c(1/n.events, a_lower+b_lower, a_upper+b_upper, 1/n.events),
      #                                 border = NA, col = catcol[i])
      #                         abline(b_lower, a_lower, col = "black", lty = 2, lwd = 2)
      #                         abline(b_upper, a_upper, col = "black", lty = 2, lwd = 2)
      #                         abline((1-slope[i])/n.events, slope[i], col = "black", lwd = 2)
      # 
      #                         ## puntos del reliability diagram (escalados por el peso)
      #                         points(0.1, .8, pch = 19, cex = cex0)
      #                         text(0.15, .8, "n = 1", cex=.95, font=2, pos=4)
      #                         points(hindcastprob[[i]], obsfreq[[i]], pch = 19,
      #                         cex = hindcastfreq[[i]]*10)
      #                         # cex = ((((hindcastfreq[[i]]*nyear*npoint)-cex0)*(cex0*10-cex0)) / ((nyear*npoint)-cex0)) + cex0)
      #                               grid(nx = NULL, ny = NULL, col = "lightgray", lty = 4, lwd = 0.5)
      #                   }
      #                   title(sprintf("n = %d years x %d points", nyear, npoint), outer = T)
      ###########################################################
      
    }
    print(pc)
  }
  result.grid <- mg
  if (isRegular(obs)) attr(result.grid$Data, "dimensions") <- c("member", "time", "lat", "lon")
  attr(result.grid$Data, "dimensions") <- c("member", "time", "loc")
  attr(result.grid$Data, "climatology:fun") <- NULL
  result <- list()
  colnames(ob.catname) <- mg$Members
  result$catname <- ob.catname
  colnames(ob.slope$sl) <- mg$Members
  colnames(ob.slope$lower) <- mg$Members
  colnames(ob.slope$upper) <- mg$Members
  result$slope <- ob.slope
  message("[", Sys.time(), "] Done.")
  result.grid$ReliabilityCategories <- result
  attr(result.grid$ReliabilityCategories, "observations") <- attr(obs, "dataset")
  # result.grid$Variable$varName <- result.grid$Variable$varName[1]
  # result.grid$Variable$level <- NA
  # attr(result.grid$Variable, "units") <- attr(result.grid$Variable, "units")[1]
  # attr(result.grid$Variable, "use_dictionary") <- attr(result.grid$Variable, "use_dictionary")[1]
  # attr(result.grid$Variable, "description") <- attr(result.grid$Variable, "description")[1]
  # attr(result.grid$Variable, "daily_agg_cellfun") <- attr(result.grid$Variable, "daily_agg_cellfun")[1]
  # attr(result.grid$Variable, "monthly_agg_cellfun") <- attr(result.grid$Variable, "monthly_agg_cellfun")[1]
  # attr(result.grid$Variable, "verification_time") <- attr(result.grid$Variable, "verification_time")[1]
  # attr(result.grid$Variable, "annual_agg_cellfun") <- attr(result.grid$Variable, "annual_agg_cellfun")[1]
  # attr(result.grid$Variable, "longname") <- NULL
  if (return.diagrams & diagrams) {
    return(list("grid" = result.grid, "plot" = pc))
  }else{
    return(result.grid)
  }
}


#End


#' @title Generate object needed by function reliability
#' 
#' @description This function provides the object needed by "calculateReliability_v2.R" 
#' for calculating the reliability categories of a probabilistic prediction.
#' 
#' @param obs m*n matrix of observations (m = years, n = locations)
#' @param hindcast m*n*l matrix of predictions (m = members, n = years, l = locations)
#' @param n.events (optional): number of categories considered (e.g. 3 for terciles). By default n.events = 3
#' @param n.bins (optional): number of probability bins considered. By default n.bins = 10
#' @param n.boot number of samples considered for bootstrapping. By default n.boot = 100
#'
#' @return List with the following elements:
#' n.events = n.events
#' nyear = number of years
#' npoint = number of locations
#' n = nyear*npoint
#' hindcastprob = probability bins (center), per event (e.g. per tercile)
#' obsfreq = observed frequency, per event (e.g. per tercile)
#' hindcastfreq = predicted frequency, per event (e.g. per tercile)
#' slope = slope of the reliability line, per event (e.g. per tercile)
#' slope_boot = n.boot*n.events matrix, with all the boostrapped values for the slope of the reliability line 
#' 
#' @author R. Manzanas \& M.Iturbide
#' @importFrom abind abind
#' @importFrom transformeR makeMultiGrid
#' @import verification
#' @keywords internal





calculateReliability <- function(obs, hindcast, n.events = n.events, n.bins = n.bins, n.boot = n.boot) {
  if (!(dim(obs)[1] == dim(hindcast)[2] & dim(obs)[2] == dim(hindcast)[3])) {
    stop("Observations and predictions are not congruent in size")
  }
  nyear <- dim(obs)[1]
  nmemb <- dim(hindcast)[1]
  O <- obs2bin(obs, n.events)
  P <- hindcast2prob(hindcast, n.events)
  ## calculo puntos diagrama fiabilidad
  aux <- concatenateDataRelDiagram_v2(O$bin, P$prob, n.bins) 
  n.events <- aux$n.events
  nyear <- aux$nyear
  npoint <- aux$npoint
  n <- aux$n
  hindcastprob <- vector("list", n.events)
  obsfreq <- vector("list", n.events)
  hindcastfreq <- vector("list", n.events)
  slope <- rep(NA, n.events)
  # intercept <- rep(NA, 1, n.events)
  for (ibins in 1:n.events) {
    hindcastprob.bin <- eval(parse(text = sprintf("aux$cat%d$y.i", ibins)))
    hindcastprob[[ibins]] <- hindcastprob.bin
    obsfreq.bin <- eval(parse(text = sprintf("aux$cat%d$obar.i", ibins)))
    obsfreq[[ibins]] <- obsfreq.bin
    # PESOS hindcastfreq!!!
    hindcastfreq.bin <- eval(parse(text = sprintf("aux$cat%d$prob.y", ibins)))
    hindcastfreq[[ibins]] <- hindcastfreq.bin
    
    if (!(is.null(hindcastprob.bin) | is.null(obsfreq.bin) | is.null(hindcastfreq.bin))) {
      fit <- lm(obsfreq.bin ~ hindcastprob.bin, weights = hindcastfreq.bin)
      slope[ibins] <- fit$coefficients[[2]]
      # intercept[ibins] <- fit$coefficients[[1]]
    } else {
      slope[ibins] <- NA
      # intercept[ibins] <- NA
    }
  }
  
  ## bootstrapping
  slope_boot <- matrix(NA, n.boot, n.events)
  # intercept_boot <- matrix(NA, n.boot, n.events)
  message("...[", Sys.time(), "] Computing bootstrapping...")
  for (iboot in 1:n.boot){
    #             if (abs(iboot/50 - round(iboot/50)) < eps()) {
    #                   print(sprintf("... computing bootstrapping %d ...", iboot))
    #             }
    
    indmembperm <- sample(1:nmemb, nmemb, replace = TRUE)
    indyearperm <- sample(1:nyear, nyear, replace = TRUE)
    indnodperm <- sample(1:dim(obs)[2], dim(obs)[2], replace = TRUE)
    
    P.prob.boot <- array(NA, c(n.events, nyear, dim(obs)[2]))
    Pcat.boot <- P$cat[indmembperm, indyearperm, indnodperm, drop = FALSE]
    for (inod in 1:dim(obs)[2]) {
      for (ibins in 1:n.events) {
        P.prob.boot[ibins, , inod] <- apply(Pcat.boot[, , inod,drop = FALSE] == ibins, 2, sum) / nmemb
        # P.prob.boot[ibins, , inod] <- apply(P$cat[, , inod] == ibins, 2, sum) / nmemb
      }
    }
    
    # O <- obs2bin(obs[indyearperm, indnodperm], n.events)
    # P <- hindcast2prob(hindcast[indmembperm, indyearperm, indnodperm], n.events)
    
    aux <- concatenateDataRelDiagram_v2(O$bin[, indyearperm, indnodperm, drop = FALSE], P.prob.boot, n.bins)  
    # aux <- concatenateDataRelDiagram_v2(O$bin, P$prob, n.bins)  
    for (ibins in 1:n.events) {   
      
      hindcastprob.bin <- eval(parse(text = sprintf("aux$cat%d$y.i", ibins)))
      obsfreq.bin <- eval(parse(text = sprintf("aux$cat%d$obar.i", ibins)))
      # PESOS hindcastfreq!!!
      hindcastfreq.bin <- eval(parse(text = sprintf("aux$cat%d$prob.y", ibins)))
      
      if (!(is.null(hindcastprob.bin) | is.null(obsfreq.bin) | is.null(hindcastfreq.bin))) {
        fit <- lm(obsfreq.bin ~ hindcastprob.bin, weights = hindcastfreq.bin)
        slope_boot[iboot, ibins] <- fit$coefficients[[2]]
        # intercept_boot[iboot, ibins] <- fit$coefficients[[1]]
      } else {
        slope_boot[iboot, ibins] <- NA
        # intercept_boot[iboot, ibins] <- NA
      }
    }  
  }
  message("...[", Sys.time(), "] Done.")
  # # intervalos de confianza para la pendiente de la reliability line
  # slope_lower <- matrix(NA, length(sigboot), n.events)
  # slope_upper <- matrix(NA, length(sigboot), n.events)
  # for (ibins in 1:n.events) {
  #   for (isigboot in 1:length(sigboot)) {
  #   aux <- quantile(slope_boot[, ibins], c(sigboot[isigboot], 1-sigboot[isigboot]))
  #   slope_lower[isigboot, ibins] <- aux[[1]]
  #   slope_upper[isigboot, ibins] <- aux[[2]]
  #   }
  # }
  
  result <- list()
  result$n.events <- n.events
  result$nyear <- nyear
  result$npoint <- npoint
  result$n <- n
  result$hindcastprob <- hindcastprob
  result$obsfreq <- obsfreq
  result$hindcastfreq <- hindcastfreq
  result$slope <- slope
  # result$intercept <- intercept
  result$slope_boot <- slope_boot
  # result$intercept_boot <- intercept_boot
  # result$conf.level <- conf.level
  # result$slope_lower <- slope_lower
  # result$slope_upper <- slope_upper
  
  return(result)
}

#End


#' @title internal function obs2bin
#' 
#' @description This function provides the object needed by "calculateReliability_v2.R" 
#' for calculating the reliability categories of a probabilistic prediction.
#' 
#' @param obs 2D-matrix of observations, dimensions = (time, npoints)
#' @param n.events number of categories (3 for terciles)
#'
#' @return 
#' bincat: list with two elements:  
#' bin: 3D-array of binary (0/1) observations, dimensions = (n.events, time, npoints)
#' cat: 2D-matrix with the observed category, dimensions = (time, npoints)
#'
#' @author R. Manzanas \& M.Iturbide
#' @keywords internal


obs2bin <- function(obs, n.events){
  bin <- array(0, c(n.events, dim(obs)[1], dim(obs)[2]))
  cat <- array(NA, c(dim(obs)[1], dim(obs)[2]))
  v.err <- rep(NA, dim(obs)[2])
  for (inod in 1:dim(obs)[2]) {
    tryCatch({
      ## categorias obs     
      catsobs <- quantile(obs[, inod], 0:n.events/n.events, na.rm = TRUE)
      auxobscat <- quantile2disc(obs[, inod], catsobs)
      auxobscat$mids <- sort(auxobscat$mids)
      
      for (ibins in 1:n.events){
        indcat <- which(auxobscat$new == auxobscat$mids[ibins])        
        bin[ibins, indcat, inod] <- 1     
        cat[indcat, inod] <- ibins
      }
    }, error = function(ex) {
      v.err[inod] <- 1
    })
  }
  if(any(!is.na(v.err))){
    message("Imposible to calculate categories for the ", (sum(v.err, na.rm = T)/length(v.err))*100, " percent of the grid boxes in obs.")
  }
  rm(auxobscat)
  bincat <- list()
  bincat$bin <- bin
  bincat$cat <- cat  
  rm(bin,cat)
  
  return(bincat)
}

#End



#' @title internal function prob2bin
#' 
#' @description This function provides the object needed by "calculateReliability_v2.R" 
#' for calculating the reliability categories of a probabilistic prediction.
#' 
#' @param hindcast 3D-array of predictions, dimensions = (member, time, npoints)
#' @param hindcast4cats Optional. 3D-array of predictions for which calculate the categories (e.g., terciles)
#' @param dimensions (member, time, npoints)
#' @param n.events Number of categories (3 for terciles)
#'
#' @return 
#' hindcastprob: 3D-array of probabilistic predictions, dimensions = (n.events, time, npoints)
#' @note For hindcast4cats, categories are calculated at model- (not at member-) level
#' @author R. Manzanas \& M.Iturbide
#' @keywords internal

hindcast2prob <- function(hindcast, n.events, hindcast4cats = NULL){
  
  prob <- array(NA, c(n.events, dim(hindcast)[2], dim(hindcast)[3]))
  cat <- array(NA, c(dim(hindcast)[1], dim(hindcast)[2], dim(hindcast)[3]))
  v.err <- rep(NA, dim(hindcast)[3])
  for (inod in 1:dim(hindcast)[3]) {
    ## categorias hindcast
    ## calculo los terciles de la prediccion a nivel de modelo (concateno todos los miembros)      
    if (!is.null(hindcast4cats)) {
      tmphindcast4catscat <- lapply(1:dim(hindcast4cats)[1], function(x) hindcast4cats[x, , inod])      
      tmphindcast4catscat <- do.call("c", tmphindcast4catscat)
    }      
    tmphindcastcat <- lapply(1:dim(hindcast)[1], function(x) hindcast[x, , inod])
    tmphindcastcat <- do.call("c", tmphindcastcat)
    
    if (!is.null(hindcast4cats)) {
      catshindcast <- quantile(tmphindcast4catscat, 0:n.events/n.events, na.rm = TRUE) 
      rm(tmphindcast4catscat)
    } else {
      catshindcast <- quantile(tmphindcastcat, 0:n.events/n.events, na.rm = TRUE) 
    }
    tryCatch({
      catshindcast <- quantile2disc(tmphindcastcat, catshindcast)
      rm(tmphindcastcat)
      catshindcast$mids <- sort(catshindcast$mids)
      auxhindcast <- matrix(NA, dim(hindcast)[2], dim(hindcast)[1])
      for (imemb in 1:dim(hindcast)[1]){
        i1 <- ((imemb-1)*dim(hindcast)[2]) + 1
        i2 <- i1 + dim(hindcast)[2] - 1
        auxhindcast[, imemb] <- catshindcast$new[i1:i2]
      }  
      tmp2 <- matrix(NA, dim(hindcast)[1], dim(hindcast)[2])
      for (ibins in 1:n.events){      
        tmp <- auxhindcast == catshindcast$mids[ibins]
        prob[ibins, , inod] <- apply(tmp, 1, sum) / dim(hindcast)[1]             
        tmp2[t(tmp)] <- ibins 
      }
      cat[, , inod] <- tmp2
      rm(tmp2)
    }, error = function(ex) {
      v.err[inod] <- 1
      nmemb<-dim(hindcast)[2]
      ntime<-dim(hindcast)[1]
      cat[, , inod] <- matrix(NA, nmemb, ntime)
    })
  }
  if(any(!is.na(v.err))){
    message("Imposible to calculate categories in the ", (sum(v.err, na.rm = T)/length(v.err))*100, " percent of the grid boxes in hindcast. Removed for the analysis.")
  }
  rm(auxhindcast)
  probcat <- list()
  probcat$prob <- prob
  probcat$cat <- cat
  rm(prob,cat)
  
  return(probcat)
}


#End


#' @title internal function concatenateDataRelDiagram_v2
#' 
#' @description This function provides the object needed by "calculateReliability_v2.R" 
#' for calculating the reliability categories of a probabilistic prediction.
#' 
#' @param obs 2D-matrix of observations, dimensions = (time, npoints)
#' @param n.events number of categories (3 for terciles)
#' @param n.bins n.bins
#'
#' @return 
#' bincat: list with two elements:  
#' bin: 3D-array of binary (0/1) observations, dimensions = (n.events, time, npoints)
#' cat: 2D-matrix with the observed category, dimensions = (time, npoints)
#' @import verification
#' @author R. Manzanas \& M.Iturbide
#' @keywords internal

concatenateDataRelDiagram_v2 <- function(obsbin, hindcastprob, n.bins) {
  # Description
  # 
  # Usage:
  # concatenateDataResDiagram(obsbin, hindcastprob, nod, n.events) 
  # Arguments:
  # obsbin: 4D-array of binary (0/1) observations, dimensions = (n.events, ynod, xnod, time)
  # hindcastprob: 4D-array of probabilistic predictions, dimensions = (n.events, ynod, xnod, time)
  # nod: matrix of coordinates, longitudes (latitudes) in first (second) column (common for obs and hindcast)
  # n.events: number of categories (3 for terciles)
  # Value:
  # dataRelDiagram: list with n.events elements, containing all the information to plot reliability diagrams 
  # (call the attribute.R function from verification R-package: 
  # http://cran.r-project.org/web/packages/verification/verification.pdf)
  
  #       require(verification)
  
  dataRelDiagram <- list()
  if (identical(as.vector(dim(obsbin)), as.vector(dim(hindcastprob)))){
    n.events <- dim(obsbin)[1]
    nyear <- dim(obsbin)[2]
    npoint <- dim(obsbin)[3]
    n <- nyear*npoint
  }
  dataRelDiagram$n.events <- n.events
  dataRelDiagram$nyear <- nyear
  dataRelDiagram$npoint <- npoint
  dataRelDiagram$n <- n
  
  for (ibins in 1:n.events) {    
    obsbinconca <- as.vector(obsbin[ibins, ,])
    hindcastprobconca <- as.vector(hindcastprob[ibins, ,])  
    # nodos sin NAs
    indnona = unique(c(which(!is.na(obsbinconca)), which(!is.na(hindcastprobconca))))  
    aux <- verify(obsbinconca[indnona], 
                  hindcastprobconca[indnona], 
                  obs.type = "binary", frcst.type = "prob",
                  thresholds = seq(0, 1, 1/n.bins), show = FALSE)
    aux$n <- length(length(indnona))
    eval(parse(text = sprintf("dataRelDiagram$cat%d = aux",ibins)))   
    
  }
  
  rm(obsbin, obsbinconca, hindcastprob, hindcastprobconca)
  
  return(dataRelDiagram)
}

#End


#' @title internal function concatenateDataRelDiagram_v2
#' 
#' @description Function to convert a vector of data into a matrix
#' 
#' @param data 1*m (m = grid points) vector of data
#' @param coordinates m*2 (m = grid points) 2D-matrix of grid points. First (second) column corresponds to longitudes (latitudes)
#'
#' @return A list with three elements:
#' data: mx*my (mx = longitudes, my = latitudes) 2D-matrix of data
#' xnod: vector (length mx) of longitudes
#' ynod: vector (length my) of latitudes
#'
#' @author R. Manzanas \& M.Iturbide
#' @keywords internal

vector2matrix <- function(data, coordinates) {
  xnod <- sort(unique(coordinates[, 1]))
  ynod <- sort(unique(coordinates[, 2]))
  
  new.data <- matrix(NA, length(xnod), length(ynod))
  
  for (ixnod in 1:length(xnod)) {
    for (iynod in 1:length(ynod)) { 
      indnod <- which(coordinates[, 1] == xnod[ixnod] & coordinates[, 2] == ynod[iynod])    
      if (length(indnod) > 0) {
        new.data[ixnod, iynod] <- data[indnod]
      }
    }
  }
  new.data <- list(data = new.data, xnod = xnod, ynod = ynod)
  return(new.data) 
}
