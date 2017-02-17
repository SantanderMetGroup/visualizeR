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

#' @title Reliability categories of a probabilistic prd.
#' 
#' @description Calculates (and draws) 
#' reliability diagrams and the related reliability categories, according to Weisheimer et al. 2014.
#' 
#' @param obs Grid of observations
#' @param prd Grid of forecast data
#' @param regions SpatialPolygons* object \code{\link[sp]{SpatialPolygons}}. delimiting the regions for which
#' the relaiability is calculated. Default is NULL (See details).
#' @param nbins Number of categories considered (e.g. 3 for terciles). By default nbins = 3
#' @param labels Character of the names to be given at the categories defined in \code{nbins} 
#' (e.g. c("lower", "middle", "upper")). If NULL (default) numbered categories are returned 
#' (Category 1 corresponds to the lowest values).
#' @param nbinsprob (optional): number of probability bins considered. By default nbinsprob = 10
#' @param nboot number of samples considered for bootstrapping. By default nboot = 100
#' @param sigboot Optional. Confidence interval for the reliability line. By default sigboot = 0.1 (two sided)
#' @param diagrams Logical (default = TRUE). Plotting results.  
#' @param cex0 numeric (default is 0.5). Minimum size of the points shown in the reliability diagrams, i.e. size of the point 
#' for the minimum n frequency (n = 1) (see parameter \code{nbinsprob}.  The sizes for points that correspond to n > 1 
#' are reescaled according to parameter \code{cex.scale}.
#' @param cex.scale numeric (default is 10). Scaling factor for points sizes in the reliability diagrams (see parameter \code{cex0}) 
#' @param layout integer (default = c(1, nbins)). Sets the layout of panels (rows,cols)
#' @param return.diagrams Logical. Available when \code{diagrams = TRUE}. If TRUE a trellis object for plotting diagrams is returned.
# @param nod Required if diagrams = TRUE. m*2 matrix of coordinates (m=locations, column1=latitude, column2=longitude)
# @param xlim Required if diagrams = TRUE. Limits for maps
# @param ylim Required if diagrams = TRUE. Limits for maps

#' @details If parameter regions is NULL (default) the whole region in \code{obs} and \code{prd} is considered 
#' for computing reliability. A subregion will be considered If the corresponding SpatialPolygons* object
#' is provided. In these cases, if parameter \code{diagrams = TRUE}, reliability diagrams are plotted for 
#' each specified category. If a SpatialPolygons* object of multiple subregions is provided, reliability is computed 
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
#' rel.reg <- reliabilityCategories(obs = tas.ncep2, prd = tas.cfs2.int, 
#'                                  nbinsprob = 5, nboot = 10, 
#'                                  regions = PRUDENCEregions)
#' rel <- reliabilityCategories(obs = tas.ncep2, prd = tas.cfs2.int, 
#'                              nbinsprob = 5, nboot = 10)
#' }
#' 
#' @export
#' @author R. Manzanas \& M.Iturbide
# @import verification
# @import plot3D
#' @importFrom transformeR getGrid redim getDim
#' @importFrom sp SpatialPoints SpatialPolygons Polygons Polygon over
#' @importFrom graphics plot.new abline polygon text grid title
#' @import lattice
#' @references Weisheimer, A., Palmer, T.N., 2014. On the reliability of seasonal climate forecasts. Journal of The Royal Society Interface 11, 20131162. doi:10.1098/rsif.2013.1162




reliabilityCategories <- function(obs,
                                  prd,
                                  regions = NULL,
                                  nbins = 3,
                                  labels = NULL,
                                  nbinsprob = 10,
                                  nboot = 100,
                                  sigboot = 0.1, 
                                  diagrams = TRUE,
                                  cex0 = 0.5,
                                  cex.scale = 10,
                                  layout = c(1,nbins),
                                  return.diagrams = FALSE){ 
      if (!identical(getGrid(obs)$y, getGrid(prd)$y) | !identical(getGrid(obs)$x, getGrid(prd)$x)) {
            stop("obs and prd are not spatially consistent. Consider using function 'interpGrid' from package transformeR")
      }
      layout <- rev(layout)
      if(is.null(labels)) labels <- paste("Category", 1:nbins)
      red <- rgb(1, 0, 0, 1, names = "red", maxColorValue = 1)
      orange <- rgb(1, 0.65, 0.3, 1, names = "orange", maxColorValue = 1)
      yellow <- rgb(1, 1, 0, 1, names = "yellow", maxColorValue = 1)
      darkyellow <- rgb(0.8, 0.8, 0, 1, names = "dark_yellow", maxColorValue = 1)
      cyan <- rgb(0, 1, 1, 1, names = "cyan", maxColorValue = 1)
      green <- rgb(0, 1, 0, 1, names = "green", maxColorValue = 1)
      
      regs <- regions
      if(is.null(regs)){
            bbox <- getGrid(obs) 
            esquinas <- matrix(ncol = 2, nrow = 5)
            esquinas[1,] <- c(bbox$x[1], bbox$y[1])
            esquinas[2,] <- c(bbox$x[1], bbox$y[2])
            esquinas[3,] <- c(bbox$x[2], bbox$y[2])
            esquinas[4,] <- c(bbox$x[2], bbox$y[1])
            esquinas[5,] <- c(bbox$x[1], bbox$y[1])
            regs <- sp::SpatialPolygons(list(sp::Polygons(list(Polygon(list(esquinas))), ID = "region")))
      }
      if(class(regs) == "SpatialPolygonsDataFrame"){
            regs <-  SpatialPolygons(regs@polygons)
      }
      prd <- redim(prd)
      memind <- which(getDim(prd)=="member")
      timeind <- which(getDim(prd)=="time")
      nmem <- dim(prd$Data)[memind]
      ntime <- dim(prd$Data)[timeind]
      coordinates <- expand.grid(obs$xyCoords$y, obs$xyCoords$x)
      coords <- as.data.frame(cbind(coordinates[,2], coordinates[,1]))
      spoints <- SpatialPoints(coords)
      suppressMessages(
            obs.fin <- transformeR::climatology(obs)
      )
      ob.full <- array3Dto2Dmat(obs$Data)
      ob.clim <- array(dim = c(nbins, dim(ob.full)[-1]))
      lower <- array(dim = c(length(regs), (nbins)))
      rownames(lower) <- names(regs)
      upper <- array(dim = c(length(regs), (nbins)))
      rownames(upper) <- names(regs)
      sl <- array(dim = c(length(regs), (nbins)))
      rownames(sl) <- names(regs)
      ob.slope <- list("sl" = sl, "lower" = lower, "upper" = upper)
      ob.catname  <- array(dim = c(length(regs), (nbins)))
      rownames(ob.catname) <- names(regs)
      for(l in 1:length(regs)){
            o <- over(spoints, regs[l,])
            w <- which(!is.na(o))
            if(length(w)>0){
                  ob <- ob.full[, w]
                  se <- array(dim = c(nmem, ntime, length(w)))
                  for(i in 1:nmem){
                        prdarray <- prd$Data[i,,,]
                        attr(prdarray, "dimensions") <-  attr(prd$Data, "dimensions")[-memind]
                        se[i,,] <- array3Dto2Dmat(prdarray)[, w]
                  }
                  #remove empty pixels
                  naind.obs <- which(is.na(ob[1,]))
                  naind.prd <- which(is.na(se[1,1,]))
                  naind <- unique(c(naind.obs, naind.prd))
                  if(length(naind) != 0){
                        obna <- ob[,-naind]
                        sena <- se[,,-naind]
                  }else{
                        obna <- ob
                        sena <- se
                  }
                  message("[", Sys.time(), "] Calculating categories for region ", l, " out of ", length(regs))
                  sl <- calculateReliability(obs = obna, prd = sena, nbins = nbins, nbinsprob = nbinsprob, nboot = nboot)
                  
                  n <- sl$n
                  nyear <- sl$nyear
                  npoint <- sl$npoint
                  slope <- sl$slope
                  slope_boot <- sl$slope_boot
                  prdprob <- sl$prdprob
                  obsfreq <- sl$obsfreq
                  prdfreq <- sl$prdfreq
                  
                  cat <- rep(NA, nbins)
                  catcol <- rep(NA, nbins)
                  catname <- rep("", nbins)
                  
                  for (ibins in 1:nbins) {
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
                              } else if (slope[ibins] >= 0.5 & slope_lower > 0 & slope_upper <= 1) {         
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
      l.obs.fin <- list()
      for(i in 1:nbins){
            obs.fin$Data <- mat2Dto3Darray(as.matrix(ob.clim[i, , drop = F]), x = obs$xyCoords$x, y = obs$xyCoords$y)
            suppressMessages(
                  cats <- transformeR::climatology(obs.fin)
            )
            attr(cats$Variable, "longname") <- labels[i]
            l.obs.fin[[i]] <- cats
      }
      mg <- makeMultiGrid(l.obs.fin)
      if (diagrams) {
            if(length(regs) > 1){
                  pc <- transformeR::plotClimatology(mg,  backdrop.theme = "countries", at = c(0.5,1.5,2.5,3.05,3.55,4.5,5.5), 
                                                     col.regions = c(red, orange, yellow, darkyellow, cyan, green),
                                                     layout = layout,
                                                     colorkey = list(labels = list( 
                                                           cex = 1,
                                                           at = c(1, 2, 2.75, 3.25, 4, 5), 
                                                           labels = c("dangerously useless", "not useful","marginally useful",
                                                                      "marginally useful +","still useful","perfect"))))
                  
                  print(pc)
            }else{
                  x1 <- 1/nbins
                  y1 <- 1/nbins
                  x2 <- 1
                  y2 <- (1/nbins) + (0.5*(1-(1/nbins)))
                  a <- (y2-y1)/(x2-x1)
                  b <- y1-((x1*(y2-y1))/(x2-y1))
                  
                  y <- unlist(obsfreq)
                  x <- unlist(prdprob)
                  z <- rep(1:nbins, each = nbinsprob)
                  w <- rep(labels, each = nbinsprob)
                  
                  a_lower <- ob.slope$lower #slope_lower 
                  b_lower <-(1-ob.slope$lower)/nbins
                  ## upper bound
                  a_upper <- ob.slope$upper
                  b_upper <- (1-ob.slope$upper)/nbins
                  
                  
                  # Customized Lattice Example
                  xyp <- xyplot(y~x|w, scales=list(x = list(at = seq(0,1,round(1/nbinsprob, digits = 2)),
                                                     labels = seq(0,1,round(1/nbinsprob, digits = 2))),
                                            y = list(at = seq(0,1,round(1/nbinsprob, digits = 2)),
                                                     labels = seq(0,1,round(1/nbinsprob, digits = 2))),
                                            
                                            cex=.8, col="black"),
                         panel=function(x, y, w, z, ...) {
                               # panel.locfit(...)
                               
                               panel.grid(h = -1, v = -1)
                               panel.polygon(c(0, 1/nbins, 1/nbins, 0), c(0, 0, 1/nbins, b),
                                             border = NA, col = "lightgray")
                               panel.polygon(c(1/nbins, 1, 1, 1/nbins), c(1/nbins, y2, 1, 1),
                                             border = NA, col = "lightgray")
                               panel.abline(coef = c(b, a), panel.number(prefix = labels[1]))
                               panel.abline(c(0, 1),  col = "black", lty = 3, lwd = 1.5)
                               panel.abline(c(0, 1),  col = "black", lty = 3, lwd = 1.5)
                               panel.abline(c(0, 1),  col = "black", lty = 3, lwd = 1.5)
                               panel.abline(h = 1/nbins, v = 1/nbins, col = "black", lty = 3)
                               for(i in 1:nbins){
                                     if(packet.number() == i){
                                           panel.polygon(c(0, 1/nbins, 0, 0), c(b_lower[,i], 1/nbins, b_upper[,i], b_lower[,i]),
                                                         border = NA, col = catcol[i])
                                           panel.polygon(c(1/nbins, 1, 1, 1/nbins), c(1/nbins, a_lower[,i]+b_lower[,i], a_upper[,i]+b_upper[,i], 1/nbins),
                                                         border = NA, col = catcol[i])
                                           panel.abline(b_lower[,i], a_lower[,i], col = "gray40", lty = 2, lwd = 1)
                                           panel.abline(b_upper[,i], a_upper[,i], col = "gray40", lty = 2, lwd = 1)
                                           panel.abline((1-slope[i])/nbins, slope[i], col = "gray40", lwd = 2)
                                           panel.text(0.35, 0.75, catname[i])
                                           
                                           panel.xyplot(x, y, pch = 16, col = "black", 
                                                       cex = ((((prdfreq[[i]]*nyear*npoint)-cex0)*(cex0*cex.scale-cex0)) / ((nyear*npoint)-cex0)) + cex0)
                                     }
                               }
                               if(packet.number() == 1){
                                     panel.xyplot(0.75,0.2, pch = 16, col = "black", cex = cex0)
                                     panel.text(0.85, 0.2, "n = 1")
                               }
                               
                               
                         },
                         
                         layout = layout,
                         xlab = "Predicted probability", ylab= "Observed frequency",
                         main= list(cex = 1, font = 1, label = sprintf("n = %d years x %d points", nyear, npoint)))
                  print(xyp)
                  # update(xyp, par.settings = list(fontsize = list(text = 8, points = 10)))
                  
                  #########################################################
                  
                  
                  # par(mfrow = c(1, nbins), pty="s", mgp=c(2,1,0), mar=c(1,3,2,2), oma=c(2,0,2,0))
                  # for(i in 1:nbins){
                  #       ## reliability diagram
                  #       x1 <- 1/nbins
                  #       y1 <- 1/nbins
                  #       x2 <- 1
                  #       y2 <- (1/nbins) + (0.5*(1-(1/nbins)))
                  #       a <- (y2-y1)/(x2-x1)
                  #       b <- y1-((x1*(y2-y1))/(x2-y1))
                  #       
                  #       plot(b, a, col = "black", lty = 3, typ = "l",
                  #            xlim = c(0,1), ylim = c(0,1),
                  #            xlab = "prd prob.", ylab = "obs. freq.",
                  #            main = labels[i], 
                  #            sub = list(catname[i], cex = 1.2),
                  #            font.sub=4)
                  #       abline(b, a, col = "black", lty = 3)
                  #       polygon(c(0, 1/nbins, 1/nbins, 0), c(0, 0, 1/nbins, b),
                  #               border = NA, col = "lightgray")
                  #       polygon(c(1/nbins, 1, 1, 1/nbins), c(1/nbins, y2, 1, 1),
                  #               border = NA, col = "lightgray")
                  #       abline(0, 1,  col = "black", lty = 3, lwd = 1.5)
                  #       abline(h = 1/nbins, col = "black", lty = 3)
                  #       abline(v = 1/nbins, col = "black", lty = 3)  
                  #       
                  #       ## intervalo de confianza para la pendiente
                  #       ## lower bound
                  #       a_lower <- ob.slope$lower[ , i] #<- #slope_lower 
                  #       b_lower <- (1-ob.slope$lower[ , i])/nbins
                  #       ## upper bound
                  #       a_upper <- ob.slope$upper[ , i]
                  #       b_upper <- (1-ob.slope$upper[ , i])/nbins
                  #       polygon(c(0, 1/nbins, 0, 0), c(b_lower, 1/nbins, b_upper, b_lower),
                  #               border = NA, col = catcol[i])
                  #       polygon(c(1/nbins, 1, 1, 1/nbins), c(1/nbins, a_lower+b_lower, a_upper+b_upper, 1/nbins),
                  #               border = NA, col = catcol[i])
                  #       abline(b_lower, a_lower, col = "black", lty = 2, lwd = 2)
                  #       abline(b_upper, a_upper, col = "black", lty = 2, lwd = 2)
                  #       abline((1-slope[i])/nbins, slope[i], col = "black", lwd = 2)
                  #       
                  #       ## puntos del reliability diagram (escalados por el peso)
                  #       points(0.1, .8, pch = 19, cex = cex0)
                  #       text(0.15, .8, "n = 1", cex=.95, font=2, pos=4)
                  #       points(prdprob[[i]], obsfreq[[i]], pch = 19, 
                  #       cex = ((((prdfreq[[i]]*nyear*npoint)-cex0)*(cex0*10-cex0)) / ((nyear*npoint)-cex0)) + cex0)
                  #             grid(nx = NULL, ny = NULL, col = "lightgray", lty = 4, lwd = 0.5)
                  # }
                  # title(sprintf("n = %d years x %d points", nyear, npoint), outer = T)
                  ############################################################
                  
            }
      }
      result.grid <- mg
      attr(result.grid$Data, "dimensions") <- c("cat", "var", "member", "time", "lat", "lon")
      attr(result.grid$Data, "climatology:fun") <- NULL
      result <- list()
      colnames(ob.catname) <- attr(mg$Variable, "longname")
      result$catname <- ob.catname
      colnames(ob.slope$sl) <- attr(mg$Variable, "longname")
      colnames(ob.slope$lower) <- attr(mg$Variable, "longname")
      colnames(ob.slope$upper) <- attr(mg$Variable, "longname")
      result$slope <- ob.slope
      message("[", Sys.time(), "] Done.")
      result.grid$ReliabilityCategories <- result
      attr(result.grid$ReliabilityCategories, "observations") <- attr(obs, "dataset")
      result.grid$Variable$varName <- result.grid$Variable$varName[1]
      result.grid$Variable$level <- NA
      attr(result.grid$Variable, "units") <- attr(result.grid$Variable, "units")[1]
      attr(result.grid$Variable, "use_dictionary") <- attr(result.grid$Variable, "use_dictionary")[1]
      attr(result.grid$Variable, "description") <- attr(result.grid$Variable, "description")[1]
      attr(result.grid$Variable, "daily_agg_cellfun") <- attr(result.grid$Variable, "daily_agg_cellfun")[1]
      attr(result.grid$Variable, "monthly_agg_cellfun") <- attr(result.grid$Variable, "monthly_agg_cellfun")[1]
      attr(result.grid$Variable, "verification_time") <- attr(result.grid$Variable, "verification_time")[1]
      attr(result.grid$Variable, "annual_agg_cellfun") <- attr(result.grid$Variable, "annual_agg_cellfun")[1]
      attr(result.grid$Variable, "longname") <- NULL
      if(return.diagrams & diagrams){
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
#' @param prd m*n*l matrix of predictions (m = members, n = years, l = locations)
#' @param nbins (optional): number of categories considered (e.g. 3 for terciles). By default nbins = 3
#' @param nbinsprob (optional): number of probability bins considered. By default nbinsprob = 10
#' @param nboot number of samples considered for bootstrapping. By default nboot = 100
#' @param sigboot sigboot
#'
#' @return List with the following elements:
#' nbins = nbins
#' nyear = number of years
#' npoint = number of locations
#' n = nyear*npoint
#' prdprob = probability bins (center), per category (e.g. per tercile)
#' obsfreq = observed frequency, per category (e.g. per tercile)
#' prdfreq = predicted frequency, per category (e.g. per tercile)
#' slope = slope of the reliability line, per category (e.g. per tercile)
#' slope_boot = nboot*nbins matrix, with all the boostrapped values for the slope of the reliability line 
#' 
#' @author R. Manzanas \& M.Iturbide
#' @importFrom abind abind
#' @importFrom transformeR makeMultiGrid
#' @import verification
#' @keywords internal





calculateReliability <- function(obs, prd, nbins = nbins, nbinsprob = nbinsprob, nboot = nboot) {
      if (!(dim(obs)[1] == dim(prd)[2] & dim(obs)[2] == dim(prd)[3])) {
            stop("Observations and predictions are not congruent in size")
      }
      nyear <- dim(obs)[1]
      nmemb <- dim(prd)[1]
      O <- obs2bin(obs, nbins)
      P <- prd2prob(prd, nbins)
      ## calculo puntos diagrama fiabilidad
      aux <- concatenateDataRelDiagram_v2(O$bin, P$prob, nbinsprob) 
      nbins <- aux$nbins
      nyear <- aux$nyear
      npoint <- aux$npoint
      n <- aux$n
      prdprob <- vector("list", nbins)
      obsfreq <- vector("list", nbins)
      prdfreq <- vector("list", nbins)
      slope <- rep(NA, nbins)
      # intercept <- rep(NA, 1, nbins)
      for (ibins in 1:nbins) {
            prdprob.bin <- eval(parse(text = sprintf("aux$cat%d$y.i", ibins)))
            prdprob[[ibins]] <- prdprob.bin
            obsfreq.bin <- eval(parse(text = sprintf("aux$cat%d$obar.i", ibins)))
            obsfreq[[ibins]] <- obsfreq.bin
            # PESOS prdfreq!!!
            prdfreq.bin <- eval(parse(text = sprintf("aux$cat%d$prob.y", ibins)))
            prdfreq[[ibins]] <- prdfreq.bin
            
            if (!(is.null(prdprob.bin) | is.null(obsfreq.bin) | is.null(prdfreq.bin))) {
                  fit <- lm(obsfreq.bin ~ prdprob.bin, weights = prdfreq.bin)
                  slope[ibins] <- fit$coefficients[[2]]
                  # intercept[ibins] <- fit$coefficients[[1]]
            } else {
                  slope[ibins] <- NA
                  # intercept[ibins] <- NA
            }
      }
      
      ## bootstrapping
      slope_boot <- matrix(NA, nboot, nbins)
      # intercept_boot <- matrix(NA, nboot, nbins)
      message("...[", Sys.time(), "] Computing bootstrapping...")
      for (iboot in 1:nboot){
            #             if (abs(iboot/50 - round(iboot/50)) < eps()) {
            #                   print(sprintf("... computing bootstrapping %d ...", iboot))
            #             }
            
            indmembperm <- sample(1:nmemb, nmemb, replace = TRUE)
            indyearperm <- sample(1:nyear, nyear, replace = TRUE)
            indnodperm <- sample(1:dim(obs)[2], dim(obs)[2], replace = TRUE)
            
            P.prob.boot <- array(NA, c(nbins, nyear, dim(obs)[2]))
            Pcat.boot <- P$cat[indmembperm, indyearperm, indnodperm]
            for (inod in 1:dim(obs)[2]) {
                  for (ibins in 1:nbins) {
                        P.prob.boot[ibins, , inod] <- apply(Pcat.boot[, , inod] == ibins, 2, sum) / nmemb
                        # P.prob.boot[ibins, , inod] <- apply(P$cat[, , inod] == ibins, 2, sum) / nmemb
                  }
            }
            
            # O <- obs2bin(obs[indyearperm, indnodperm], nbins)
            # P <- prd2prob(prd[indmembperm, indyearperm, indnodperm], nbins)
            
            aux <- concatenateDataRelDiagram_v2(O$bin[, indyearperm, indnodperm], P.prob.boot, nbinsprob)  
            # aux <- concatenateDataRelDiagram_v2(O$bin, P$prob, nbinsprob)  
            for (ibins in 1:nbins) {   
                  
                  prdprob.bin <- eval(parse(text = sprintf("aux$cat%d$y.i", ibins)))
                  obsfreq.bin <- eval(parse(text = sprintf("aux$cat%d$obar.i", ibins)))
                  # PESOS prdfreq!!!
                  prdfreq.bin <- eval(parse(text = sprintf("aux$cat%d$prob.y", ibins)))
                  
                  if (!(is.null(prdprob.bin) | is.null(obsfreq.bin) | is.null(prdfreq.bin))) {
                        fit <- lm(obsfreq.bin ~ prdprob.bin, weights = prdfreq.bin)
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
      # slope_lower <- matrix(NA, length(sigboot), nbins)
      # slope_upper <- matrix(NA, length(sigboot), nbins)
      # for (ibins in 1:nbins) {
      #   for (isigboot in 1:length(sigboot)) {
      #   aux <- quantile(slope_boot[, ibins], c(sigboot[isigboot], 1-sigboot[isigboot]))
      #   slope_lower[isigboot, ibins] <- aux[[1]]
      #   slope_upper[isigboot, ibins] <- aux[[2]]
      #   }
      # }
      
      result <- list()
      result$nbins <- nbins
      result$nyear <- nyear
      result$npoint <- npoint
      result$n <- n
      result$prdprob <- prdprob
      result$obsfreq <- obsfreq
      result$prdfreq <- prdfreq
      result$slope <- slope
      # result$intercept <- intercept
      result$slope_boot <- slope_boot
      # result$intercept_boot <- intercept_boot
      # result$sigboot <- sigboot
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
#' @param nbins number of categories (3 for terciles)
#'
#' @return 
#' bincat: list with two elements:  
#' bin: 3D-array of binary (0/1) observations, dimensions = (nbins, time, npoints)
#' cat: 2D-matrix with the observed category, dimensions = (time, npoints)
#'
#' @author R. Manzanas \& M.Iturbide
#' @keywords internal


obs2bin <- function(obs, nbins){
      bin <- array(0, c(nbins, dim(obs)[1], dim(obs)[2]))
      cat <- array(NA, c(dim(obs)[1], dim(obs)[2]))
      v.err <- rep(NA, dim(obs)[2])
      for (inod in 1:dim(obs)[2]) {
            tryCatch({
                  ## categorias obs     
                  catsobs <- quantile(obs[, inod], 0:nbins/nbins, na.rm = TRUE)
                  auxobscat <- quantile2disc(obs[, inod], catsobs)
                  auxobscat$mids <- sort(auxobscat$mids)
                  
                  for (ibins in 1:nbins){
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
#' @param prd 3D-array of predictions, dimensions = (member, time, npoints)
#' @param prd4cats Optional. 3D-array of predictions for which calculate the categories (e.g., terciles)
#' @param dimensions (member, time, npoints)
#' @param nbins Number of categories (3 for terciles)
#'
#' @return 
#' prdprob: 3D-array of probabilistic predictions, dimensions = (nbins, time, npoints)
#' @note For prd4cats, categories are calculated at model- (not at member-) level
#' @author R. Manzanas \& M.Iturbide
#' @keywords internal

prd2prob <- function(prd, nbins, prd4cats = NULL){
      
      prob <- array(NA, c(nbins, dim(prd)[2], dim(prd)[3]))
      cat <- array(NA, c(dim(prd)[1], dim(prd)[2], dim(prd)[3]))
      v.err <- rep(NA, dim(prd)[3])
      for (inod in 1:dim(prd)[3]) {
            ## categorias prd
            ## calculo los terciles de la prediccion a nivel de modelo (concateno todos los miembros)      
            if (!is.null(prd4cats)) {
                  tmpprd4catscat <- lapply(1:dim(prd4cats)[1], function(x) prd4cats[x, , inod])      
                  tmpprd4catscat <- do.call("c", tmpprd4catscat)
            }      
            tmpprdcat <- lapply(1:dim(prd)[1], function(x) prd[x, , inod])
            tmpprdcat <- do.call("c", tmpprdcat)
            
            if (!is.null(prd4cats)) {
                  catsprd <- quantile(tmpprd4catscat, 0:nbins/nbins, na.rm = TRUE) 
                  rm(tmpprd4catscat)
            } else {
                  catsprd <- quantile(tmpprdcat, 0:nbins/nbins, na.rm = TRUE) 
            }
            tryCatch({
                  catsprd <- quantile2disc(tmpprdcat, catsprd)
                  rm(tmpprdcat)
                  catsprd$mids <- sort(catsprd$mids)
                  auxprd <- matrix(NA, dim(prd)[2], dim(prd)[1])
                  for (imemb in 1:dim(prd)[1]){
                        i1 <- ((imemb-1)*dim(prd)[2]) + 1
                        i2 <- i1 + dim(prd)[2] - 1
                        auxprd[, imemb] <- catsprd$new[i1:i2]
                  }  
                  tmp2 <- matrix(NA, dim(prd)[1], dim(prd)[2])
                  for (ibins in 1:nbins){      
                        tmp <- auxprd == catsprd$mids[ibins]
                        prob[ibins, , inod] <- apply(tmp, 1, sum) / dim(prd)[1]             
                        tmp2[t(tmp)] <- ibins 
                  }
                  cat[, , inod] <- tmp2
                  rm(tmp2)
            }, error = function(ex) {
                  v.err[inod] <- 1
                  nmemb<-dim(prd)[2]
                  ntime<-dim(prd)[1]
                  cat[, , inod] <- matrix(NA, nmemb, ntime)
            })
      }
      if(any(!is.na(v.err))){
            message("Imposible to calculate categories in the ", (sum(v.err, na.rm = T)/length(v.err))*100, " percent of the grid boxes in prd. Removed for the analysis.")
      }
      rm(auxprd)
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
#' @param nbins number of categories (3 for terciles)
#' @param nbinsprob nbinsprob
#'
#' @return 
#' bincat: list with two elements:  
#' bin: 3D-array of binary (0/1) observations, dimensions = (nbins, time, npoints)
#' cat: 2D-matrix with the observed category, dimensions = (time, npoints)
#' @import verification
#' @author R. Manzanas \& M.Iturbide
#' @keywords internal

concatenateDataRelDiagram_v2 <- function(obsbin, prdprob, nbinsprob) {
      # Description
      # 
      # Usage:
      # concatenateDataResDiagram(obsbin, prdprob, nod, nbins) 
      # Arguments:
      # obsbin: 4D-array of binary (0/1) observations, dimensions = (nbins, ynod, xnod, time)
      # prdprob: 4D-array of probabilistic predictions, dimensions = (nbins, ynod, xnod, time)
      # nod: matrix of coordinates, longitudes (latitudes) in first (second) column (common for obs and prd)
      # nbins: number of categories (3 for terciles)
      # Value:
      # dataRelDiagram: list with nbins elements, containing all the information to plot reliability diagrams 
      # (call the attribute.R function from verification R-package: 
      # http://cran.r-project.org/web/packages/verification/verification.pdf)
      
      #       require(verification)
      
      dataRelDiagram <- list()
      if (identical(as.vector(dim(obsbin)), as.vector(dim(prdprob)))){
            nbins <- dim(obsbin)[1]
            nyear <- dim(obsbin)[2]
            npoint <- dim(obsbin)[3]
            n <- nyear*npoint
      }
      dataRelDiagram$nbins <- nbins
      dataRelDiagram$nyear <- nyear
      dataRelDiagram$npoint <- npoint
      dataRelDiagram$n <- n
      
      for (ibins in 1:nbins) {    
            obsbinconca <- as.vector(obsbin[ibins, ,])
            prdprobconca <- as.vector(prdprob[ibins, ,])  
            # nodos sin NAs
            indnona = unique(c(which(!is.na(obsbinconca)), which(!is.na(prdprobconca))))  
            aux <- verify(obsbinconca[indnona], 
                          prdprobconca[indnona], 
                          obs.type = "binary", frcst.type = "prob",
                          thresholds = seq(0, 1, 1/nbinsprob), show = FALSE)
            aux$n <- length(length(indnona))
            eval(parse(text = sprintf("dataRelDiagram$cat%d = aux",ibins)))   
            
      }
      
      rm(obsbin, obsbinconca, prdprob, prdprobconca)
      
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
