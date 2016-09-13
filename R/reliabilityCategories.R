
#' @title Reliability categories of a probabilistic prediction.
#' 
#' @description This function calculates (and draws) 
#' reliability diagrams and the related reliability categories, according to Weisheimer et al. 2014:
#' http://rsif.royalsocietypublishing.org/content/11/96/20131162
#' 
#' @param obs Grid of observations
#' @param prd Grid of predictions 
#' @param nbins (optional): number of categories considered (e.g. 3 for terciles). By default nbins = 3
#' @param nbinsprob (optional): number of probability bins considered. By default nbinsprob = 10
#' @param nboot number of samples considered for bootstrapping. By default nboot = 100
#' @param sigboot Optional. Confidence interval for the reliability line. By default sigboot = 0.05
#' @param diagrams Logical (default = TRUE). Plotting results.  
#' @param nod Required if diagrams = TRUE. m*2 matrix of coordinates (m=locations, column1=latitude, column2=longitude)
#' @param xlim Required if diagrams = TRUE. Limits for maps
#' @param ylim Required if diagrams = TRUE. Limits for maps
#' @return Same as prd but with an aditional list ($ReliabilityCategories) containing the following elements:
#' catcol: color of the reliability category
#' catname: reliability category
#' sl: slope of the reliability line
#' sl_lower: lower bound confidence for slope (according to "sigboot") 
#' sl_lower: upper bound confidence for slope (according to "sigboot") 
#' @export
#' @author R. Manzanas \& M.Iturbide
# @import verification
# @import plot3D
#' @import maps 





reliabilityCategories <- function(obs, prd,  nbins = 3, nbinsprob = 100, nboot = 100, sigboot = 0.05, 
                                  diagrams = TRUE){ #, regions = NULL) {
      if(!identical(getGrid(obs)$y, getGrid(prd)$y) | !identical(getGrid(obs)$x, getGrid(prd)$x)){
            stop("obs and prd are not spatially consistent. Try using function interpGrid from package downscaleR")
      }
      xlim <- getGrid(prd)$x
      ylim <- getGrid(prd)$y
      prd <- downscaleR:::redim(prd)
      coordinates <- expand.grid(obs$xyCoords$y, obs$xyCoords$x)
      coordinates <- cbind(coordinates[,2], coordinates[,1])
      ob <- array3Dto2Dmat(obs$Data)
      memind <- which(downscaleR:::getDim(prd)=="member")
      timeind <- which(downscaleR:::getDim(prd)=="time")
      
      
      nmem <- dim(prd$Data)[memind]
      ntime <- dim(prd$Data)[timeind]
      se <- array(dim = c(nmem, ntime, length(prd$xyCoords$x) * length(prd$xyCoords$y)))
      for(i in 1:nmem){
            prdarray <- prd$Data[i,,,]
            attr(prdarray, "dimensions") <-  attr(prd$Data, "dimensions")[-memind]
            se[i,,] <- array3Dto2Dmat(prdarray)
      }
      naind <- which(is.na(ob[1,]))
      obna0 <- ob[,-naind]
      sena0 <- se[,,-naind]
      naind <- which(is.na(sena0[1,1,]))
      obna <- obna0[,-naind]
      sena <- sena0[,,-naind]
      
      #       corna <- unname(as.matrix(coordinates[-naind,]))
      sl <- calculateReliability(obna, sena, nbins = nbins, nbinsprob = nbinsprob, nboot = nboot, sigboot = sigboot)
      message("[", Sys.time(), "] Calculating categories...")
      ## colores
      red <- rgb(1, 0, 0, 1, names = "red", maxColorValue = 1)
      orange <- rgb(1, 0.65, 0.3, 1, names = "orange", maxColorValue = 1)
      yellow <- rgb(1, 1, 0, 1, names = "yellow", maxColorValue = 1)
      dark_yellow <- rgb(0.8, 0.8, 0, 1, names = "dark_yellow", maxColorValue = 1)
      cyan <- rgb(0, 1, 1, 1, names = "cyan", maxColorValue = 1)
      green <- rgb(0, 1, 0, 1, names = "green", maxColorValue = 1)
      
      n <- sl$n
      nyear <- sl$nyear
      npoint <- sl$npoint
      slope <- sl$slope
      slope_boot <- sl$slope_boot
      prdprob <- sl$prdprob
      obsfreq <- sl$obsfreq
      prdfreq <- sl$prdfreq
      
      cat <- rep(NA, 1, nbins)
      catcol <- rep(NA, 1, nbins)
      catname <- rep("", 1, nbins)
      sl <- rep(NA, 1, nbins)
      sl_lower <- rep(NA, 1, nbins)
      sl_upper <- rep(NA, 1, nbins)
      
      if (diagrams) {
            par(mfcol = c(2, nbins)) 
      }
      for (ibins in 1:nbins) {
            sl[ibins] <- slope[ibins]
            
            aux <- quantile(slope_boot[, ibins], c(sigboot, 1-sigboot))
            slope_lower <- aux[[1]]
            slope_upper <- aux[[2]]
            rm(aux)
            
            sl_lower[] <- slope_lower
            sl_upper[] <- slope_upper
            
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
                        catcol[ibins] <- dark_yellow  
                        catname[ibins] <- "marginally useful*"
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
                        catname[ibins] <- "dangerous"
                  } 
            }
            obsplot <- obs 
            obsplot$Data[which(!is.na(obsplot$Data))] <- cat[ibins]
            if (diagrams) {
                  #                   ## mapa
                  #                   aux <- rep(NA, nrow(coordinates))
                  #                   aux[-naind] <- cat[ibins]
                  #                   aux <- cbind(aux, coordinates)
                  #                   
                  #                   if (!is.null(xlim) & !is.null(ylim)) {
                  #                         image(aux[,2], aux[,3], aux[,1], xlim = xlim, 
                  #                               ylim = ylim, col = catcol[ibins], 
                  #                               main = sprintf("category %d (%s)", ibins, catname[ibins]), xlab = "lon",  ylab = "lat")
                  #                   } else if (is.null(xlim) | is.null(ylim)) {
                  #                         image(aux$xnod, aux$ynod, aux$data, xlim = c(min(corna[,1]), max(corna[,1])), 
                  #                               ylim = c(min(corna[,2]), max(corna[,2])), col = catcol[ibins], 
                  #                               main = sprintf("category %d (%s)", ibins, catname[ibins]), xlab = "lon",  ylab = "lat")
                  #                   }
                  #                   map(add=T)
                  #                   grid(col = "lightgray", lty = 2, lwd = 0.1)
                  image(obsplot$xyCoords$x,obsplot$xyCoords$y, t(obsplot$Data[1,,]), 
                        xlim = xlim, 
                        ylim = ylim, col = catcol[ibins], 
                        main = sprintf("category %d (%s)", ibins, catname[ibins]), xlab = "lon",  ylab = "lat")
                  map(add=T)
                  ## reliability diagram
                  x1 <- 1/nbins
                  y1 <- 1/nbins
                  x2 <- 1
                  y2 <- (1/nbins) + (0.5*(1-(1/nbins)))
                  a <- (y2-y1)/(x2-x1)
                  b <- y1-((x1*(y2-y1))/(x2-y1))
                  
                  plot.new()
                  abline(b, a, col = "black", lty = 3)
                  polygon(c(0, 1/nbins, 1/nbins, 0), c(0, 0, 1/nbins, b),
                          border = NA, col = "lightgray")
                  polygon(c(1/nbins, 1, 1, 1/nbins), c(1/nbins, y2, 1, 1),
                          border = NA, col = "lightgray")
                  abline(0, 1,  col = "black", lty = 3, lwd = 1.5)
                  abline(h = 1/nbins, col = "black", lty = 3)
                  abline(v = 1/nbins, col = "black", lty = 3)  
                  
                  ## intervalo de confianza para la pendiente
                  ## lower bound
                  a_lower <- slope_lower      
                  b_lower <- (1-slope_lower)/nbins
                  rm(slope_lower)
                  ## upper bound
                  a_upper <- slope_upper
                  b_upper <- (1-slope_upper)/nbins
                  rm(slope_upper)
                  
                  polygon(c(0, 1/nbins, 0, 0), c(b_lower, 1/nbins, b_upper, b_lower),
                          border = NA, col = catcol[ibins])
                  polygon(c(1/nbins, 1, 1, 1/nbins), c(1/nbins, a_lower+b_lower, a_upper+b_upper, 1/nbins),
                          border = NA, col = catcol[ibins])
                  abline(b_lower, a_lower, col = "black", lty = 2, lwd = 2)
                  abline(b_upper, a_upper, col = "black", lty = 2, lwd = 2)
                  abline((1-slope[ibins])/nbins, slope[ibins], col = "black", lwd = 2)
                  
                  ## puntos del reliability diagram (escalados por el peso)
                  par(new = TRUE)
                  plot(c(0.1, 0.1), c(0.65, 0.8), pch = 19, cex = c(1,10), xlim = c(0,1), ylim = c(0,1),
                       xlab = "forecast prob.", ylab = "obs. freq.") 
                  #                   plot(c(0.1, 0.1), c(0.65, 0.8), pch = 19, cex = c(1,10), xlim = c(0,1), ylim = c(0,1),
                  #                        xlab = "forecast prob.", ylab = "obs. freq.") !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!cambiar tamaños -> frequencia del max y min
                  text(0.2, 0.8, sprintf("n=%d", n), cex=1.25, font=2, pos=4)
                  text(0.2, 0.65, "n=1", cex=1.25, font=2, pos=4)
                  par(new = TRUE)
                  plot(prdprob[[ibins]], obsfreq[[ibins]], pch = 19, 
                       cex = ((((prdfreq[[ibins]]*nyear*npoint)-1)*(10-1)) / ((nyear*npoint)-1)) + 1,
                       xlim = c(0,1), ylim = c(0,1),
                       xlab = "forecast prob.", ylab = "obs. freq.")  
                  grid(nx = NULL, ny = NULL, col = "lightgray", lty = 4, lwd = 0.5)
                  title(sprintf("%d data, %d points", nyear, npoint))
            }  
      }
      result <- list()
      # result$cat <- cat
      result$catcol <- catcol
      result$catname <- catname
      result$sl <- sl
      result$sl_lower <- sl_lower
      result$sl_upper <- sl_upper
      message("[", Sys.time(), "] Done.")
      result.grid <- prd
      result.grid$ReliabilityCategories <- result
      attr(result.grid$ReliabilityCategories, "observationData") <- attr(obs, "dataset")
      return(result.grid)
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
#' @param sigboot
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
#' @references
#' Lawson, B.D. & Armitage, O.B., 2008. Weather guide for the Canadian Forest Fire Danger Rating System. Northern Forestry Centre, Edmonton (Canada).
#' 
#' van Wagner, C.E., 1987. Development and structure of the Canadian Forest Fire Weather Index (Forestry Tech. Rep. No. 35). Canadian Forestry Service, Ottawa, Canada.
#' 
#' van Wagner, C.E., Pickett, T.L., 1985. Equations and FORTRAN program for the Canadian forest fire weather index system (Forestry Tech. Rep. No. 33). Canadian Forestry Service, Ottawa, Canada.
#' 
#' @author R. Manzanas \& M.Iturbide
#' @importFrom abind abind
#' @importFrom downscaleR makeMultiGrid
# @import verification



# Description:
# This function provides the object needed by "calculateReliability_v2.R" 
# for calculating the reliability categories of a probabilistic prediction.
# Usage:
# caculateReliability(obs, prd, varargin) 
# Arguments:
# obs: m*n matrix of observations (m = years, n = locations)
# prd: m*n*l matrix of predictions (m = members, n = years, l = locations)
# nbins (optional): number of categories considered (e.g. 3 for terciles). By default nbins = 3
# nbinsprob (optional): number of probability bins considered. By default nbinsprob = 10
# nboot: number of samples considered for bootstrapping. By default nboot = 100
# Value: List with the following elements:
# nbins = nbins
# nyear = number of years
# npoint = number of locations
# n = nyear*npoint
# prdprob = probability bins (center), per category (e.g. per tercile)
# obsfreq = observed frequency, per category (e.g. per tercile)
# prdfreq = predicted frequency, per category (e.g. per tercile)
# slope = slope of the reliability line, per category (e.g. per tercile)
# slope_boot = nboot*nbins matrix, with all the boostrapped values for the slope of the reliability line
##





calculateReliability <- function(obs, prd, nbins = 3, nbinsprob = 10, nboot = 100, sigboot = 0.05) {
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
      slope <- rep(NA, 1, nbins)
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
      message("[", Sys.time(), "] Computing bootstrapping...")
      for (iboot in 1:nboot){
            print(iboot)
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
      message("[", Sys.time(), "] Done.")
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


obs2bin <- function(obs, nbins){
      bin <- array(0, c(nbins, dim(obs)[1], dim(obs)[2]))
      cat <- array(NA, c(dim(obs)[1], dim(obs)[2]))
      
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
                  message("Imposible calcular categorias el punto de grid %d", inod)
            })
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

prd2prob <- function(prd, nbins, prd4cats = NULL){
      
      prob <- array(NA, c(nbins, dim(prd)[2], dim(prd)[3]))
      cat <- array(NA, c(dim(prd)[1], dim(prd)[2], dim(prd)[3]))
      
      for (inod in 1:dim(prd)[3]) {
            tryCatch({
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
                  message("Problemas en el punto de grid %d, imposible calcular categorias", inod)
            })
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
#'
#' @return 
#' bincat: list with two elements:  
#' bin: 3D-array of binary (0/1) observations, dimensions = (nbins, time, npoints)
#' cat: 2D-matrix with the observed category, dimensions = (time, npoints)
#' @import verification
#' @author R. Manzanas \& M.Iturbide

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
      
      if (identical(dim(obsbin), dim(prdprob))) {
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
            
            if (identical(which(is.na(obsbinconca)), which(is.na(prdprobconca)))) {
                  aux <- verify(obsbinconca[which(is.na(obsbinconca) == FALSE)], 
                                prdprobconca[which(is.na(prdprobconca) == FALSE)], 
                                obs.type = "binary", frcst.type = "prob",
                                thresholds = seq(0, 1, 1/nbinsprob), show = FALSE)
                  aux$n <- length(length(which(is.na(obsbinconca) == FALSE)))
                  eval(parse(text = sprintf("dataRelDiagram$cat%d = aux",ibins)))   
            }  
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
