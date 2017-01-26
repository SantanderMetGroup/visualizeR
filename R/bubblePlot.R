##     bubblePlot Bubble plot for visualization of forecast skill of ensemble predictions.
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

#' @title Bubble plot for visualization of forecast skill of ensemble predictions.
#' 
#' @description Bubble plot for visualization of forecast skill of ensemble predictions. It provides a
#'  spatially-explicit representation of the skill, resolution and reliability of a probabilistic predictive 
#'  system in a single map. 
#'  This function is prepared to plot the data sets loaded from the ECOMS User Data Gateway (ECOMS-UDG). See 
#'  the loadeR.ECOMS R package for more details (http://meteo.unican.es/trac/wiki/udg/ecoms/RPackage).
#' 
#' @param mm.obj A multi-member list with the hindcast for verification. See details.
#' @param obs List with the benchmarking observations for forecast verification.
#' @param forecast A multi-member list with the forecasts. Default is NULL. 
#' @param year.target Year within the hindcast period considered as forecast. Default is NULL.
#' @param detrend Logical indicating if the data should be detrended. Default is FALSE.
#' @param score Logical indicating if the relative operating characteristic skill score (ROCSS) should be included. See 
#'  details. Default is TRUE
#' @param size.as.probability Logical indicating if the tercile probabilities (magnitude proportional to bubble radius) 
#'  are drawn in the plot. See details. Default is TRUE.
#' @param bubble.size Number for the bubble size. bubble.size=4 by default.
#' @param score.range Two dim vector to rescale the transparency of the bubbles (e.g c(0.5, 0.8) values
#'  lower than 0.5 are transparent while values up to 0.8 have maximum transparency). Default is NULL that is transparency 
#'  ranges from 0 to 1. 
#' @param piechart Logical flag indicating if pie charts should be plot instead of bubbles. Default is FALSE.
#' @param subtitle String to include a subtitle bellow the title. Default is NULL.
#' @param color.reverse Logical indicating if the color palete for the terciles (blue, grey, red) should be
#'  reversed (e.g for precipitation). Default is FALSE.
#' @param pch.neg.score pch value to highlight the negative score values. Default is NULL. Not available for piecharts.
#' @param pch.obs.constant pch value to highlight those whose score cannot be computed due to constant obs 
#'  conditions (e.g. always dry). Default is NULL.
#' @param pch.data.nan pch value to highlight those whose score cannot be computed due to time series with all NA values in 
#'  the observations and/or models. If score=F, highlight those grids with all forecasts NaN. 
#' 
#' @importFrom scales alpha
#' @importFrom mapplots draw.pie add.pie
#' @importFrom transformeR array3Dto2Dmat mat2Dto3Darray draw.world.lines
#' @importFrom abind abind
#' @importFrom grDevices gray
#' @importFrom graphics par plot mtext points legend
#' @importFrom stats complete.cases
#' 
#' @export
#' 
#' @details  
#'  First daily data are averaged to obtain a single seasonal value. The corresponding terciles 
#'  for each ensemble member are then computed for the hindcast period. Thus, each particular grid point, member and season,
#'  are categorized into three categories (above, between or below), according to their respective climatological 
#'  terciles. Then, a probabilistic forecast is computed year by year by considering the number of members falling 
#'  within each category. For instance, probabilities below 1/3 are very low, indicating that a minority of the members 
#'  falls in the tercile. Conversely, probabilities above 2/3 indicate a high level of member agreement (more than 66\% of members
#'  falling in the same tercile). Probabilities are also computed for the forecast or the selected year. Color represents the tercile 
#'  with the highest probability for the forecast or selected year. The bubble size indicates the probability of that tercile. This 
#'  option is not plotted if the size.as.probability argument is FALSE.
#' 
#'  Finally, the ROC Skill Score (ROCSS) is computed for the hindcast period. For each tercile, it provides a quantitative measure 
#'  of the forecast skill, and it is commonly used to evaluate the performance of probabilistic systems (Joliffe and Stephenson 2003). 
#'  The value of this score ranges from 1 (perfect forecast system) to -1 (perfectly bad forecast system). A value zero indicates no 
#'  skill compared with a random prediction. The transparency of the bubble is associated to the ROCSS. By default only positive values
#'  are plotted if the score argument is TRUE. The target year is considered as forecast and it is not included in the computation of 
#'  the score (operational point of view). 
#' 
#' @note The computation of climatological terciles requires a representative period to obtain meaningful results.
#' 
#' @author M.D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez based on the original diagram 
#'  conceived by Slingsby et al 2009.
#' 
#' @family VisualizeR
#' 
#' @references
#'  Jolliffe, I. T. and Stephenson, D. B. 2003. Forecast Verification: A Practitioner's Guide in Atmospheric 
#'  Science, Wiley, NY.
#'  
#'  Slingsby A., Lowe R., Dykes J., Stephenson D. B., Wood J., Jupp T. E. 2009. A pilot study for the collaborative 
#'  development of new ways of visualising seasonal climate forecasts. Proc. 17th Annu. Conf. of GIS Research UK, 
#'  Durham, UK, 1-3 April 2009.

bubblePlot <- function(mm.obj, obs, forecast=NULL, year.target=NULL, detrend=FALSE, score=TRUE, size.as.probability=TRUE, bubble.size=4, score.range=NULL, piechart=FALSE, subtitle=NULL, color.reverse=FALSE, pch.neg.score=NULL, pch.obs.constant=NULL, pch.data.nan=NULL){
      yy <- unique(getYearsAsINDEX(mm.obj))
      if (is.null(score.range)){
        score.range <- c(0,1)
      }
      # Check grid from the data. 
      if (!checkCoords(mm.obj, obs)){
        message("WARNING: Data with no common grid. Interpolating observations to hindcast grid")
        # Interpolate observations to the hindcast grid
        obs <- interpGrid(obs, new.coordinates = getGrid(mm.obj), method = "nearest")
      }
      if (!checkCoords(mm.obj, forecast)){
        message("WARNING: Data with no common grid. Interpolating forecasts to hindcast grid")
        # Interpolate forecast to the hindcast grid
        forecast <- interpGrid(forecast, new.coordinates = getGrid(mm.obj), method = "nearest")
      }
      if (is.null(forecast)){
        if (is.null(year.target)){
          year.target <- last(yy)
        }
        if (!year.target %in% yy) {
          stop("Target year outside temporal data range")
        }
        yy.forecast <- year.target
        forecast <- subsetGrid(mm.obj, years=yy.forecast, drop=F)
        mm.obj <- subsetGrid(mm.obj, years=yy[yy!=yy.forecast], drop=F)
        obs <- subsetGrid(obs, years=yy[yy!=yy.forecast], drop=F)
        yy <- yy[yy!=yy.forecast]
      }      
      # Check input datasets
      if (isS4(mm.obj)==FALSE){
        mm.obj <- convertIntoS4(mm.obj)
      }
      if (isS4(obs)==FALSE){
        obs <- convertIntoS4(obs)
      }
      stopifnot(checkData(mm.obj, obs))
      if (!is.null(forecast)){
        yy.forecast <- unique(getYearsAsINDEX(forecast))
        if (length(yy.forecast)>1) {
          stop("Select just one year for forecast")
        }
        year.target <- NULL
        if (isS4(forecast)==FALSE){
          forecast <- convertIntoS4(forecast)
        }
      }
      # Detrend
      if (detrend){
        mm.obj <- detrend.forecast(mm.obj)
        obs <- detrend.forecast(obs)
      } 
      # Computation of seasonal mean
      sm.mm.obj <- seasMean(mm.obj)
      sm.obs <- seasMean(obs)
      sm.forecast <- seasMean(forecast)
      # Computation exceedance probabilities
      probs.mm.obj <- QuantileProbs(sm.mm.obj)
      probs.obs <- QuantileProbs(sm.obs)
      probs.forecast <- QuantileProbs(sm.forecast, sm.mm.obj)
      # Detect gridpoints with time series with all NA values 
      obs.na <- as.vector(apply(getData(sm.obs)[1,1,,,], MARGIN=c(2,3), FUN=function(x){sum(!is.na(x))==0}))
      mm.obj.na <- as.vector(apply(getData(sm.mm.obj)[1,,,,], MARGIN=c(3,4), FUN=function(x){sum(!is.na(x))==0}))
      forecast.na <- as.vector(apply(getData(sm.forecast)[1,,,,], MARGIN=c(2,3), FUN=function(x){sum(!is.na(x))==0}))
      # Tercile for the maximum probability
      prob <- getData(probs.mm.obj)
      prob.forecast <- getData(probs.forecast)
      margin <- c(getDimIndex(probs.mm.obj,"member"), getDimIndex(probs.mm.obj,"time"), getDimIndex(probs.mm.obj,"y"), getDimIndex(probs.mm.obj,"x"))
      t.max <- function(t.probs, margin.dim){
        # Mask for cases with no all probs equal to NAN. This avoid errors in ROCSS computation.  
        mask.nallnan <- apply(t.probs, MARGIN = margin.dim, FUN = function(x) {sum(!is.na(x))}) 
        t.max.prob <- apply(t.probs, MARGIN = margin.dim, FUN = which.max)
        t.max.prob[mask.nallnan==0] <- NaN
        return(t.max.prob)
      } 
      t.max.prob <- t.max(prob, margin)
      t.max.forecast <- t.max(prob.forecast, margin)
      obs.t <- getData(probs.obs)[1,,,,]+getData(probs.obs)[2,,,,]*2+getData(probs.obs)[3,,,,]*3  
      idxmat.max.prob <- cbind(c(as.numeric(t.max.forecast)), 1:prod(dim(t.max.forecast))) 
      # Probability of the most likely tercile
      max.prob.forecast <- apply(prob.forecast, MARGIN = margin, FUN = max)      
      ve.max.prob <- as.vector(max.prob.forecast)
      v.t.max.prob <- as.vector(t.max.forecast, mode="numeric") 
      gridpoints <- length(ve.max.prob)
      if (!size.as.probability){
        ve.max.prob <- rep(1, gridpoints)
      }
      v.prob <- array(dim = c(gridpoints,3))
      for (i in 1:3){
        v.prob[,i] <- as.vector(prob.forecast[i,1,1,,])
      }
      # Select the corresponding lon and lat
      x.mm <- attr(getxyCoords(mm.obj),"longitude") 
      y.mm <- attr(getxyCoords(mm.obj),"latitude")  
      nn.yx <- as.matrix(expand.grid(y.mm, x.mm))
      # Define colors
      df <- data.frame(max.prob = ve.max.prob, t.max.prob = v.t.max.prob)
      df$color <- "black"
      t.colors <- c("blue", "darkgrey", "red")
      if (color.reverse){
        t.colors <- c("red", "darkgrey", "blue")
      }
      df$color[df$t.max.prob == 3] <- t.colors[3]
      df$color[df$t.max.prob == 2] <- t.colors[2]
      df$color[df$t.max.prob == 1] <- t.colors[1]      
      # Compute ROCSS for all terciles
      if (score) { 
        rocss <- array(dim=dim(prob)[-2:-3]) # remove member and year dimensions       
        for (i.tercile in 1:3){
          rocss[i.tercile, , ] <- apply(
            array(c(obs.t==i.tercile, prob[i.tercile, 1, , ,]), dim=c(dim(obs.t),2)),
            MARGIN=c(2,3),
            FUN=function(x){rocss.fun(x[,1],x[,2])})
        }  
        # Select those whose ROCSS cannot be computed due to constant obs conditions (e.g. always dry)
        t.obs.constant <- apply(obs.t, MARGIN=c(2,3), FUN=function(x){diff(suppressWarnings(range(x, na.rm=T)))==0})
        t.obs.constant <- as.vector(t.obs.constant)
        # Compute transparency of the bubbles
        rocss <- unshape(rocss)
        colors <- array(dim=dim(rocss))
        min.score.range <- score.range[1]
        max.score.range <- score.range[2]
        a <- -(255*min.score.range)/(max.score.range-min.score.range)
        b <- 255/(max.score.range-min.score.range)
        for (i.tercile in 1:3){
          transparency <- a+b*rocss[i.tercile,]
          transparency[rocss[i.tercile,]>max.score.range] <- 255
          transparency[rocss[i.tercile,]<min.score.range] <- 0
          colors[i.tercile,] <- alpha(t.colors[i.tercile],transparency)
        }
        rocss <- deunshape(rocss)
        if (!piechart) { 
          df$transp <- colors[idxmat.max.prob]  # Select the transparency for the tercile with max prob
          rocss <- unshape(rocss)
          v.score <- rocss[idxmat.max.prob]  # Select the rocss for the tercile with max prob.
          rocss <- deunshape(rocss)
          pos.val <- v.score >= 0
          neg.val <- v.score < 0
        }
      }
      # Starting with the plot
      mons.start <- months(as.POSIXlt((getDates(obs)$start)[1]),abbreviate=T)
      mons.end <- months(last(as.POSIXlt(getDates(obs)$end))-1, abbreviate=T)
      title <- sprintf("%s, %s to %s, %d", attr(getVariable(mm.obj), "longname"), mons.start, mons.end, yy.forecast)
      opar <- par(no.readonly=TRUE)
      par(bg = "white", mar = c(4, 3, 3, 1))
      plot(0, xlim=range(x.mm), ylim=range(y.mm), type="n", xlab="")
      mtext(title, side=3, line=1.5, at=min(x.mm), adj=0, cex=1.2, font=2)
      if (!is.null(subtitle)){
        mtext(subtitle, side=3, line=0.5, at=min(x.mm), adj=0, cex=0.8)
      }
      symb.size <- (df$max.prob-0.33) * bubble.size
      symb.size.lab1 <- (1-0.33) * bubble.size
      symb.size.lab075 <- (0.75-0.33) * bubble.size
      symb.size.lab050 <- (0.5-0.33) * bubble.size
      if (piechart){   # Plot with pies
        pch.neg.score <- NULL
        size.as.probability <- F
        dx <- diff(x.mm[1:2])
        dy <- diff(y.mm[1:2])
        radius <- min(dx,dy)/2*0.8
        if (score){
          v.valid <- c(1:gridpoints)
          # Remove gridpoints with ROCSS or forecast data all NaN
          all.nan <- unique(sort(c(which(colSums(is.na(rocss))==3), which(forecast.na))))
          if (length(all.nan)!=0){
            v.valid <- v.valid[-all.nan]
          }
        } else {
          colors <- matrix(rep(t.colors, gridpoints), nrow = 3)
          v.valid <- which(!forecast.na)
        }
        # Plot the piechart only for those grid points with no NaN probabilities
        for (i.loc in v.valid){
          add.pie(v.prob[i.loc,], nn.yx[i.loc, 2], nn.yx[i.loc, 1], col=colors[,i.loc],
                  radius=radius, init.angle=90, clockwise = F, border="lightgray", labels=NA
          )  
        }
        if (score){
          # Highlight those whose ROCSS cannot be computed due to constant obs conditions (e.g. always dry)       
          if (!is.null(pch.obs.constant)){
            valid.points <- which(t.obs.constant)
            for (i.loc in valid.points){  
              points(nn.yx[i.loc, 2], nn.yx[i.loc, 1], cex=radius/2, col="black", pch=pch.obs.constant, xlab="", ylab="")
            }
          }
          # Highlight those whose ROCSS cannot be computed due to time series with all NA values in the observations and/or models
          if (!is.null(pch.data.nan)){
            valid.points <- which((obs.na + mm.obj.na)>0)
            for (i.loc in valid.points){   
              points(nn.yx[i.loc, 2], nn.yx[i.loc, 1], cex=radius/2, col="black", pch=pch.data.nan, xlab="", ylab="")
            }
          }
        } else{
          # Highlight those grids with all forecasts NaN. 
          if (!is.null(pch.data.nan)){
            na.points <- which(forecast.na)
            points(nn.yx[na.points, 2], nn.yx[na.points, 1], cex=radius/2, col="white", pch=pch.data.nan, xlab="", ylab="")
          }
        }
      } else { # Plot with bubbles
        cex.val <- 0.5
        if (score) {
          points(nn.yx[pos.val, 2], nn.yx[pos.val, 1], cex=symb.size[pos.val], col=df$transp[pos.val], pch=16, xlab="", ylab="")          
          # Highlight those whose ROCSS is negative
          if (!is.null(pch.neg.score)){
            points(nn.yx[neg.val, 2], nn.yx[neg.val, 1], pch=pch.neg.score, cex=cex.val, col="black")
          }
          # Highlight those whose ROCSS cannot be computed due to constant obs conditions (e.g. always dry)
          if (!is.null(pch.obs.constant)){
            points(nn.yx[which(t.obs.constant), 2], nn.yx[which(t.obs.constant), 1], cex=cex.val, col="black", pch=pch.obs.constant, xlab="", ylab="")
          }          
          # Highlight those whose ROCSS cannot be computed due to time series with all NA values in the observations and/or models
          if (!is.null(pch.data.nan)){
            na.points <- (obs.na + mm.obj.na)>0
            points(nn.yx[na.points, 2], nn.yx[na.points, 1], cex=cex.val, col="black", pch=pch.data.nan, xlab="", ylab="")
          }
        } else {
          # Plot bubbles only for those grid points with not all NaN values
          v.valid <- which(!forecast.na)
          points(nn.yx[v.valid, 2], nn.yx[v.valid, 1], cex=symb.size[v.valid], col=df$color[v.valid], pch=16, xlab="", ylab="")
          # Highlight those grids with all NaN values.
          if (!is.null(pch.data.nan)){
            na.points <- which(forecast.na)
            points(nn.yx[na.points, 2], nn.yx[na.points, 1], cex=cex.val, col="black", pch=pch.data.nan, xlab="", ylab="")
          }
        }        
      } 
      # Add borders
      draw.world.lines(lwd=1)
      #world(add = TRUE, interior = T)      
      #world(add = TRUE, interior = F, lwd=3)    
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
      # Add legend
      # Transparency color for mean of the score.range
      mean.score.range <- mean(score.range)
      legend.color.transparency <- c(alpha(t.colors[1], 255*mean.score.range), alpha(t.colors[2], 255*mean.score.range), alpha(t.colors[3], 255*mean.score.range))
      if (size.as.probability) {
        if (score & !is.null(pch.neg.score)){
          legtext <- c("Below (size: 50% likelihood)", "Normal (size: 75%)", sprintf("Above (size: 100%%) Transparency: ROCSS=%3.2f", mean.score.range), "Negative score")
          xcoords <- c(0, 0.55, 0.95, 1.35)
          secondvector <- (1:length(legtext))-1
          textwidths <- xcoords/secondvector 
          textwidths[1] <- 0
          legend('bottomleft', legend=legtext, pch=c(19, 19, 19, pch.neg.score), col = c(legend.color.transparency, "black"), cex=0.7, pt.cex=c(symb.size.lab050, symb.size.lab075, symb.size.lab1, 1), horiz=T, bty="n", text.width=textwidths, xjust=0)      
        } else{
          if (score){
            t.colors <- legend.color.transparency
            legtext <- c("Below (size: 50% likelihood)", "Normal (size: 75%)", sprintf("Above (size: 100%%) Transparency: ROCSS=%3.2f", mean.score.range))
          } else{
            legtext <- c("Below (size: 50% likelihood)", "Normal (size: 75%)", "Above (size: 100%)")     
          }
          xcoords <- c(0, 0.55, 0.95)
          secondvector <- (1:length(legtext))-1
          textwidths <- xcoords/secondvector 
          textwidths[1] <- 0
          #legend('bottomleft', c("Below (size: 50% likelihood)", "Normal (size: 75%)", "Above (size: 100%)"), pch=c(19, 19, 19), col = c(t.colors), cex=0.8, pt.cex=c(symb.size.lab050, symb.size.lab075, symb.size.lab1), horiz = T, inset = c(0, 0), xpd = TRUE, bty = "n")      
          legend('bottomleft', legend=legtext, pch=c(19, 19, 19), col = c(t.colors), cex=0.7, pt.cex=c(symb.size.lab050, symb.size.lab075, symb.size.lab1), horiz=T, bty="n", text.width=textwidths, xjust=0)     
        }
      } else {
        if (score & !is.null(pch.neg.score)){
          legend('bottomleft', c("Below", "Normal", sprintf("Above (Transparency: ROCSS=%3.2f)", mean.score.range), "Negative score"), pch=c(19, 19, 19, pch.neg.score), col = c(legend.color.transparency, "black"), cex=0.7, horiz=T, bty="n", xjust=0)        
        } else{
          if (score){
            t.colors <- legend.color.transparency
            legtext <- c("Below", "Normal", sprintf("Above (Transparency: ROCSS=%3.2f)", mean.score.range))
          } else{
            legtext <- c("Below", "Normal", "Above")     
          }
          legend('bottomleft', legend=legtext, pch=c(19, 19, 19), col=c(t.colors), cex=0.7, horiz=T, bty="n", xjust=0)        
        }  
      }
      par(opar)
}
# End