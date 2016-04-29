#' @title Bubble plot for visualization of the skill of an ensemble forecast prediction
#' 
#' @description Bubble plot for the visualization of the skill of an ensemble forecast prediction. It provides a
#'  spatially-explicit representation of the skill, resolution and reliability of a probabilistic predictive system in
#'  a single map.
#' 
#' @param mm.obj A multi-member object with predictions, either a field or a multi-member station object as a result of
#' downscaling of a forecast using station data. See details.
#' @param obs The benchmarking observations for forecast verification. 
#' @param select.year Year within the whole verification period to display the results for.
#' @param detrend Logical indicating if the data should be detreded. Default is TRUE
#' @param score Logical indicating if the relative operating characteristic skill score (ROCSS) should be included. See 
#'  details. Default is TRUE
#' @param size.as.probability Logical indicating if the tercile probabilities (magnitude proportional to bubble radius) 
#'  are drawn in the plot. See details. Default is TRUE.
#' @param piecharts. Logical flag indicating if pie charts should be plot. Default is FALSE.
#' @param subtitle. String to include a subtitle bellow the title. Default is NULL.
#' @param color.reverse. Logical indicating if the color palete for the terciles (blue, grey, red) should be
#'  reversed (e.g for precipitation). Default is FALSE.
#' @param pch.neg.score. pch value to highlight the negative score values. Default is NULL. Not available for piecharts.
#' @param pch.obs.constant. pch value to highlight those whose score cannot be computed due to constant obs 
#'  conditions (e.g. always dry). Default is NULL.
#' @param pch.data.nan. pch value to highlight those whose score cannot be computed due to time series with all NA values in 
#' the observations and/or models. Default is NULL. Not available for piecharts.
#' 
#' @importFrom scales alpha
#' @importFrom mapplots draw.pie
#' @importFrom mapplots add.pie
#' 
#' @export
#' 
#' @details 
#' 
#' For each member, the daily predictions are averaged to obtain a single seasonal forecast. The corresponding terciles 
#' for each ensemble member are then computed for the analysis period. Thus, each particular grid point, member and season,
#' are categorized into three categories (above, between or below), according to their respective climatological 
#' terciles. Then, a probabilistic forecast is computed year by year by considering the number of members falling 
#' within each category. For instance, probabilities below 1/3 are very low, indicating that a minority of the members 
#' falls in the tercile. Conversely, probabilities above 2/3 indicate a high level of member agreement (more than 66\% of members
#' falling in the same tercile). Color represents the tercile with the highest probability for the selected year. The bubble size 
#' indicates the probability of that tercile. This option is not plotted if the size.as.probability argument is FALSE.
#' 
#' Finally, the ROC Skill Score (ROCSS) is computed. For each tercile, it provides a quantitative measure of the forecast skill,
#' and it is commonly used to evaluate the performance of probabilistic systems (Joliffe and Stephenson 2003). The value of 
#' this score ranges from 1 (perfect forecast system) to -1 (perfectly bad forecast system). A value zero indicates no skill 
#' compared with a random prediction. The transparency of the bubble is associated to the ROCSS (negative values are
#' plotted with x).  This option is not plotted if the score argument is FALSE.
#' 
#' 
#' @note The computation of climatological terciles requires a representative period to obtain meaningful results.
#' 
#' @author M.D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez 
#' 
#' @family VisualizeR
#' 
#' @references
#'  Jolliffe, I. T. and Stephenson, D. B. 2003. Forecast Verification: A Practitioner's Guide in 
#'  Atmospheric Science, Wiley, NY
#'  


bubblePlotS4 <- function(mm.obj, obs, select.year, detrend=TRUE, score=TRUE, size.as.probability=TRUE, piechart=FALSE, only.at=NULL, subtitle=NULL, color.reverse=FALSE, pch.neg.score=NULL, pch.obs.constant=NULL, pch.data.nan=NULL) {
      # Check input datasets
      stopifnot(checkEnsemblesObs(mm.obj, obs))
      yrs <- getYearsAsINDEX(mm.obj)
      yy <- unique(yrs)
      if (!select.year %in% yy) {
        stop("Target year outside temporal data range")
      }
      iyear <- which(yy == select.year)
      #
      #### INCLUIR AQUI LA INTEGRACION DE GRIDS ENTRE OBS Y MM: interpGridData(obs, new.grid = getGrid(prd), method = "nearest")
      #
      # Detrend
      if (detrend){
        mm.obj <- detrend.forecast(mm.obj)
        obs <- detrend.forecast(obs)
      }
      # Computation of seasonal mean
      sm.mm.obj <- seasMean(mm.obj)
      sm.obs <- seasMean(obs)
      # Computation exceedance probabilities
      probs.mm.obj <- QuantileProbs(sm.mm.obj)
      probs.obs <- QuantileProbs(sm.obs)
      # Tercile for the maximum probability
      margin <- c(getDimIndex(probs.mm.obj,"member"), getDimIndex(probs.mm.obj,"time"), getDimIndex(probs.mm.obj,"y"), getDimIndex(probs.mm.obj,"x"))
      prob <- getData(probs.mm.obj)
      mask.nallnan.mm.obj <- apply(prob, MARGIN = margin, FUN = function(x) {sum(!is.na(x))}) # Mask for cases with no all NaN probs
      t.max.prob <- apply(prob, MARGIN = margin, FUN = which.max)
      t.max.prob[mask.nallnan.mm.obj==0] <- NaN
      obs.t <- getData(probs.obs)[1,,,,]+getData(probs.obs)[2,,,,]*2+getData(probs.obs)[3,,,,]*3     
      idxmat.max.prob <- cbind(c(as.numeric(t.max.prob[,iyear,,])), 1:prod(dim(t.max.prob[,iyear,,]))) # ... as index matrix for the target year
      # Probability of the most likely tercile
      max.prob <- apply(prob, MARGIN = margin, FUN = max)      
      # Select a year and remove in model data the NaN cases detected for the observations. 
      v.max.prob <- as.vector(max.prob[1,iyear, , ])
      v.t.max.prob <- as.vector(t.max.prob[1,iyear, , ], mode="numeric") 
      v.valid <- complete.cases(as.vector(obs.t[iyear, , ]), v.max.prob)
      ve.max.prob <- v.max.prob[v.valid]
      if (!size.as.probability){
        ve.max.prob <- rep(1, length(ve.max.prob))
      }
      v.prob <- array(dim = c(length(v.valid),3))
      for (i in 1:3){
        v.prob[,i] <- as.vector(prob[i,1,iyear,,])
      }
      # Select the corresponding lon and lat
      x.mm <- attr(getxyCoords(mm.obj),"longitude") 
      y.mm <- attr(getxyCoords(mm.obj),"latitude")       
      yx <- as.matrix(expand.grid(y.mm, x.mm))
      nn.yx <- yx[v.valid, ]  # remove NaN               
      # Define colors
      df <- data.frame(max.prob = ve.max.prob, t.max.prob = v.t.max.prob[v.valid])
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
        # Remove select.year to the score calculation
        i.yy <- !yy == select.year
        rocss <- array(dim=dim(prob)[-2:-3]) # remove year and member dimensions       
        for (i.tercile in 1:3){
          rocss[i.tercile, , ] <- apply(
            array(c(obs.t[i.yy,,]==i.tercile, prob[i.tercile, 1,i.yy , ,]), dim=c(dim(obs.t[i.yy,,]),2)),
            MAR=c(2,3),
            FUN=function(x){rocss.fun(x[,1],x[,2])})
        }        
        # Select those whose ROCSS cannot be computed due to constant obs conditions (e.g. always dry)
        t.obs.constant <- apply(obs.t[i.yy,,], MAR=c(2,3), FUN=function(x){diff(suppressWarnings(range(x, na.rm=T)))==0})
        t.obs.constant <- as.vector(t.obs.constant)
        if (!piechart) { # Select the rocss for the tercile with max prob.
          rocss <- unshape(rocss)
          max.rocss <- rocss[idxmat.max.prob]
          rocss <- deunshape(rocss)
          dim(max.rocss) <- dim(rocss)[-1]
          v.score <- c(max.rocss)[v.valid]
          pos.val <- v.score >= 0
          neg.val <- v.score < 0
        }
      }
      # Starting with the plot
      mons <- unique(months(as.POSIXlt(getDates(obs)$start), abbreviate = T))
      title <- sprintf("%s, %s to %s, %d", attr(getVariable(mm.obj), "longname"), mons[1],last(mons), select.year)
      par(bg = "white", mar = c(4, 3, 3, 1))
      plot(0, xlim=range(x.mm), ylim=range(y.mm), type="n", xlab="")
      mtext(title, side=3, line=1.5, at=min(x.mm), adj=0, cex=1.2, font=2)
      if (!is.null(subtitle)){
        mtext(subtitle, side=3, line=0.5, at=min(x.mm), adj=0, cex=0.8)
      }
      symb.size <- (df$max.prob-0.33) * 4
      symb.size.lab1 <- (1-0.33) * 4
      symb.size.lab075 <- (0.75-0.33) * 4
      symb.size.lab050 <- (0.5-0.33) * 4
      if (piechart){   # Plot with pies
        pch.neg.score <- NULL
        size.as.probability <- F
        dx <- diff(x.mm[1:2])
        dy <- diff(y.mm[1:2])
        radius <- min(dx,dy)/2*0.8
        if (score){
          rocss <- unshape(rocss)
          colors <- array(dim=dim(rocss))
          t.colors[2] <- gray(0.2)
          for (i.tercile in 1:3) {colors[i.tercile,] <- alpha(t.colors[i.tercile],255*rocss[i.tercile,])}          
          score.valid <- v.valid
        } else {
          colors <- matrix(rep(t.colors, length(v.valid)), nrow = 3)
          score.valid <- v.valid
        }
        if (!is.null(only.at)) { ##### Comprobar que funciona en este caso
          ns <- nrow(only.at$Stations$LonLatCoords)
          score.valid <- rep(FALSE, length(score.valid))
          for (i.station in 1:ns){
            score.valid[which.min((only.at$Stations$LonLatCoords[i.station,1] - nn.yx[,2])^2 +
                                    (only.at$Stations$LonLatCoords[i.station,2] - nn.yx[,1])^2)] <- TRUE
          }
        }
        for (i.loc in 1:sum(score.valid)){
          add.pie(v.prob[which(score.valid)[i.loc],], nn.yx[i.loc, 2], nn.yx[i.loc, 1], col=colors[,which(score.valid)[i.loc]],
                  radius=radius, init.angle=90, clockwise = F, border="lightgray", labels=NA
          )  
        }
        # Highlight those whose ROCSS cannot be computed due to constant obs conditions (e.g. always dry)       
        if (score & !is.null(pch.obs.constant)){
          if (!is.null(only.at)) {score.valid <- rep(TRUE, sum(v.valid))} ##### Comprobar que funciona en este caso
          valid.points <- intersect(which(t.obs.constant),which(score.valid))
          for (i.loc in valid.points){              
              add.pie(v.prob[i.loc,], yx[i.loc, 2], yx[i.loc, 1], col=NA,
                    radius=radius, init.angle=90, clockwise = F, border="black", labels=NA)            
          }
        }  
      } else { # Plot with bubbles
        if (score) {
          points(nn.yx[pos.val, 2], nn.yx[pos.val, 1], cex=symb.size[pos.val], col=alpha(df$color[pos.val], 255*v.score[pos.val]), pch=16, xlab="", ylab="")          
          # Highlight those whose ROCSS is negative
          if (!is.null(pch.neg.score)){
            points(nn.yx[neg.val, 2], nn.yx[neg.val, 1], pch=pch.neg.score, cex=1, col="black")
          }
          # Highlight those whose ROCSS cannot be computed due to constant obs conditions (e.g. always dry)
          if (!is.null(pch.obs.constant)){
            points(nn.yx[which(t.obs.constant[v.valid]), 2], nn.yx[which(t.obs.constant[v.valid]), 1], cex=1, col="black", pch=pch.obs.constant, xlab="", ylab="")
          }          
          # Highlight those whose ROCSS cannot be computed due to time seres with all NA values in the observations and/or models
          if (!is.null(pch.data.nan)){
            obs.na <- as.vector(apply(getData(sm.obs)[1,1,i.yy,,], MAR=c(2,3), FUN=function(x){sum(!is.na(x))==0}))
            mm.obj.na <- as.vector(apply(getData(sm.mm.obj)[1,,i.yy,,], MAR=c(3,4), FUN=function(x){sum(!is.na(x))==0}))
            na.points <- (obs.na + mm.obj.na)>0
            points(yx[na.points, 2], yx[na.points, 1], cex=1, col="black", pch=pch.data.nan, xlab="", ylab="")
          }
        } else {
          points(nn.yx[ , 2], nn.yx[ , 1], cex=symb.size, col=df$color, pch=16, xlab="", ylab="")
        }        
      } 
      # Add borders
      world(add = TRUE, interior = T)      
      world(add = TRUE, interior = F, lwd=3)    
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
      # Add legend
      if (size.as.probability) {
        if (score & !is.null(pch.neg.score)){
          legend('bottomleft', c("Below (size: 50% likelihood)", "Normal (size: 75% likelihood)", "Above (size: 100% likelihood)", "Negative score"), pch=c(19, 19, 19, pch.neg.score), col = c(t.colors, "black"), cex=0.8, pt.cex=c(symb.size.lab050, symb.size.lab075, symb.size.lab1, 1),horiz = T, inset = c(0, 0), xpd = TRUE, bty = "n")      
        } else{
          legend('bottomleft', c("Below (size: 50% likelihood)", "Normal (size: 75% likelihood)", "Above (size: 100% likelihood)"), pch=c(19, 19, 19), col = c(t.colors), cex=0.8, pt.cex=c(symb.size.lab050, symb.size.lab075, symb.size.lab1),horiz = T, inset = c(0, 0), xpd = TRUE, bty = "n")      
        }
      } else {
        if (score & !is.null(pch.neg.score)){
          legend('bottomleft', c("Below", "Normal", "Above", "Negative score"), pch=c(19, 19, 19, pch.neg.score), col = c(t.colors, "black"), cex=0.8, horiz = T, inset = c(0, 0), xpd = TRUE, bty = "n")        
        } else{
          legend('bottomleft', c("Below", "Normal", "Above"), pch=c(19, 19, 19), col = c(t.colors), cex=0.8, horiz = T, inset = c(0, 0), xpd = TRUE, bty = "n")        
        }  
      }
}
# End
