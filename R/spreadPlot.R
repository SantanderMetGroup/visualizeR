#' @title Box plot for visualization of the skill of an ensemble forecast prediction
#' 
#' @description Box plot for the visualization of the skill of an ensemble forecast prediction.
#' 
#' @param mm.obj A multi-member object with predictions, either a field or a multi-member station object as a result of
#' downscaling of a forecast using station data. See details.
#' @param obs The benchmarking observations for forecast verification
#' @param year.target Year selected to plot the probability of the tercile in bars
#' @param boxplot Logical flag indicating whether a boxplot should be added to the graph. Default is TRUE
#' @param violin  Logical flag indicating whether a violin plot should be added to the graph instead of the boxplot. 
#' Default is FALSE
#' @param add.points Logical flag indicating whether crosses indicating the ensemble members should be added to the 
#' graph. Default is FALSE.
#' @param pch Single character or an integer code for one of the set of R graphics symbols. Cross is the default symbol. 
#' Only consider if add.points is TRUE. 
#' @param stationId In case of multi-member multi-station objects, one station can be selected to plot
#'  the diagram. Otherwise ignored.
#' 
#' @importFrom abind asub
#' @importFrom verification roc.area
#' @import vioplot
#'   
#' @export
#' 
#' @details 
#'  
#' For each member, the daily predictions are averaged to obtain a single seasonal forecast. For
#' rectangular spatial domains (i.e., for fields), the spatial average is first computed (with a warning) to obtain a
#' unique series for the whole domain. The climatology for the forecast period is computed. The shaded areas show 
#' the central tercile (dark shade) and the maximum and minimum (light shade). To avoid overinterpretation of daily 
#' peaks, the daily data has been smoothed by means of a (centered) moving average of 31 days. Therefore, at the 
#' location of the boxplots, the background shows the monthly mean forecast (the terciles and extremes being computed 
#' over members and years). Monthly means are calculated for each ensemble member. This information is included to the 
#' graph in a boxplot or a violin plot (to unveil multimodalities in the data) or/and the values of the ensemble members. 
#' 
#' In case of multi-member fields, the field is spatially averaged to obtain one single time series
#' for each member prior to data analysis, with a warning. In case of multimember stations, one single station
#' can be selected through the \code{stationId} argument, otherwise all station series are also averaged.
#'   
#' 
#' @note The computation of climatological terciles requires a representative period to obtain meaningful results.
#' 
#' @author M.D. Frias, J. Fernandez and J. Bedia \email{joaquin.bedia@@gmail.com}
#' 
#' @family visualization

spreadPlot <- function(mm.obj, obs, year.target, boxplot=TRUE, violin = FALSE, add.points=FALSE, pch=NULL, stationId = NULL) {
      mm.dimNames <- attr(mm.obj$Data, "dimensions")
      obs.dimNames <- attr(obs$Data, "dimensions")
      if (!("member" %in% mm.dimNames)) {
            stop("The input data for 'multimember' is not a multimember field")
      }
      if ("member" %in% obs.dimNames) {
            stop("The verifying observations can't be a multimember")
      }
      if ("var" %in% mm.dimNames | "var" %in% obs.dimNames) {
            stop("Multifields are not a valid input")
      }
      # Preparation of a 2D array with "member" and "time" dimensions for the target
      is.mm.station <- ifelse(exists("Metadata", where = mm.obj), TRUE, FALSE)
      is.obs.station <- ifelse(exists("Metadata", where = obs), TRUE, FALSE)
      if (identical(mm.dimNames, c("member", "time", "lat", "lon"))) {
            warning("The results presented are the spatial mean of the input field")
            lat.dim.index <- grep("lat", mm.dimNames)
            lon.dim.index <- grep("lon", mm.dimNames)
            mar <- setdiff(1:length(mm.dimNames), c(lat.dim.index, lon.dim.index))
            arr <- apply(mm.obj$Data, mar, mean, na.rm = TRUE)
            attr(arr, "dimensions") <- mm.dimNames[mar]
            x.mm <- mm.obj$xyCoords$x
            y.mm <- mm.obj$xyCoords$y
      } else {
            if (identical(mm.dimNames, c("member", "time"))) {
                  arr <- mm.obj$Data
                  if (is.mm.station) {
                        x.mm <- mm.obj$xyCoords[ ,1]
                        y.mm <- mm.obj$xyCoords[ ,2]
                  } else {
                        x.mm <- mm.obj$xyCoords$x
                        y.mm <- mm.obj$xyCoords$y
                  }
            } else {
                  if (identical(mm.dimNames, c("member", "time", "station"))) {
                        if (is.null(stationId)) {
                              warning("The results presented are the mean of all input stations")
                              mar <- match(setdiff(mm.dimNames, "station"), mm.dimNames)
                              arr <- apply(mm.obj$Data, mar, mean, na.rm = TRUE)
                              attr(arr, "dimensions") <- mm.dimNames[mar]
                              x.mm <- mm.obj$xyCoords[ ,1]
                              y.mm <- mm.obj$xyCoords[ ,2]
                        } else {
                              idx <- grep(stationId, mm.obj$Metadata$station_id)
                              if (identical(idx, integer(0))) {
                                    stop("The 'stationId' provided was not found")
                              }
                              st.dim.index <- grep("station", mm.dimNames)
                              arr <- asub(mm.obj$Data, idx, st.dim.index)
                              attr(arr, "dimensions") <- setdiff(mm.dimNames, "station")
                              x.mm <- mm.obj$xyCoords[idx,1]
                              y.mm <- mm.obj$xyCoords[idx,2]
                        }
                  } else {
                        stop("Invalid input data array")
                  }
            }
      }
      # Preparation of a "time" 1D vector vec for the target 
      if (identical(obs.dimNames, c("time", "lat", "lon"))) {
            # Spatial consistency check
            x.obs <- obs$xyCoords$x
            y.obs <- obs$xyCoords$y
            lat.dim.index <- grep("lat", obs.dimNames)
            lon.dim.index <- grep("lon", obs.dimNames)
            mar <- setdiff(1:length(obs.dimNames), c(lat.dim.index, lon.dim.index))
            arr.obs <- apply(obs$Data, mar, mean, na.rm = TRUE)
            attr(arr.obs, "dimensions") <- obs.dimNames[mar]
      } else {
            if (identical(obs.dimNames, "time")) {
                  arr.obs <- obs$Data
            } else {
                  if (identical(obs.dimNames, c("time", "station"))) {
                        if (is.null(stationId)) {
                              mar <- match(setdiff(obs.dimNames, "station"), obs.dimNames)
                              arr.obs <- apply(obs$Data, mar, mean, na.rm = TRUE)
                              attr(arr.obs, "dimensions") <- obs.dimNames[mar]
                        } else {
                              idx <- grep(stationId, obs$Metadata$station_id)
                              if (identical(idx, integer(0))) {
                                    stop("The 'stationId' provided was not found in the 'obs' dataset")
                              }
                              st.dim.index <- grep("station", obs.dimNames)
                              arr.obs <- asub(obs$Data, idx, st.dim.index)
                              attr(arr.obs, "dimensions") <- setdiff(obs.dimNames, "station")
                        }
                  }
            }
      }
      # Temporal matching check (obs-pred)
      obs.dates <- as.POSIXlt(obs$Dates$start, tz="GMT")
      mm.dates <- as.POSIXlt(mm.obj$Dates$start, tz="GMT")
      if (!identical(obs.dates$yday, mm.dates$yday) || !identical(obs.dates$year, mm.dates$year)) {
            stop("Forecast and verifying observations are not coincident in time")
      }

      # Plot the climatology shadow 
      ma <- function(x,n=31){filter(x,rep(1/n,n), sides=2)} # Time filter (moving average)
      mm.ma <- t(apply(arr, MARGIN=1, FUN=ma)) 
      days <- sprintf("%02d%02d", mm.dates$mon+1, mm.dates$mday)
      days <- factor(days, levels=unique(days))
      # Quantiles mixing for each day all the years and members
      ens.quant <- t(do.call("rbind",
                             lapply(unique(days), FUN=function(x){quantile(c(mm.ma[,days==x]), probs=c(0.0,1/3,0.5,2/3,1.0), na.rm=T)})
      ))
      seadates <- 1:length(unique(days))    
      par(bg="white", mar=c(4,4,1,1))
      plot(seadates, ens.quant[3,], ylim = range(mm.ma, na.rm=T), ty = 'n', ylab = paste(mm.obj$Variable$varName,"- Daily Mean"), xlab = sprintf("time\n(shade: %d to %d, symbols: %d)", obs.dates[1]$year+1900, last(obs.dates)$year+1900, year.target), xaxt="n")
      polygon(x = c(seadates, rev(seadates)), y = c(ens.quant[1, ], rev(ens.quant[5, ])), border = "transparent", col = rgb(0.2,0.2,0.2,.2))
      polygon(x = c(seadates, rev(seadates)), y = c(ens.quant[2, ], rev(ens.quant[4, ])), border = "transparent", col = rgb(0.3,0.3,0.3,.3))
      lines(seadates, ens.quant[3,], lwd=2)      
      
      # Boxplot, Violinplot and/or points for the selected year
      year.filter <- getYearsAsINDEX(mm.obj)==year.target
      month.index <- factor(months(mm.dates[year.filter]), levels=unique(months(mm.dates)))
      nmembers <- length(mm.obj$Members)
      mm.monmeans <- do.call("rbind", lapply(c(1:nmembers), function(x) tapply(arr[x,year.filter], INDEX = month.index, FUN = mean, na.rm = TRUE)))
      pos.cen <- grep("..15", levels(days)) + 0.5
      pos.ini <- grep("..01", levels(days)) 
      if (violin) {
        boxplot=FALSE # Uncompatible options
        for(i in seq(1,length(pos.cen))){
          vioplot(mm.monmeans[,i], wex=6, border="black", at=pos.cen[i], col="grey", add=T)
        }
      }       
      if (boxplot){
        boxplot(mm.monmeans, boxwex=3, lwd=3, border="black", at=pos.cen, col=rgb(1,0,0,0), axes=F, add=T)
      }
      if (add.points) {
        # Different color of the points depending on the tercile 
        color <- matrix(c(rep("#353535",nrow(mm.monmeans)*ncol(mm.monmeans))), ncol=ncol(mm.monmeans), nrow=nrow(mm.monmeans))
        for (i in seq(1,ncol(mm.monmeans))){
          color[mm.monmeans[,i]<ens.quant[2, grep("..15", levels(days))[i]],i] <- "blue"
          color[mm.monmeans[,i]>ens.quant[4, grep("..15", levels(days))[i]],i] <- "red"
        }       
        if (is.null(pch)) {
          pch="+"
        }  
        points(rep(pos.cen,each=nmembers), mm.monmeans, col=color, pch=pch)
      }
      axis(1, at=pos.cen, labels=levels(month.index), las="1")
      abline(v=pos.ini, lty="dotted")          
}
# End

