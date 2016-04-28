#' @title Tercile bar plot for visualization of the skill of an ensemble forecast prediction for a particular year.
#' 
#' @description Tercile bar plot for visualization of the skill of an ensemble forecast prediction for a particular year.
#' 
#' @param mm.obj A multi-member object with predictions, either a field or a multi-member station object as a result of
#' downscaling of a forecast using station data. See details.
#' @param obs The benchmarking observations for forecast verification
#' @param year.target Year selected to plot the probability of the tercile in bars
#' @param score.threshold Threshold to remark high positive score values
#' @param stationId In case of multi-member multi-station objects, one station can be selected to plot
#'  the diagram. Otherwise ignored.

#' 
#' @importFrom abind asub
#' @importFrom RColorBrewer brewer.pal
#' @importFrom verification roc.area
#'   
#' @export
#' 
#' @details 
#'  
#' For each member, the daily predictions are averaged to obtain a single seasonal forecast. For
#' rectangular spatial domains (i.e., for fields), the spatial average is first computed (with a warning) to obtain a
#' unique series for the whole domain. The corresponding terciles for each ensemble member are then computed
#' for the analysis period. Thus, each particular member and season, are categorized into three categories (above, 
#' between or below), according to their respective climatological terciles. Then, a probabilistic forecast is computed 
#' year by year by considering the number of members falling within each category. The probability for the selected year
#' is represented by the bars. The 1/3 probability is plotted by a grey line. For instance, probabilities below this 
#' line are very low, indicating that a minority of the members falls in the tercile. Conversely, probabilities above 2/3 
#' indicate a high level of member agreement (more than 66\% of members falling in the same tercile). 
#'  
#'  Finally, the ROC Skill Score (ROCSS) is indicated at the bottom part of the bar plot for each tercile. It provides a 
#'  quantitative measure of the forecast skill, and it is commonly used to evaluate the performance of probabilistic systems
#'  (Joliffe and Stephenson 2003). The value of this score ranges from 1 (perfect forecast system) to -1 
#'  (perfectly bad forecast system). Zero indicates no skill compared with a random prediction. The negative values
#'  are written in red while high positive values are in blue. The threshold to highlight high positive values can be
#'  modified with the score.threshold argument.
#'  
#'  In case of multi-member fields, the field is spatially averaged to obtain one single time series
#'  for each member prior to data analysis, with a warning. In case of multimember stations, one single station
#'  can be selected through the \code{stationId} argument, otherwise all station series are also averaged.
#'   
#' 
#' @note The computation of climatological terciles requires a representative period to obtain meaningful results.
#' 
#' @author M.D. Frias, J. Fernandez and J. Bedia \email{joaquin.bedia@@gmail.com}
#' 
#' @family visualization
#' 
#' @references
#'  Jolliffe, I. T. and Stephenson, D. B. 2003. Forecast Verification: A Practitioner's Guide in 
#'  Atmospheri Science, Wiley, NY
#'

tercileBarplot <- function(mm.obj, obs, year.target, score.threshold=NULL, stationId = NULL) {
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
  obs.dates <- as.POSIXlt(obs$Dates$start)
  mm.dates <- as.POSIXlt(mm.obj$Dates$start)
  if (!identical(obs.dates$yday, mm.dates$yday) || !identical(obs.dates$year, mm.dates$year)) {
    stop("Forecast and verifying observations are not coincident in time")
  }
  #mm.dates <- NULL
  yrs <- getYearsAsINDEX(obs)
  yy <- unique(yrs)
  # Computation of terciles and exceedance probabilities
  n.mem <- dim(arr)[1]
  l <- lapply(1:n.mem, function(x) tapply(arr[x, ], INDEX = yrs, FUN = mean, na.rm = TRUE))
  aux <- do.call("rbind", l)
  terciles <- apply(aux, 1, quantile, probs = c(1/3, 2/3), na.rm = TRUE)    
  t.u <- apply(aux > terciles[2, ], 2, sum) / n.mem
  t.l <- apply(aux < terciles[1, ], 2, sum) / n.mem
  t.m <- 1-t.u-t.l
  cofinogram.data <- cbind(t.l,t.m,t.u)
  # Benchmark
  obs.mean <- tapply(arr.obs, yrs, mean, na.rm = TRUE)
  obs.terciles <- quantile(obs.mean, probs = c(1/3, 2/3), na.rm = TRUE)
  obs.t.u <- obs.mean > obs.terciles[2]
  obs.t.l <- obs.mean < obs.terciles[1]
  obs.t.m <- obs.mean >= obs.terciles[1] & obs.mean <= obs.terciles[2]
  obs.t <- obs.t.u*1+obs.t.l*-1
  # Area underneath a ROC curve
  roca.t.u <- suppressWarnings(roc.area(obs.t.u, t.u))
  roca.t.l <- suppressWarnings(roc.area(obs.t.l, t.l))
  roca.t.m <- suppressWarnings(roc.area(obs.t.m, t.m))
  # ROCSS
  rocss.t.u <- roca.t.u$A*2-1
  rocss.t.l <- roca.t.l$A*2-1
  rocss.t.m <- roca.t.m$A*2-1  
  
  # Threshold to write the score with different colors in the plot
  if (is.null(score.threshold)){
    threshold <- 0.5
  } else{
    threshold <-score.threshold
  }  
  color.score <- function(sval){
    if (sval<0){return("red")}
    else if (sval>=threshold){return("blue")}
    else {return("darkgrey")}
  }
  
  # Bars for the selected year
  year.filter <- yy==year.target
  Sys.setlocale("LC_TIME","C") # For the dates in North-American
  title <- paste(unique(months(obs.dates))[1],"to",unique(months(obs.dates))[3], year.target)
  par(oma = c(5, 0, 0, 0))
  barplot(cofinogram.data[year.filter,], names.arg=c("Below", "Normal", "Above"), col="lightgrey", ylab="Probability of the tercile", ylim=c(0,1), main=title)
  abline(h=0.33, col="darkgrey", lwd=4)
  # Add skill score values to the plot
  mtext("ROCSS:", side=1, line=3, adj=0, font=2)
  mtext(round(rocss.t.l,2), side=1, line=3, at=0.7, font=2, col=color.score(round(rocss.t.l,2)))
  mtext(round(rocss.t.m,2), side=1, line=3, at=1.9, font=2, col=color.score(round(rocss.t.m,2)))
  mtext(round(rocss.t.u,2), side=1, line=3, at=3.1, font=2, col=color.score(round(rocss.t.u,2)))
  # Add legend
  par(oma = c(1.5, 0, 0, 10))
  brks <- c(-1,0,threshold,1)
  cbar <- c("red", "darkgrey", "blue")
  image.plot(add = TRUE, legend.only = TRUE, breaks = brks, lab.breaks=brks, col = cbar, zlim=c(-1,1), horizontal=T) 
}
# End
