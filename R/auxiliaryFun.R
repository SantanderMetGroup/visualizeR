#' @keywords internal
#' @importFrom grDevices rgb col2rgb
alpha <- function(col, alpha){
  alpha <- ifelse(is.na(alpha),0,alpha) # set NA to fully transparent
  alpha <- ifelse(alpha<0,0,alpha) # set negative to fully transparent
  rgb(t(col2rgb(col)), alpha=alpha, maxColorValue = 255)
}

# Detrend: performs detrending of data
#' @keywords internal
#' @importFrom stats lm

detrend <- function(x) {
  if (all(is.na(x))) {
    rep(NA, length(x))
  } else if (any(is.na(x))){ 
    x2 <- rep(NA,length(x))
    x2[!is.na(x)]<-lm(x ~ I(1:length(x)))$resid
    x2
  } else { 
    lm(x ~ I(1:length(x)))$resid
  }
} 

# From Chus
#detrend <- function(dsRobj){
#  dsRobj.dimNames <- attr(dsRobj$Data, "dimensions")
#  lon.dim <- grep("lon", dsRobj.dimNames)
#  lat.dim <- grep("lat", dsRobj.dimNames)
#  member.dim <- grep("member", dsRobj.dimNames)
#  time.dim <- grep("time", dsRobj.dimNames)
#  n.mem <- dim(dsRobj$Data)[member.dim]
#  rval <- dsRobj
#  times <- as.POSIXlt(dsRobj$Dates$start)
#  margin <- c(lat.dim, lon.dim, member.dim)
#  detrended <- function(x){
#    if (sum(!is.na(x))==0)
#      return (rep(NA, length(x)))
#    else
#      return(resid(lm(y~t,data.frame(y=x, t=times))))
#  }
#  rval$Data <- apply(dsRobj$Data, MARGIN = margin, FUN = detrended)
#  # Recover the original dimension order
#  newdims <- c("time", dsRobj.dimNames[margin])
#  rval$Data <- aperm(rval$Data, match(dsRobj.dimNames, newdims))
#  attr(rval$Data, "dimensions") <- dsRobj.dimNames
#  rval$Dates$start <- tapply(dsRobj$Dates$start, INDEX=yrs, FUN=min)
#  rval$Dates$end <- tapply(dsRobj$Dates$end, INDEX=yrs, FUN=max)
#  attr(rval$Dates, "season") <- getSeason(dsRobj)
#  return(rval)
#}

deunshape <- function(x){
  dim(x) <- attr(x,"shape")
  attr(x,"shape") <- NULL
  return(x)
}

#fldsubsample <- function(dsRobj, one.out.of = 2){
#  dsRobj.dimNames <- attr(dsRobj$Data, "dimensions")
#  lon.dim <- grep("lon", dsRobj.dimNames)
#  lat.dim <- grep("lat", dsRobj.dimNames)
#  nx <- length(dsRobj$xyCoords$x)
#  ny <- length(dsRobj$xyCoords$y)
#  filter.x <- seq(1,nx,by=one.out.of)
#  filter.y <- seq(1,ny,by=one.out.of)
#  rval <- dsRobj
#  rval$xyCoords$x <- dsRobj$xyCoords$x[filter.x]
#  rval$xyCoords$y <- dsRobj$xyCoords$y[filter.y]
#  attr(rval$xyCoords, "resX") <- attr(dsRobj$xyCoords, "resX")*one.out.of
#  attr(rval$xyCoords, "resY") <- attr(dsRobj$xyCoords, "resY")*one.out.of
#  rval$Data <- extract(dsRobj$Data,
#                       indices=list(filter.x,filter.y),
#                       dims=c(lon.dim, lat.dim)
#  )
#  attr(rval$Data, "dimensions") <- attr(dsRobj$Data, "dimensions")
#  return(rval)
#}

last <- function(x){
   return(x[length(x)])
}

#summary.dsRgrid <- function(x){
#  cat(sprintf("Variable: %s\n", x$Variable$varName))
#  cat("Shape: ", dim(x$Data), "(", attr(x$Data, "dimensions"), ")\n")
#  nx <- length(x$xyCoords$x)
#  ny <- length(x$xyCoords$y)
#  nt <- length(x$Dates$start)
#  cat(sprintf("X range: %6.2f to %6.2f (%d values)\n", min(x$xyCoords$x), max(x$xyCoords$x),nx))
#  cat(sprintf("Y range: %6.2f to %6.2f (%d values)\n", min(x$xyCoords$y), max(x$xyCoords$y), ny))
#  available.months <- sort(unique(as.integer(substr(x$Dates$start, 6,7))))
#  if (identical(available.months, 1:12)) {
#    available.months <- "All year"
#  } else {
#    available.months <- sprintf("Season: %s", paste(available.months, collapse=' '))
#  }
#  cat(sprintf("time range: %s to %s (%d values, %s)\n", x$Dates$start[1], x$Dates$end[nt], nt, available.months))
#  if ("member" %in% attr(x$Data, "dimensions"))
#    member.dim <- grep("member", attr(x$Data, "dimensions"))
#  cat(sprintf("Members: %d\n", dim(x$Data)[member.dim]))
#}

tercileBrewerColorRamp <- function(ncolors){
  ncolors <- ncolors-2
  tcols <- tercileColor()
  tmp <- brewer.pal(ncolors, tcols[1])
  rval <- data.frame(low=c(tmp[1],tmp[1],tmp), stringsAsFactors=F)
  tmp <- brewer.pal(ncolors, tcols[2])
  rval$middle <- c(tmp[1],tmp[1],tmp)
  tmp <- brewer.pal(ncolors, tcols[3])
  rval$high <- c(tmp[1],tmp[1],tmp)
  return(rval)
}

tercileColor <- function(){
  #c("red", "yellow", "blue")
  c("Blues", "Greys", "Reds")
}

#tercileColorRamp <- function(ncolors){
#  alphas <- seq(0,255,len=ncolors)
#  tcols <- tercileColor()
#  t.low <- alpha(tcols[1],alphas)
#  t.mid <- alpha(tcols[2],alphas)
#  t.hi <- alpha(tcols[3],alphas)
#  return(data.frame(t.low,t.mid,t.hi))
#}

unshape <- function(x, MAR=c(1)){
  attr(x, "shape") <- dim(x)
  ndim <- length(dim(x))
  dim(x) <- c(dim(x)[MAR],prod(dim(x)[-MAR]))
  return(x)
}

#yearmean <- function(dsRobj, yrOfSeasonEnd = TRUE){
#  dsRobj.dimNames <- attr(dsRobj$Data, "dimensions")
#  lon.dim <- grep("lon", dsRobj.dimNames)
#  lat.dim <- grep("lat", dsRobj.dimNames)
#  member.dim <- grep("member", dsRobj.dimNames)
#  time.dim <- grep("time", dsRobj.dimNames)
#  n.mem <- dim(dsRobj$Data)[member.dim]
#  if (yrOfSeasonEnd) {
#    yrs <- getYearsAsINDEX(dsRobj)
#  } else {
#    yrs <- as.integer(substr(dsRobj$Dates$start, 0,4))
#  }
#  rval <- dsRobj
#  margin <- c(lat.dim, lon.dim, member.dim)
#  rval$Data <- apply(dsRobj$Data, MARGIN = margin,
#    FUN = function(x) {tapply(x, INDEX = yrs, FUN = mean, na.rm = TRUE)}
#  )
#  # Recover the original dimension order
#  newdims <- c("time", dsRobj.dimNames[margin])
#  rval$Data <- aperm(rval$Data, match(dsRobj.dimNames, newdims))
#  attr(rval$Data, "dimensions") <- dsRobj.dimNames
#  rval$Dates$start <- tapply(dsRobj$Dates$start, INDEX=yrs, FUN=min)
#  rval$Dates$end <- tapply(dsRobj$Dates$end, INDEX=yrs, FUN=max)
#  return(rval)
#}

#' @title Compute the ROC Area Skill Score
#' @description Computes the skill score for the area under the ROC curve compared to an 
#' arbitrary reference forecast. 
#' @param obs A binary observation (code: 0, 1)
#' @param pred A probability prediction on the interval [0,1]
#' @return The ROC area skill score
#' @author M. D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez
#' @note Adapted from the roc.area function from the verification library. 
#' @export

rocss.fun <- function (obs, pred) {
  id <- is.finite(obs) & is.finite(pred)
  obs <- obs[id]
  pred <- pred[id]
  n1 <- sum(obs)
  n <- length(obs)
  A.tilda <- (mean(rank(pred)[obs == 1]) - (n1 + 1)/2)/(n - n1)
  rval <- A.tilda*2-1
  return(rval)
}


#' @title Convert a dataset to a S4 class. 
#' 
#' @description Convert a data set to a S4 class. The data set must have the elements Variable, 
#'  Dates, Data, xyCoords. Depending of the kind of data it must also have the elements Members 
#'  and InitializationDates (for gridded data) or MetaData (for station data). This function is 
#'  prepared to convert the data sets loaded from the ECOMS User Data Gateway (ECOMS-UDG). See 
#'  the loadeR.ECOMS R package for more details (http://meteo.unican.es/trac/wiki/udg/ecoms/RPackage). 
#' 
#' @param obj A list the with the elements Variable, Dates, Data, xyCoords and Members 
#'  and InitializationDates (for gridded data) or MetaData (for station data)
#
#' @import methods
#' 
#' @details  
#'  The visualization functions are programmed to work with S4 class. This function converts  
#'  datasets into S4 to use those functions. 
#'  
#' @note For gridded data sets with just one ensemble member use as.MrGrid function.
#'  For station data use as.MrStation function.
#'  
#' @author M.D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez 
#' 
#' @family VisualizeR

convertIntoS4 <- function(obj) {   
  if(class(obj)=="list"){
    if(!is.null(obj$Variable) & !is.null(obj$Data) & !is.null(obj$xyCoords) & !is.null(obj$Dates)){  
      if(!is.null(obj$Members)){
        if(length(obj$Members)>1){
          # The input dataset is a multi-member ensemble.
          obj.o <- as.MrEnsemble(obj)
        } else{
          # The input dataset is not a multi-member ensemble.
          obj.o <- as.MrGrid(obj)
        }
      } else{
        if(!is.null(obj$Metadata)){
          obj.o <- as.MrStation(obj)
        } else{
          obj.o <- as.MrGrid(obj)
        }    
      }
    } else {
      obj.o <- obj
    }
    return(obj.o)
  } else{
    stop ('The input data is not a list')
  }
}

###################################
# Functions for S4 objects
###################################

getDimIndex <- function(obj, dim) {
  obj.dimNames <- attr(getData(obj), "dimensions")
  return(grep(dim, obj.dimNames))
}

getCountIndex <- function(obj, dim) {
  obj.Data <- getData(obj)
  return(dim(obj.Data)[getDimIndex(obj, dim)])
}

# Check the validation of the data for the VisualizeR plots
checkEnsemblesObs <- function(mm.obj, obs) {
  if(isS4(mm.obj)==TRUE & isS4(obs)==TRUE){
    vec <- c()
    if (!class(mm.obj)[1]=="MrEnsemble") {
      vec <- c(vec,FALSE)
      message("The input data for forecasts is not a multimember field")
    }
    if (class(obs)[1]=="MrEnsemble") {
      vec <- c(vec,FALSE)
      message("The verifying observations can't be a multimember")
    }
    if (length(getVarName(mm.obj))>1 | length(getVarName(obs))>1) {
      vec <- c(vec,FALSE)
      message("Multifields are not a valid input")
    }
    # Temporal matching check (obs-pred)
    obs.dates <- as.POSIXlt(getDates(obs)$start)
    mm.dates <- as.POSIXlt(getDates(mm.obj)$start) 
    # For monthly values
    if (diff.Date(mm.dates$yday)[1]>27 & diff.Date(obs.dates$yday)[1]>27){
      if (!identical(obs.dates$mon, mm.dates$mon) || !identical(obs.dates$year, mm.dates$year)) {
        vec <- c(vec,FALSE)
        message("Forecast and verifying observations are not coincident in time")
      }  
    } else{
      if (!identical(unique(obs.dates$yday), unique(mm.dates$yday)) || !identical(unique(obs.dates$year), unique(mm.dates$year))) {
        vec <- c(vec,FALSE)  
        message("Forecast and verifying observations are not coincident in time")
      }  
    }
    if (is.null(vec)){
      vec <- TRUE
    }   
    return(vec)    
  } else{
      stop("The input data are not S4 objects")    
  } 
}



#' @title Get the spatial mean from a S4 object
#' @description Get the spatial mean from a S4 object with the dimensions c("var", "member", "time", "y", "x")
#' @param obj S4 object with dimensions c("var", "member", "time", "y", "x") it can be a station, field, multi-member field, etc
#' @return A S4 object with spatial mean. The object has dimensions c("var", "member", "time", "y", "x")
#' @author M. D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez
#' @export

spatialMean <- function(obj) {
  if (isS4(obj)==TRUE){
    obj.Data <- getData(obj)
    obj.dimNames <- attr(obj.Data, "dimensions")
    if (identical(obj.dimNames, c("var", "member", "time", "y", "x"))) {
      obj.xyCoords <- getxyCoords(obj)
      if (!length(obj.xyCoords$x)==1 | !length(obj.xyCoords$y)==1){
        warning("The results presented are the spatial mean of the input field") 
        y.dim.index <- getDimIndex(obj, "y")
        x.dim.index <- getDimIndex(obj, "x") 
        mar <- setdiff(1:length(obj.dimNames), c(y.dim.index, x.dim.index))
        arr <- apply(obj.Data, mar, mean, na.rm = TRUE)
        attr(arr, "dimensions") <- obj.dimNames    
        obj.xyCoords$x <- 1
        obj.xyCoords$y <- 1
        attr(obj.xyCoords, "longitude") <- mean(attr(obj.xyCoords,"longitude"))
        attr(obj.xyCoords, "latitude") <- mean(attr(obj.xyCoords,"latitude"))
        newdim <- c(dim(obj.Data)[getDimIndex(obj, "var")],dim(obj.Data)[getDimIndex(obj, "member")],dim(obj.Data)[getDimIndex(obj, "time")],length(obj.xyCoords$y), length(obj.xyCoords$x))
        dim(arr) <- newdim 
        slot(obj, "Data") <- arr
        slot(obj, "xyCoords") <- obj.xyCoords
        slot(obj, "Transformation") <- c(getTransformation(obj), "spatialMean")
      }
      return(obj)
    } else {
        stop("Invalid input data array. Data dimensions should be: 'var', 'member', 'time', 'y', 'x'")
    }
  } else{
      stop("The input data is not a S4 object")    
  } 
}



#' @title Get the seasonal mean from a S4 object
#' @description Get the seasonal mean from a S4 object with the dimensions c("var", "member", "time", "y", "x")
#' @param obj S4 object with dimensions c("var", "member", "time", "y", "x") it can be a station, field, multi-member field, etc
#' @return A S4 object with the seasonal mean. The object has dimensions c("var", "member", "time", "y", "x")
#' @author M. D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez
#' @export

seasMean <- function(obj) {
  if (isS4(obj)==TRUE){
    obj.Data <- getData(obj)
    obj.dimNames <- attr(obj.Data, "dimensions")
    if (identical(obj.dimNames, c("var", "member", "time", "y", "x"))) {  
      obj.Dates <- getDates(obj)
      yrs <- getYearsAsINDEX.S4(obj)
      yy <- unique(yrs)
      margin <- c(getDimIndex(obj,"var"), getDimIndex(obj,"member"), getDimIndex(obj,"y"), getDimIndex(obj,"x"))
      arr <- apply(getData(obj), MARGIN = margin,
                   FUN = function(x) {tapply(x, INDEX = yrs, FUN = mean, na.rm = TRUE)}
      )
      dimnames(arr)<-NULL
      newdims <- c("time", obj.dimNames[margin])
      arr <- aperm(arr, match(obj.dimNames, newdims))  
      attr(arr, "dimensions") <- obj.dimNames
      dates.start.index <- which(duplicated(yrs)==FALSE)
      dates.end.index <- c(dates.start.index[2:length(dates.start.index)]-1,length(yrs))
      obj.Dates$start <- getDates(obj)$start[dates.start.index]
      obj.Dates$end <- getDates(obj)$end[dates.end.index] 
      slot(obj, "Data") <- arr
      slot(obj, "Dates") <- obj.Dates
      slot(obj, "Transformation") <- c(getTransformation(obj), "seasMean")
      return(obj)
    } else {
        stop("Invalid input data array. Dimensions should be 'var', 'member', 'time', 'y', 'x'")
    }
  } else{
      stop("The input data is not a S4 object")    
  } 
}


#' @title Get quantiles from a S4 object
#' @description Get quantiles from a S4 object with the dimensions c("var", "member", "time", "y", "x")
#' @param obj S4 object with dimensions c("var", "member", "time", "y", "x") it can be a station, field, multi-member field, etc
#' @param k order of the quantile/s (e.g. c(1/3, 2/3) for terciles)
#' @return A S4 object with quantiles. The object has dimensions c("var", "member", "time", "y", "x")
#' @author M. D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez
#' @export
#' @importFrom stats quantile

MrQuantile <- function(obj, k=NULL){
  if (isS4(obj)==TRUE){
    if (is.null(k)){
      stop('Introduce the order of the quantile (e.g. c(1/3, 2/3) for terciles)')
    }
    obj.Data <- getData(obj)
    obj.dimNames <- attr(obj.Data, "dimensions")
    if (identical(obj.dimNames, c("var", "member", "time", "y", "x"))) {
      obj.Dates <- getDates(obj)
      obj.Variable <- getVariable(obj)
      obj.xyCoords <- getxyCoords(obj)
      n.var <- getCountIndex(obj,"var")
      n.mem <- getCountIndex(obj,"member")
      n.y <- getCountIndex(obj,"y")
      n.x <- getCountIndex(obj,"x")
      arr <- array(NA, c(length(k)*n.var, n.mem, 1, n.y, n.x))
      count <- seq(1,length(k)*n.var,length(k))
      newvar <- character(length = 0)
      mar <- setdiff(1:length(dim(obj.Data[1,,,,])), which(dim(obj.Data[1,,,,])==dim(obj.Data)[getDimIndex(obj, "time")]))  
      for (ivar in 1:n.var){ 
        if (is.null(dim(obj.Data[1,,,,]))){
          arr[count[ivar]:(count[ivar]+length(k)-1),,,,] <- quantile(obj.Data[ivar,,,,], probs = k, na.rm = TRUE)
        } else{
          arr[count[ivar]:(count[ivar]+length(k)-1),,,,] <- apply(obj.Data[ivar,,,,], mar, quantile, probs = k, na.rm = TRUE)
        }
        for (ik in 1:length(k)){
          newvar <- c(newvar, paste(obj.Variable$varName[ivar], paste("Q",round(k[ik],4)*10000, sep=""), sep="_"))
        }
      }
      attr(arr, "dimensions") <- obj.dimNames
      obj.Dates$start <- getDates(obj)$start[1]
      obj.Dates$end <- getDates(obj)$end[length(getDates(obj)$end)] 
      obj.Variable$varName <- newvar
      slot(obj, "Data") <- arr
      slot(obj, "Dates") <- obj.Dates
      slot(obj, "Variable") <- obj.Variable
      slot(obj, "Transformation") <- c(getTransformation(obj), "MrQuantile")
      return(obj)
    } else {
        stop("Invalid input data array. Dimensions should be 'var', 'member', 'time', 'y', 'x'")
    }  
  } else{
      stop("The input data is not a S4 object")    
  } 
}


#' @title Get exceedance probabilities from a S4 object
#' @description Get exceedance probabilities from a S4 object with the dimensions c("var", "member", "time", "y", "x")
#' @param obj S4 object with dimensions c("var", "member", "time", "y", "x") it can be a station, field, multi-member field, etc
#' @param nbins number of bins derived from the quantiles selected (by default nbins=3, terciles)
#' @return A S4 object with the exceedance probabilities. The object has dimensions c("var", "member", "time", "y", "x")
#' @author M. D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez
#' @export

QuantileProbs <- function(obj, nbins=3){
  if (isS4(obj)==TRUE){
    obj.Data <- getData(obj)
    obj.dimNames <- attr(obj.Data, "dimensions")
    if (identical(obj.dimNames, c("var", "member", "time", "y", "x"))) {
      k <- c(1:(nbins-1))/nbins
      obj.Variable <- getVariable(obj)
      n.mem <- getCountIndex(obj,"member")
      n.var <- getCountIndex(obj,"var")
      n.dates <- getCountIndex(obj,"time")
      n.y <- getCountIndex(obj,"y")
      n.x <- getCountIndex(obj,"x")
      quantiles <- MrQuantile(obj, k)
      quantiles.Data <- getData(quantiles)
      probs <- array(NA, c(nbins*n.var, 1, n.dates, n.y, n.x))
      count <-  seq(1,nbins*n.var,nbins)
      countq <-  seq(1,length(k)*n.var,length(k))
      newvar <- character(length = 0)
      for (ivar in 1:n.var){
        for (iy in 1:n.y){
          for (ix in 1:n.x){
            if (n.mem==1){
              probs[count[ivar],1,,iy,ix] <- obj.Data[ivar,,,iy,ix]<quantiles.Data[countq[ivar],,,iy,ix]
              for (ibins in 1:(nbins-2)){
                probs[count[ivar]+ibins,1,,iy,ix] <- obj.Data[ivar,,,iy,ix]>=quantiles.Data[countq[ivar]+ibins-1,,,iy,ix] & obj.Data[ivar,,,iy,ix]<quantiles.Data[countq[ivar]+ibins,,,iy,ix]
              }
              probs[count[ivar]+nbins-1,1,,iy,ix] <- obj.Data[ivar,,,iy,ix]>=quantiles.Data[countq[ivar]+length(k)-1,,,iy,ix] 
              probs <- probs*1
            } else { 
              n.mem.nn <- colSums(!is.nan(obj.Data[ivar,,,iy,ix])) # Members with no NaN
              probs[count[ivar],1,,iy,ix] <- apply(obj.Data[ivar,,,iy,ix]<quantiles.Data[countq[ivar],,,iy,ix], 2, sum, na.rm=T)/n.mem.nn
              for (ibins in 1:(nbins-2)){
                probs[count[ivar]+ibins,1,,iy,ix] <- apply(obj.Data[ivar,,,iy,ix]>=quantiles.Data[countq[ivar]+ibins-1,,,iy,ix] & obj.Data[ivar,,,iy,ix]<quantiles.Data[countq[ivar]+ibins,,,iy,ix], 2, sum, na.rm=T)/n.mem.nn
              }
              probs[count[ivar]+nbins-1,1,,iy,ix] <- apply(obj.Data[ivar,,,iy,ix]>=quantiles.Data[countq[ivar]+length(k)-1,,,iy,ix], 2, sum, na.rm=T)/n.mem.nn
            }        
          }
        }
        for (ibins in 1:nbins){
          newvar <- c(newvar, paste(obj.Variable$varName[ivar], "prob", paste("Q",round(ibins/nbins,4)*10000, sep=""), sep="_"))
        }
      }
      attr(probs, "dimensions") <- obj.dimNames
      obj.Variable$varName <- newvar
      slot(obj, "Data") <- probs
      slot(obj, "Dates") <- getDates(quantiles)
      slot(obj, "Variable") <- obj.Variable
      slot(obj, "Transformation") <- c(getTransformation(obj), "QuantileProbs")
      return(obj)
    } else {
      stop("Invalid input data array. Dimensions should be 'var', 'member', 'time', 'y', 'x'")
    } 
  } else{
      stop("The input data is not a S4 object")    
  } 
}


#' @title Detrend data from a S4 object
#' @description Detrend data from a S4 object with the dimensions c("var", "member", "time", "y", "x")
#' @param obj S4 object with dimensions c("var", "member", "time", "y", "x") it can be a station, field, multi-member field, etc
#' @return A S4 object with the data detrended. The object has dimensions c("var", "member", "time", "y", "x")
#' @author M. D. Frias \email{mariadolores.frias@@unican.es}, J. Fernandez and J. Bedia
#' @export

detrend.forecast <- function(obj){
  if (isS4(obj)==TRUE){
    obj.Data <- getData(obj)
    obj.dimNames <- attr(obj.Data, "dimensions")
    if (identical(obj.dimNames, c("var", "member", "time", "y", "x"))) {
      obj.Dates <- getDates(obj)
      obj.Variable <- getVariable(obj)
      obj.xyCoords <- getxyCoords(obj)
      n.var <- getCountIndex(obj,"var")
      n.mem <- getCountIndex(obj,"member")    
      for (ivar in 1:n.var){  
        aux.list <- lapply(1:n.mem, function(x) {
          arr <- array(obj.Data[ivar,x,,,], dim(obj.Data)[-1:-2])
          attr(arr, "dimensions") <- c("time", "lat", "lon")
          mat <- array3Dto2Dmat(arr)
          aux <- apply(mat, 2, detrend)
          mat2Dto3Darray(aux, obj.xyCoords$x, obj.xyCoords$y)
        })
        obj.Data[ivar,,,,] <- unname(do.call("abind", c(aux.list, along = -1)))
      }
      slot(obj, "Data") <- obj.Data
      slot(obj, "Transformation") <- c(getTransformation(obj), "detrend")
      return(obj)
    } else {
      stop("Invalid input data array. Dimensions should be 'var', 'member', 'time', 'y', 'x'")
    } 
  } else{
    stop("The input data is not a S4 object")    
  } 
}    







# Modified from downscaleR to work with S4 class !!!!
getYearsAsINDEX.S4 <- function(obj) {
  season <- getSeason.S4(obj)
  aux.dates <- as.POSIXlt(getDates(obj)$start)
  yrs <- aux.dates$year + 1900
  if (!identical(season, sort(season))) {
    yy <- unique(yrs)[-1]
    aux <- match(aux.dates$mon + 1, season)
    brks <- c(1, which(diff(aux) < 0) + 1, length(aux) + 1)
    l <- lapply(1:(length(brks) - 1), function(x) {
      a <- yrs[brks[x] : (brks[x + 1] - 1)]
      return(rep(yy[x], length(a)))
    })
    yrs  <- do.call("c", l)
  }
  return(yrs)
}

# Modified from downscaleR to work with S4 class !!!!
getSeason.S4 <- function(obj) {
  aux <- as.POSIXlt(getDates(obj)$start)$mon + 1      
  return(unique(aux))
}
