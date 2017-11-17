#' @keywords internal
#' @importFrom grDevices rgb col2rgb
alpha <- function(col, alpha){
  alpha <- ifelse(is.na(alpha),0,alpha) # set NA to fully transparent
  alpha <- ifelse(alpha<0,0,alpha) # set negative to fully transparent
  rgb(t(col2rgb(col)), alpha=alpha, maxColorValue = 255)
}

# Detrend: performs detrending of data
#' @keywords internal
#' @importFrom stats lm coef
detrend <- function(x, tt, y=NULL, tty=NULL) {
  #tt <- as.POSIXct(tt)
  tt <- as.numeric(as.Date(tt))
  if (all(is.na(x))) {
    xd <- rep(NA, length(x))
  } else if (any(is.na(x))){ 
    xd <- rep(NA,length(x))
    mod <- lm(x ~ tt)
    xd[!is.na(x)]<-mod$residuals
  } else { 
    mod <- lm(x ~ tt)
    xd <- mod$residuals
  }
  if (!is.null(y) & !is.null(tty) & !all(is.na(x))){
    tty <- as.numeric(as.Date(tty))
    yd <- y-(coef(mod)[1]+tty*coef(mod)[2])
    return(yd)
  } else if (!is.null(y) & !is.null(tty) & all(is.na(x))){
    yd <- rep(NA, length(y))    
    return(yd)
  } else {
    return(xd)
  }
} 


#' @keywords internal
deunshape <- function(x){
  dim(x) <- attr(x,"shape")
  attr(x,"shape") <- NULL
  return(x)
}

#' @keywords internal
unshape <- function(x, MAR=c(1)){
  attr(x, "shape") <- dim(x)
  ndim <- length(dim(x))
  dim(x) <- c(dim(x)[MAR],prod(dim(x)[-MAR]))
  return(x)
}


#' @keywords internal
last <- function(x){
   return(x[length(x)])
}

#' @keywords internal
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

#' @keywords internal
tercileColor <- function(){
  #c("red", "yellow", "blue")
  c("Blues", "Greys", "Reds")
}


# Check xyCoords from two datasets
#' @keywords internal
#' @importFrom transformeR getCoordinates
#' 
checkCoords <- function(data1, data2) {
  v <- FALSE
  if (is.null(data1) | is.null(data2)) v <- TRUE
  if (length(getCoordinates(data1)$x) == length(getCoordinates(data2)$x)) {
    if (isTRUE(all.equal(getCoordinates(data1)$x, getCoordinates(data2)$x, tolerance = 1e-03))) v <- TRUE
  }
  if (length(getCoordinates(data1)$y) == length(getCoordinates(data2)$y)) {
    if (isTRUE(all.equal(getCoordinates(data1)$y, getCoordinates(data2)$y, tolerance = 1e-03))) v <- TRUE
  }
  return(v)
}

#' @keywords internal
getDimIndex <- function(obj, dim) {
  obj.dimNames <- attr(getData(obj), "dimensions")
  return(grep(dim, obj.dimNames))
}

#' @keywords internal
getCountIndex <- function(obj, dim) {
  obj.Data <- getData(obj)
  return(dim(obj.Data)[getDimIndex(obj, dim)])
}

# Check data dimensions from the original data set before performing plots
#' @keywords internal
checkDim <- function(obj) {
  if (length(attr(obj$Data, "dimensions"))<3) { 
    stop("Invalid input data array. Data dimensions should be at least: 'time', 'lat', 'lon'")    
  }
}

# Check the data format and dates before performing plots
#' @keywords internal
checkData <- function(mm.obj, obs) {
  vec <- c()
  if(isS4(mm.obj)==TRUE){
    if (!class(mm.obj)[1]=="MrEnsemble") {
       vec <- c(vec,FALSE)
       message("The input data for hindcast is not a multimember field ")
    }
    if (length(getVarName(mm.obj))>1) {
      vec <- c(vec,FALSE)
      message("Multifields are not a valid input")
    }
    if (!missing(obs)){
      if(isS4(obs)==TRUE){
        if (class(obs)[1]=="MrEnsemble") {
          vec <- c(vec,FALSE)
          message("The verifying observations can't be a multimember")
        }
        if (length(getVarName(obs))>1) {
          vec <- c(vec,FALSE)
          message("Multifields are not a valid input for observations")
        }
        # Temporal matching check (obs-pred)
        obs.dates <- as.POSIXlt(getDates(obs)$start)
        mm.dates <- as.POSIXlt(getDates(mm.obj)$start) 
        # For monthly values
        if (diff.Date(mm.dates$yday)[1]>27 & diff.Date(obs.dates$yday)[1]>27){
          if (!identical(obs.dates$mon, mm.dates$mon) || !identical(obs.dates$year, mm.dates$year)) {
            vec <- c(vec,FALSE)
            message("Hindcast and verifying observations are not coincident in time")
          }  
        } else{
            if (!identical(unique(obs.dates$yday), unique(mm.dates$yday)) || !identical(unique(obs.dates$year), unique(mm.dates$year))) {
              vec <- c(vec,FALSE)  
              message("Hindcast and verifying observations are not coincident in time")
            }  
        }
      } else{
          stop("The obs data is not S4 object")
      }  
    } 
    if (is.null(vec)){
      vec <- TRUE
    }   
    return(vec)   
  } else{
      stop("The forecast is not S4 object")    
  } 
}

# Check for daily data
#' @keywords internal
check.daily <- function(obj) {
  obj.dates <-as.POSIXlt(obj$Dates$start)
  if (diff.Date(obj.dates$mday)[1]==0){
    # Annual data or Monthly data
    if (diff.Date(obj.dates$year)[1]==1 | diff.Date(obj.dates$mon)[2]==1){
      stop("Data are not at daily time scale")
    } 
  }
}

# Modified from transformeR to work with S4 class data
#' @keywords internal
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

# Modified from transformeR to work with S4 class data
#' @keywords internal
getSeason.S4 <- function(obj) {
  aux <- as.POSIXlt(getDates(obj)$start)$mon + 1      
  return(unique(aux))
}

#' @title Compute the ROC Area Skill Score and the significance of the Area under the Curve
#' @description Computes the skill score for the area under the ROC curve compared to an 
#' arbitrary reference forecast. 
#' @param obs A binary observation (code: 0, 1)
#' @param pred A probability prediction on the interval [0,1]
#' @param conf.level Confidence level to compute the score significance, by default conf.level=0.95
#' @return The ROC area skill score and the significance (TRUE or FALSE)
#' @author M. D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez
#' @importFrom SpecsVerification Auc
#' @importFrom stats qnorm
#' @export
rocss.fun <- function (obs, pred, conf.level = 0.95){
  no.nan <- complete.cases(obs, pred)
  if (sum(no.nan)==0){
    rval <- list(score.val = NaN, sig = NaN)     
  } else{ 
    alfa <- 1-conf.level
    z <- qnorm(1-alfa/2)
    auc.val <- Auc(pred[no.nan], obs[no.nan], handle.na = "only.complete.pairs")
    sig <- auc.val[[1]] - z*auc.val[[2]] > 0.5
    rval <- list(score.val = auc.val[[1]]*2-1, sig = sig)
  }
  return(rval)
}


#' @title Convert a dataset to a S4 class. 
#' @description Convert a data set to a S4 class. The data set must have the elements Variable, 
#'  Dates, Data, xyCoords. Depending of the kind of data it must also have the elements Members 
#'  and InitializationDates (for gridded data) or MetaData (for station data). This function is 
#'  prepared to convert the data sets loaded from the ECOMS User Data Gateway (ECOMS-UDG). See 
#'  the loadeR.ECOMS R package for more details (http://meteo.unican.es/trac/wiki/udg/ecoms/RPackage). 
#' @param obj A list the with the elements Variable, Dates, Data, xyCoords and Members 
#'  and InitializationDates (for gridded data) or MetaData (for station data)
#' @import methods
#' @details  
#'  Most of the visualization functions are programmed to work with S4 class. This function converts  
#'  datasets into S4 to use those functions. 
#' @note For gridded data sets with just one ensemble member use as.MrGrid function.
#'  For station data use as.MrStation function.
#' @author M.D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez 
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
      stop ('Invalid input data list')
    }  
    return(obj.o)
  } else{
    stop ('The input data is not a list')
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
      arr <- apply(obj.Data, MARGIN = margin,
                   FUN = function(x) {tapply(x, INDEX = yrs, FUN = mean, na.rm = TRUE)}
      )
      newdim <- c(length(yy), getCountIndex(obj,"var"), getCountIndex(obj,"member"), getCountIndex(obj,"y"), getCountIndex(obj,"x"))
      dim(arr) <- newdim
      dimnames(arr)<-NULL
      newdimNames <- c("time", obj.dimNames[margin])
      arr <- aperm(arr, match(obj.dimNames, newdimNames))  
      attr(arr, "dimensions") <- obj.dimNames
      dates.start.index <- which(duplicated(yrs)==FALSE)
      if (length(yy)==1){
        dates.end.index <- length(yrs)
      } else {
        dates.end.index <- c(dates.start.index[2:length(dates.start.index)]-1,length(yrs))
      }
      if (is.null(attr(obj.Dates, "season"))){
        attr(obj.Dates, "season") <- unique(as.numeric(format(as.POSIXlt(obj.Dates$start),"%m")))    
      }
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
#' @param k order of the quantile/s (e.g. c(1/3, 2/3) for terciles). Terciles are computed by default.
#' @return A S4 object with quantiles. The object has dimensions c("var", "member", "time", "y", "x")
#' @author M. D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez
#' @export
#' @importFrom stats quantile
MrQuantile <- function(obj, k=NULL){
  if (isS4(obj)==TRUE){
    if (is.null(k)){
      k <- c(1/3, 2/3)  
      warning('Terciles are computed')
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
      if (n.mem==1){
        mar <- setdiff(1:length(dim(obj.Data[1,,,,])), getDimIndex(obj, "time")-2)
      } else{
        mar <- setdiff(1:length(dim(obj.Data[1,,,,])), getDimIndex(obj, "time")-1)
      }
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
#' @param obj S4 object (hindcast or forecast) with dimensions c("var", "member", "time", "y", "x") 
#' it can be a station, field, multi-member field, etc. Probabilities will be computed for this object.
#' @param obj2 same S4 object (hindcast) as obj. The default is NULL. The cuantiles from obj2 are used to compute 
#' the obj probabilities. 
#' @param nbins number of bins derived from the quantiles selected (by default nbins=3, terciles)
#' @return A S4 object with the exceedance probabilities. The object has dimensions c("var", "member", "time", "y", "x")
#' @author M. D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez
#' @export
QuantileProbs <- function(obj, obj2=NULL, nbins=3){
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
      if (!is.null(obj2)){
        if (isS4(obj2)==TRUE){
          quantiles <- MrQuantile(obj2, k)
        } else{
          stop("The input data is not a S4 object")   
        }
      } else {
        quantiles <- MrQuantile(obj, k)
      }
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
                probs[count[ivar]+ibins,1,,iy,ix] <- obj.Data[ivar,,,iy,ix]>=quantiles.Data[countq[ivar]+ibins-1,,,iy,ix] & obj.Data[ivar,,,iy,ix]<=quantiles.Data[countq[ivar]+ibins,,,iy,ix]
              }
              probs[count[ivar]+nbins-1,1,,iy,ix] <- obj.Data[ivar,,,iy,ix]>quantiles.Data[countq[ivar]+length(k)-1,,,iy,ix] 
              probs <- probs*1
            } else { 
              if (n.dates==1){
                n.mem.nn <- sum(!is.na(obj.Data[ivar,,,iy,ix])) # Members with no NaN
                probs[count[ivar],1,,iy,ix] <- sum(obj.Data[ivar,,,iy,ix]<quantiles.Data[countq[ivar],,,iy,ix], na.rm=T)/n.mem.nn
                for (ibins in 1:(nbins-2)){
                  probs[count[ivar]+ibins,1,,iy,ix] <- sum(obj.Data[ivar,,,iy,ix]>=quantiles.Data[countq[ivar]+ibins-1,,,iy,ix] & obj.Data[ivar,,,iy,ix]<=quantiles.Data[countq[ivar]+ibins,,,iy,ix], na.rm=T)/n.mem.nn
                }
                probs[count[ivar]+nbins-1,1,,iy,ix] <- sum(obj.Data[ivar,,,iy,ix]>quantiles.Data[countq[ivar]+length(k)-1,,,iy,ix], na.rm=T)/n.mem.nn
              } else {
                n.mem.nn <- colSums(!is.na(obj.Data[ivar,,,iy,ix])) # Members with no NaN
                probs[count[ivar],1,,iy,ix] <- apply(obj.Data[ivar,,,iy,ix]<quantiles.Data[countq[ivar],,,iy,ix], 2, sum, na.rm=T)/n.mem.nn
                for (ibins in 1:(nbins-2)){
                  probs[count[ivar]+ibins,1,,iy,ix] <- apply(obj.Data[ivar,,,iy,ix]>=quantiles.Data[countq[ivar]+ibins-1,,,iy,ix] & obj.Data[ivar,,,iy,ix]<=quantiles.Data[countq[ivar]+ibins,,,iy,ix], 2, sum, na.rm=T)/n.mem.nn
                }
                probs[count[ivar]+nbins-1,1,,iy,ix] <- apply(obj.Data[ivar,,,iy,ix]>quantiles.Data[countq[ivar]+length(k)-1,,,iy,ix], 2, sum, na.rm=T)/n.mem.nn
              }
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
#' @param obj2 obj2. Default to \code{NULL}.
#' @return A S4 object with the data detrended. The object has dimensions c("var", "member", "time", "y", "x")
#' @author M. D. Frias \email{mariadolores.frias@@unican.es}, J. Fernandez and J. Bedia
#' @export
detrend.data <- function(obj, obj2=NULL){
  if (isS4(obj)==FALSE){
    obj <- convertIntoS4(obj)
  }
  obj.Data <- getData(obj)
  obj.dimNames <- attr(obj.Data, "dimensions")
  if (identical(obj.dimNames, c("var", "member", "time", "y", "x"))) {
    obj.Dates <- getDates(obj)
    obj.xyCoords <- getxyCoords(obj)
    n.var <- getCountIndex(obj,"var")
    n.mem <- getCountIndex(obj,"member")
    if (!is.null(obj2)){
      if (isS4(obj2)==FALSE){
        obj2 <- convertIntoS4(obj2)
      }
      obj2.Data <- getData(obj2)
      obj2.dimNames <- attr(obj2.Data, "dimensions")
      if (identical(obj2.dimNames, c("var", "member", "time", "y", "x"))) {
        obj2.Dates <- getDates(obj2)
        obj2.xyCoords <- getxyCoords(obj2)
        n2.var <- getCountIndex(obj2,"var")
        n2.mem <- getCountIndex(obj2,"member")
        if (n.mem!=n2.mem){
          stop("The two datasets have different number of members")      
        }
        for (ivar in 1:n2.var){  
          for (imember in 1:n2.mem){
            for (ilat in 1:length(obj2.xyCoords$y)){
              for (ilon in 1:length(obj2.xyCoords$x)){
                obj2.Data[ivar, imember, , ilat, ilon] <- detrend(x=obj.Data[ivar,imember,,ilat,ilon], tt=obj.Dates$start, y=obj2.Data[ivar,imember,,ilat,ilon], tty=obj2.Dates$start)
              }      
            }
          }
        }
        slot(obj2, "Data") <- obj2.Data
        slot(obj2, "Transformation") <- c(getTransformation(obj2), "detrend")
        return(obj2)
      } else {
        stop("Invalid input data array. Dimensions should be 'var', 'member', 'time', 'y', 'x'")    
      }
    } else{
      for (ivar in 1:n.var){  
        aux.list <- lapply(1:n.mem, function(x) {
          arr <- array(obj.Data[ivar,x,,,], dim(obj.Data)[-c(getDimIndex(obj, "var"),getDimIndex(obj, "member"))])
          attr(arr, "dimensions") <- c("time", "lat", "lon")
          mat <- array3Dto2Dmat(arr)
          aux <- apply(mat, 2, detrend, tt=obj.Dates$start)
          mat2Dto3Darray(aux, obj.xyCoords$x, obj.xyCoords$y)
        })
        obj.Data[ivar,,,,] <- unname(do.call("abind", c(aux.list, along = -1)))
      }
      slot(obj, "Data") <- obj.Data
      slot(obj, "Transformation") <- c(getTransformation(obj), "detrend")
      return(obj)    
    }
  } else {
    stop("Invalid input data array. Dimensions should be 'var', 'member', 'time', 'y', 'x'")
  } 
}    
