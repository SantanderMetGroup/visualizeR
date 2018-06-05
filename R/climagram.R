##     Climagram plot 
##
##     Copyright (C) 2018 Santander Meteorology Group (http://www.meteo.unican.es)
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

#' @title Climagram
#' @description A climagram is a box plot for visualization of a seasonal forecast with added EQC information from the hindcast
#' (and possibly the verifying observations).
#' @param hindcast A climate4R multi-member grid with the hindcast for verification. 
#' @param forecast A climate4R multi-member grid with the forecast. Default is \code{NULL}. See details. 
#' @param obs A climate4R grid with the reference observations. They should match the hindcast period.
#'  Default is \code{NULL}.
#' @param year.target Year within the hindcast period considered as forecast. Default is NULL. See Details
#' @param detrend Logical indicating if the data should be (linearly) detrended. Default is FALSE.
#' @param violin  Logical flag indicating whether a violin plot should be added to the graph instead
#'  of an ordinary boxplot. Default is \code{TRUE}.
#' @param use.anomalies Should anomalies or the original variable magnitudes be used for data representation? 
#' Default to \code{TRUE} (i.e., anomalies). See Details.
#' @param add.points Logical flag indicating whether crosses indicating the individual forecast members
#'  should be added to the graph. Default to \code{FALSE}.
#' @param add.legend Should legend be added to the plot. Default to \code{TRUE}
#' @param clim.time.frame Default to \code{"none"}. Temporal frame used as reference for the calculation of the climatological
#' normal. See Details. 
#' @param add.eqc.info Should EQC (Evaluation and Quality Control) information be added onto the plot?.
#' Ignored if no \code{obs} is passed. See Details
#' 
#' @return 
#' 
#' Plots a climagram. Note that the number of forecast members falling within each (hindcast) tercile are indicated as figures on the 
#' right of the forecast box/violins.
#' 
#' @details 
#' 
#' \strong{Forecast data}
#' 
#' The product is conceived to display forecast information in an operative framework. However, for
#' testing purposes, a target year from the hindcast can be extracted via the \code{year.target} argument and 
#' treated as the forecast year. Note that all the hindcast statistics (including the optional detrending) 
#' will be computed after extracting the target year from the hindcast.
#' 
#' \strong{Spatial averaging}
#' 
#'  For spatial domains encompassing multiple points/grid points, the spatial average is first computed to obtain a
#'  unique series for the whole domain vias \code{link[transformeR]{aggregateGrid}} (latitudial weighting is used).
#' 
#' \strong{Forecast/Hindcast representation}
#'   
#'  In order to represent the forecast and hindcast information, \dQuote{violins} are used as default,
#'   although boxplots (see \code{\link[graphics]{boxplot}}) can be obtained by setting 
#'   argument \code{violin} to \code{FALSE}. For further details on violin construction see
#'    the documentation of funtion \code{\link[vioplot]{vioplot}}.
#'  
#'  Note that the hindcast climatology is calculated by considering the annual multimember averages.
#' 
#' \strong{Using anomalies}
#' 
#' By default, the function will compute anomalies. In this case, the forecast anomalies 
#' will be calculated considering the hindcast climatology (see next section for details). 
#' 
#' \strong{Climatological reference period}
#'   
#' By default, the climatology is computed for the entire seasonal slice of the forecast. For instance,
#' suppose a forecast for DJF. By default, the DJF anomaly will be computed by subtracting to the 
#' January forecast the DJF climatology, and so on. If set to \code{"monthly"}, the climatology
#' is computed sepparately for each month, so the anomalies for January will be computed using the January
#'  climatology, the February anomalies using the February climatology and so on.
#'   See argument \code{"time.frame"} of \code{\link[transformeR]{scaleGrid}} for more details.
#' 
#' \strong{EQC Information}  
#' 
#' EQC (Evaluation and Quality Control) information can be optionally added to the plot. This is the default behaviour when 
#' \code{obs} is supplied. Here CRPS (bias-sensitive) and RPS (bias-insensitive) EQC measures are considered.
#'       
#' @note 
#' 
#' All inputs for this function must be at a monthly temporal resolution.
#' 
#' The computation of climatological terciles requires a representative period to obtain meaningful results.
#'  
#' @author J. Bedia
#' 
#' @family visualization functions
#' @importFrom transformeR subsetGrid checkDim checkSeason aggregateGrid detrendGrid getYearsAsINDEX getTimeResolution scaleGrid getSeason
#' @importFrom stats filter median
#' @importFrom graphics plot axis grid boxplot box legend
#' @importFrom magrittr %>% extract2
#' @importFrom vioplot vioplot
#' @importFrom SpecsVerification FairRps FairCrps
#' @export
#' @examples \dontrun{
#' my_load <- function(file.url, verbose = TRUE) {
#'     tmpfile <- tempfile()
#'     download.file(url = file.url, destfile = tmpfile)
#'     readRDS(tmpfile)
#' }
#' hindcast <- my_load("http://meteo.unican.es/work/visualizeR/data/tas.cfs.enso34.rds")
#' forecast <- my_load("http://meteo.unican.es/work/visualizeR/data/tas.cfs.2016.enso34.rds")
#' 
#' # A seasonal forecast product, without reference obs information
#' climagram(hindcast = hindcast, forecast = forecast, violin = TRUE, obs = NULL)
#' # Violins can be replaced by ordinary boxplots:
#' climagram(hindcast = hindcast, forecast = forecast, violin = FALSE, obs = NULL)
#' # Instead of anomalies, raw values can be used: 
#' climagram(hindcast = hindcast, forecast = forecast, use.anomalies = FALSE)
#' # Observations are depicted by the backgroud shadows (median indicated by the dotted line):
#' # (RPS and CRPS are added by default when observations are included)
#' obs <- my_load("http://meteo.unican.es/work/visualizeR/data/tas.ncep.enso34.rds")
#' climagram(hindcast = hindcast, forecast = forecast, obs = obs)
#' }

climagram <- function(hindcast,
                      forecast = NULL,
                      obs = NULL,
                      year.target = NULL,
                      use.anomalies = TRUE,
                      violin = TRUE,
                      add.points = FALSE,
                      detrend = FALSE,
                      add.legend = TRUE,
                      clim.time.frame = "none",
                      add.eqc.info = TRUE) {
    # Check data dimension from the original data sets
    message("[", Sys.time(),"] Checking input consistency ...")
    stopifnot(is.logical(detrend))
    stopifnot(is.logical(use.anomalies))
    stopifnot(is.logical(violin))
    stopifnot(is.logical(add.points))
    stopifnot(is.logical(add.legend))
    stopifnot(is.logical(add.eqc.info))
    if (getTimeResolution(hindcast) != "MM") {
        stop("Monthly data is required for climagram construction (use 'aggregateGrid' for submonthly data)")
    }
    if (is.null(forecast)) {
        if (is.null(year.target)) stop("Either a year.target from the hindcast or a forecast dataset must be indicated",
                                       call. = FALSE)
        yrs <- unique(getYearsAsINDEX(hindcast))
        if (!year.target %in% yrs) stop("Target year outside temporal hindcast range", call. = FALSE)
        year.hind <- setdiff(yrs, year.target)
        forecast <- subsetGrid(hindcast, years = year.target, drop = FALSE)
        hindcast <- subsetGrid(hindcast, years = year.hind, drop = FALSE)
    } else {
        transformeR::checkDim(hindcast, forecast, dimensions = c("member", "lat", "lon"))    
        checkSeason(hindcast, forecast)
    }
    if (!is.null(obs)) {
        checkSeason(hindcast, obs)
        transformeR::checkDim(hindcast, obs, dimensions = c("time", "lat", "lon"))
    } else {
        if (add.eqc.info) add.eqc.info <- FALSE
        message("NOTE: 'add.eqc.info' was set to FALSE, as no observations were provided")
    }
    # Detrend
    if (detrend) {
        message("[", Sys.time(),"] Detrending ...")
        hindcast <- suppressMessages(detrendGrid(hindcast)) 
        forecast <- suppressMessages(detrendGrid(hindcast, grid2 = forecast))
        obs <- suppressMessages(detrendGrid(obs)) 
    }
    # anomalies
    if (use.anomalies) {
        forecast <- suppressMessages(scaleGrid(forecast, base = hindcast, time.frame = clim.time.frame))
        hindcast <- suppressMessages(scaleGrid(hindcast, time.frame = clim.time.frame))
        if (!is.null(obs)) {
            obs <- suppressMessages(scaleGrid(obs, time.frame = clim.time.frame))
        }
    }
    # Spatial aggregation ------------------------------------------------------
    latShape <- getShape(hindcast, "lat")
    if (!(is.na(latShape) || latShape == 1)) {
        message("[", Sys.time(),"] Latitudinal (weighted) averaging ...")  
        hindcast <- suppressMessages(aggregateGrid(hindcast, aggr.lat = list(FUN = "mean", na.rm = TRUE))) %>% drop()
        forecast <- suppressMessages(aggregateGrid(forecast, aggr.lat = list(FUN = "mean", na.rm = TRUE))) %>% drop()
        if (!is.null(obs)) {
            obs <- suppressMessages(aggregateGrid(obs, aggr.lat = list(FUN = "mean", na.rm = TRUE))) %>% drop()
        }
    }
    lonShape <- getShape(hindcast, "lon")
    if (!(is.na(lonShape) || lonShape == 1)) {
        message("[", Sys.time(),"] Longitudinal averaging ...")    
        hindcast <- suppressMessages(aggregateGrid(hindcast, aggr.lon = list(FUN = "mean", na.rm = TRUE))) %>% drop()
        forecast <- suppressMessages(aggregateGrid(forecast, aggr.lon = list(FUN = "mean", na.rm = TRUE))) %>% drop()
        if (!is.null(obs)) {
            obs <- suppressMessages(aggregateGrid(obs, aggr.lon = list(FUN = "mean", na.rm = TRUE)))# %>% drop()
        }
    }
    # Plotting part
    message("[", Sys.time(),"] Calculating plotting parameters ...")    
    # Y-axis limits
    upperbound <- max(max(hindcast$Data, na.rm = TRUE), max(forecast$Data, na.rm = TRUE)) %>% ceiling()
    lowerbound <- min(min(hindcast$Data, na.rm = TRUE), min(forecast$Data, na.rm = TRUE)) %>% floor()
    ylim <- c(lowerbound, upperbound)
    sea <- getSeason(hindcast)
    # plotting ---------------------------------
    par(bg = "white", mar = c(2.5,3,4,1.5))
    plot(1:(length(sea) + 2), seq(lowerbound, upperbound, length.out = length(sea) + 2),
         ty = "n", axes = FALSE, xlab = "", ylab = "", ylim = ylim)
    tick.pos <-  1:(length(sea) + 2) - .25
    axis(1, at = tick.pos, labels = c("", month.name[getSeason(hindcast)], ""))
    axis(2, las = 1)
    grid(nx = NA, ny = NULL)
    x.pos <- tick.pos[2:(length(tick.pos) - 1)]
    abline(v = x.pos, lty = "dotted", col = "lightgrey")
    # Observations ------------------------
    if (!is.null(obs)) {
        ref <- sapply(sea, function(x) subsetGrid(obs, season = x) %>% extract2("Data"))
        q.obs <- apply(ref, MARGIN = 2, FUN = "quantile", prob = c(0, 1/3, 1/2, 2/3, 1), na.rm = TRUE)
        # Climatology tercile shadows -----------
        polygon(c(x.pos, rev(x.pos)), y = c(q.obs[1, ], rev(q.obs[5, ])), border = NA, col = "grey80") 
        polygon(c(x.pos, rev(x.pos)), y = c(q.obs[2, ], rev(q.obs[4, ])), border = NA, col = "grey40") 
    }
    # hindcast ----------------
    hindm <- suppressMessages(aggregateGrid(hindcast, aggr.mem = list(FUN = "mean", na.rm = TRUE)))
    hind <- sapply(sea, function(x) subsetGrid(hindm, season = x, drop = TRUE) %>% extract2("Data"))
    ter.hind <- apply(hind, MARGIN = 2, FUN = "quantile", prob = 1:2/3, na.rm = TRUE)
    # forecast ----------------
    fore <- sapply(sea, function(x) subsetGrid(forecast, season = x) %>% extract2("Data"))    
    if (violin) {
        for (i in 1:length(sea)) {
            vioplot(hind[,i], wex = .55, border = "black",
                    col = "thistle", add = TRUE, axes = FALSE,
                    at = x.pos[i], colMed = "black")
            vioplot(fore[,i], wex = .35, border = "black",
                    col = "white", add = TRUE, axes = FALSE,
                    at = x.pos[i], colMed = "black")
        }
    } else {
        boxplot(hind, add = TRUE, axes = FALSE,
                pars = list(boxwex = .45), at = x.pos, col = "thistle")
        boxplot(fore, add = TRUE, axes = FALSE,
                pars = list(boxwex = .25), at = x.pos, col = "white")
        box()
    }
    if (add.points) {
        for (i in 1:length(sea)) {
            points(rep(x.pos[i], nrow(fore)), fore[,i], pch = 4)
        }
    }
    # Obs median
    if (!is.null(obs)) {
        lines(x.pos, q.obs[3,], ty = "o", pch = 0, lty = 2, cex = 1.5)    
    }
    # Text members per tercile ------------------
    for (i in 1:length(sea)) {
        x.text <- x.pos[i] + .3
        which(fore[,i] > ter.hind[2,i]) %>% length() %>% text(x = x.text, y = max(fore[,i], na.rm = TRUE))    
        which(fore[,i] > ter.hind[1,i] & fore[,i] <= ter.hind[2,i]) %>% length() %>% text(x = x.text, y = median(fore[,i], na.rm = TRUE))    
        which(fore[,i] <= ter.hind[1,i]) %>% length() %>% text(x = x.text, y = min(fore[,i], na.rm = TRUE))
    }
    if (add.legend) {
        if (is.null(obs)) {
            legend("right", legend = c("forecast", "hindcast"), pch = 22,
                   pt.bg = c("white", "thistle"), bty = "n", pt.cex = 2)
        } else {
            legend("right",
                   legend =  c("forecast", "hindcast", "obs T1/T3", "obs T2", "obs med"),
                   pch = c(rep(22,4), 0), pt.cex = c(rep(2,4),1),
                   pt.bg = c("white", "thistle", "grey80", "grey40", NULL),
                   lty = c(rep(0,4), 2),
                   bty = "n")
        }
    }
    # EQC information ------------------------------
    if (add.eqc.info) {
        message("[", Sys.time(),"] Calculating CRPS and RPS...")
        ens <- suppressMessages(aggregateGrid(hindcast, aggr.y = list(FUN = "mean", na.rm = TRUE))) %>% extract2("Data") %>% t()
        ob <- suppressMessages(redim(obs, member = FALSE) %>% aggregateGrid(aggr.y = list(FUN = "mean", na.rm = TRUE))) %>% extract2("Data") %>% as.matrix() %>% drop()
        crps <- FairCrps(ens, ob) %>% mean() %>% round(digits = 2)
        rps <- FairRps(ens, ob) %>% mean() %>% round(digits = 2)
        legend("topright", c(paste0("RPS=", rps), paste0("CRPS=", crps)), bty = "n")
    }
    # Metadata info ---------------------------------
    fsname <- attr(hindcast, "dataset")
    l1 <- paste0("Forecasting System: ", fsname, " - ", getShape(hindcast, "member"), " members")
    ind <- forecast$InitializationDates %>% unlist() %>% substr(start = 6, stop = 7) %>% as.integer() %>% max()
    l2 <- paste(month.name[ind], "initializations")
    l3 <- if (!is.null(obs)) {
        refname <- attr(obs, "dataset")
        paste0("Observing System: ", refname)
    } else {
        NULL  
    }     
    vn <- hindcast$Variable$varName
    seastring <- paste(substr(month.name[getSeason(hindcast)], 1, 1), collapse = "")
    yrs <- getYearsAsINDEX(hindcast) %>% range() %>% paste(collapse = "-")
    l4 <- paste0(vn , " - ", seastring, " (", yrs ,")")
    main <- ifelse(!is.null(l3), paste(l1, l2, l3, l4, sep = "\n"), paste(l1, l2, l4, sep = "\n"))
    title(main = list(main, cex = .9, col = "blue", font = 1), adj = 0)
    message("[", Sys.time(),"] Done")        
}
# End


