##     climagram.R Implementation of the so-called "climagrams" for seasonal forecasts
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

#' @title Implementation of the so-called "climagrams" for seasonal forecasts
#' @description A "climagram" is a box plot for visualization of a seasonal forecast with added EQC information from the hindcast
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
#' @param clim.time.frame Default to \code{"monthly"}. Temporal frame used as reference for the calculation of the climatological
#' normal. See Details. 
#' @param add.eqc.info Should EQC (Evaluation and Quality Control) information be added onto the plot?.
#' Ignored if no \code{obs} is passed. See Details
#' @param tercile.member.counts Logical flag indicating whether the number of forecast members in each tercile
#' category should be indicated in the plot. Default to \code{FALSE}.
#' 
#' @return 
#' 
#' Plots a climagram. Some degree of transparency is given
#' to the forecast boxes/violins to avoid a potential masking of underlying information.
#' 
#' @details 
#' 
#' \strong{Forecast data}
#' 
#' The product is conceived to display forecast information in an operative framework. However, for
#' testing purposes, a target year from the hindcast can be extracted via the \code{year.target} argument and 
#' treated as the forecast year (see last example). Note that all the hindcast statistics 
#' (including the optional detrending) will be computed after extracting the target year from the hindcast 
#' (and the observations).
#' 
#' \strong{Spatial averaging}
#' 
#' For spatial domains encompassing multiple points/grid points, the spatial average is first computed to obtain a
#' unique series for the whole domain via \code{\link[transformeR]{aggregateGrid}} (latitude cosine 
#' weighting is used).
#' 
#' \strong{Forecast/Hindcast representation}
#'   
#' In order to represent the forecast and hindcast information, \dQuote{violins} are used as default,
#' although boxplots (see \code{\link[graphics]{boxplot}}) can be obtained by setting 
#' argument \code{violin} to \code{FALSE}. For further details on violin construction see
#' the documentation of the function \code{\link[vioplot]{vioplot}}.
#'  
#' \strong{Using anomalies}
#' 
#' By default, anomalies are displayed. In this case, the forecast anomalies 
#' will be calculated considering the hindcast climatology (see next section for details). 
#' 
#' \strong{Climatological reference period}
#' 
#' The hindcast climatology is calculated by considering the annual multimember averages
#'  (i.e., the univariate annual time series of ensemble means).
#'     
#' By default, the climatology is computed on a monthly basis. For instance,
#' assuming a forecast for DJF is given, the DJF anomaly will be computed by subtracting to the 
#' January forecast the January climatology, to February the February climatology and so on.
#' If \code{clim.time.frame} is set to \code{"monthly"}, the climatology is computed using the seasonal climatology
#' , so the anomalies for January will be computed using the DJF climatology, and the same for the remaining months.
#' See argument \code{"time.frame"} of \code{\link[transformeR]{scaleGrid}}, to which the \code{clim.time.frame}
#' argument is passed, for more details.
#' 
#' \strong{EQC Information}  
#' 
#' EQC (Evaluation and Quality Control) information can be optionally added to the plot. This is the default behaviour when 
#' \code{obs} is supplied. Here the CRPS (bias-sensitive) measure is considered.
#'       
#' @note 
#' 
#' All inputs for this function must be at a monthly temporal resolution.
#' 
#' The computation of climatological quantiles requires a representative period to obtain meaningful results.
#'  
#' @author J. Bedia
#' 
#' @family visualization functions
#' @importFrom transformeR subsetGrid checkDim checkSeason checkTemporalConsistency aggregateGrid detrendGrid getYearsAsINDEX getTimeResolution scaleGrid getSeason getGridUnits 
#' @importFrom stats filter median
#' @importFrom graphics plot axis grid boxplot box legend
#' @importFrom magrittr %>% extract2 %<>% 
#' @importFrom vioplot vioplot
#' @importFrom easyVerification indRef generateRef
#' @importFrom SpecsVerification EnsCrps SkillScore
#' @importFrom grDevices adjustcolor
#' @export
#' @examples \dontrun{
#' my_load <- function(file.url, verbose = TRUE) {
#'     tmpfile <- tempfile()
#'     download.file(url = file.url, destfile = tmpfile)
#'     readRDS(tmpfile)
#' }
#' hindcast <- my_load("http://meteo.unican.es/work/visualizeR/data/tas.cfs.enso34.rds")
#' forecast <- my_load("http://meteo.unican.es/work/visualizeR/data/tas.cfs.2016.enso34.rds")
#' # A seasonal forecast product, without reference obs information
#' climagram(hindcast = hindcast, forecast = forecast, violin = TRUE, obs = NULL)
#' # Violins can be replaced by ordinary boxplots:
#' climagram(hindcast = hindcast, forecast = forecast, violin = FALSE, obs = NULL)
#' # Instead of anomalies, raw values can be used: 
#' climagram(hindcast = hindcast, forecast = forecast, use.anomalies = FALSE)
#' # Observation quartiles are depicted by the backgroud shadows (median indicated by the dotted line):
#' # CRPS is added by default when observations are included:
#' obs <- my_load("http://meteo.unican.es/work/visualizeR/data/tas.ncep.enso34.rds")
#' climagram(hindcast = hindcast, forecast = forecast, obs = obs)
#' # An arbitrary year can be extracted from the hindcast to be used as forecast
#' climagram(hindcast = hindcast, year.target = 2001, obs = obs)
#' # Adding tercile counts:
#' climagram(hindcast = hindcast, year.target = 2001, obs = obs, tercile.member.count = TRUE)
#' }
#' @seealso Type \code{utils::RShowDoc("climagram_vignette", package = "visualizeR")} for further worked
#'  examples on climagram options.

climagram <- function(hindcast,
                      forecast = NULL,
                      obs = NULL,
                      year.target = NULL,
                      use.anomalies = TRUE,
                      violin = TRUE,
                      add.points = FALSE,
                      detrend = FALSE,
                      add.legend = TRUE,
                      clim.time.frame = "monthly",
                      add.eqc.info = TRUE,
                      tercile.member.counts = FALSE) {
    # Check data dimension from the original data sets
    message("[", Sys.time(),"] Checking input consistency ...")
    stopifnot(is.logical(detrend))
    stopifnot(is.logical(use.anomalies))
    stopifnot(is.logical(violin))
    stopifnot(is.logical(add.points))
    stopifnot(is.logical(add.legend))
    stopifnot(is.logical(add.eqc.info))
    stopifnot(is.logical(tercile.member.counts))
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
        hindcast  %<>%  subsetGrid(years = year.hind, drop = FALSE)
        if (!is.null(obs)) {
            obs %<>% subsetGrid(years = year.hind, drop = FALSE)
        }
    } else {
        transformeR::checkDim(hindcast, forecast, dimensions = c("member", "lat", "lon"))    
        checkSeason(hindcast, forecast)
    }
    if (!is.null(obs)) {
        transformeR::checkTemporalConsistency(hindcast, obs)
        transformeR::checkDim(hindcast, obs, dimensions = c("time", "lat", "lon"))
    } else {
        if (add.eqc.info) add.eqc.info <- FALSE
        message("NOTE: 'add.eqc.info' was set to FALSE, as no observations were provided")
    }
    eastings <- getGrid(forecast)$x %>% round() %>% paste(collapse = " : ") %>% paste("E")
    northings <- getGrid(forecast)$y %>% round() %>% paste(collapse = " : ") %>% paste("N")
    # Detrend
    if (detrend) {
        message("[", Sys.time(),"] Detrending ...")
        hindcast <- suppressMessages(detrendGrid(hindcast)) 
        forecast <- suppressMessages(detrendGrid(hindcast, grid2 = forecast))
        if (!is.null(obs)) obs <- suppressMessages(detrendGrid(obs)) 
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
    hindm <- suppressMessages(aggregateGrid(hindcast, aggr.mem = list(FUN = "mean", na.rm = TRUE)))
    upperbound <- max(max(hindm$Data, na.rm = TRUE), max(forecast$Data, na.rm = TRUE)) %>% ceiling()
    lowerbound <- min(min(hindm$Data, na.rm = TRUE), min(forecast$Data, na.rm = TRUE)) %>% floor()
    ylim <- c(lowerbound, upperbound)
    sea <- getSeason(hindcast)
    # plotting ---------------------------------
    vn <- forecast$Variable$varName
    uds <- getGridUnits(forecast)
    ylab <- if (use.anomalies) {
        paste0(vn, " anomaly (", uds, ")")
    } else {
        paste0(vn, " (", uds, ")")
    }
    par(bg = "white", mar = c(2.5,4,4,1.5))
    plot(1:(length(sea) + 2), seq(lowerbound, upperbound, length.out = length(sea) + 2),
         ty = "n", axes = FALSE, xlab = "", ylab = ylab, ylim = ylim)
    tick.pos <-  1:(length(sea) + 2) - .25
    axis(1, at = tick.pos, labels = c("", month.name[getSeason(hindcast)], ""))
    axis(2, las = 1)
    grid(nx = NA, ny = NULL)
    x.pos <- tick.pos[2:(length(tick.pos) - 1)]
    abline(v = x.pos, lty = "dotted", col = "lightgrey")
    # Observations ------------------------
    if (!is.null(obs)) {
        ref <- sapply(sea, function(x) subsetGrid(obs, season = x) %>% extract2("Data"))
        q.obs <- apply(ref, MARGIN = 2, FUN = "quantile", prob = 0:4/4, na.rm = TRUE)
        # Climatology tercile shadows -----------
        obs.rng.color <- "grey80"
        obs.iqr.color <- "grey40"
        polygon(c(x.pos, rev(x.pos)), y = c(q.obs[1, ], rev(q.obs[5, ])), border = NA, col = obs.rng.color) 
        polygon(c(x.pos, rev(x.pos)), y = c(q.obs[2, ], rev(q.obs[4, ])), border = NA, col = obs.iqr.color) 
        lines(x.pos, q.obs[3,], lty = 2) # , cex = 1.5, ty = "o", pch = 0)    
    }
    # hindcast ----------------
    hind <- sapply(sea, function(x) subsetGrid(hindm, season = x, drop = TRUE) %>% extract2("Data"))
    ter.hind <- apply(hind, MARGIN = 2, FUN = "quantile", prob = 1:2/3, na.rm = TRUE)
    # forecast ----------------
    fore <- sapply(sea, function(x) subsetGrid(forecast, season = x) %>% extract2("Data"))
    hind.color <- adjustcolor("white", alpha.f = .65)
    fore.color <- adjustcolor("thistle", alpha.f = .85)
    if (violin) {
        for (i in 1:length(sea)) {
            vioplot(hind[,i], wex = .55, border = "black",
                    col = hind.color, add = TRUE, axes = FALSE,
                    at = x.pos[i], colMed = "black")
            vioplot(fore[,i], wex = .35, border = "black",
                    col = fore.color, add = TRUE, axes = FALSE,
                    at = x.pos[i], colMed = "purple4", rectCol = "purple3")
        }
    } else {
        boxplot(hind, add = TRUE, axes = FALSE,
                pars = list(boxwex = .45), at = x.pos, col = hind.color)
        boxplot(fore, add = TRUE, axes = FALSE,
                pars = list(boxwex = .25), at = x.pos, col = fore.color)
        box()
    }
    if (add.points) {
        for (i in 1:length(sea)) {
            points(rep(x.pos[i], nrow(fore)), fore[,i], pch = 4)
        }
    }
    # Text members per tercile ------------------
    if (tercile.member.counts) {
        for (i in 1:length(sea)) {
            x.text <- x.pos[i] - .33
            which(fore[,i] > ter.hind[2,i]) %>% length() %>% text(x = x.text, y = quantile(hind[,i], probs = .66 + .165, na.rm = TRUE))    
            which(fore[,i] > ter.hind[1,i] & fore[,i] <= ter.hind[2,i]) %>% length() %>% text(x = x.text, y = median(hind[,i], na.rm = TRUE))    
            which(fore[,i] <= ter.hind[1,i]) %>% length() %>% text(x = x.text, y = quantile(hind[,i], prob = .165, na.rm = TRUE))
        }
    }
    if (add.legend) {
        if (is.null(obs)) {
            legend("right", legend = c("forecast", "hindcast"), pch = 22,
                   pt.bg = c(fore.color, hind.color), bty = "n", pt.cex = 2)
        } else {
            legend("right",
                   legend =  c("forecast", "hindcast", "obs IQR", "obs range", "obs med"),
                   pch = c(rep(22,4), 0), pt.cex = c(rep(2,4), 0),
                   pt.bg = c(fore.color, hind.color, obs.iqr.color, obs.rng.color, NULL),
                   lty = c(rep(0,4), 2),
                   bty = "n", cex = .9)
        }
    }
    # EQC information ------------------------------
    n.mem <- getShape(hindcast, "member")
    if (add.eqc.info) {
        message("[", Sys.time(),"] Calculating CRPSS...")
        ens <- suppressMessages(aggregateGrid(hindcast, aggr.y = list(FUN = "mean", na.rm = TRUE))) %>% redim(drop = TRUE) %>% extract2("Data") %>% t()
        ob <- suppressMessages(redim(obs, member = FALSE) %>% aggregateGrid(aggr.y = list(FUN = "mean", na.rm = TRUE))) %>% extract2("Data") %>% as.matrix() %>% drop()
        ind <- indRef(nfcst = length(ob))
        clim.ref <- generateRef(ob, ind)
        crpss <- SkillScore(EnsCrps(ens, ob, R.new = Inf),
                            scores.ref = EnsCrps(clim.ref, ob, R.new = Inf)) %>% round(digits = 2)
        legend("topright", paste0("CRPSS\n", paste(crpss, collapse = "\u00B1")), bty = "n")
    }
    # Metadata info ---------------------------------
    fsname <- attr(hindcast, "dataset")
    ind <- attr(forecast$Variable, "initMonth")
    if (is.null(ind)) {
        ind <- forecast$InitializationDates %>% unlist() %>% substr(start = 6, stop = 7) %>% as.integer() %>% max()    
    }
    init.month <- month.name[ind]
    fyr <- getYearsAsINDEX(forecast) %>% unique()
    seastring <- paste(substr(month.name[getSeason(hindcast)], 1, 1), collapse = "")
    yrs <- getYearsAsINDEX(hindcast) %>% range() %>% paste(collapse = "-")
    l1 <- paste0("Forecasting System: ", fsname, " - ", n.mem, " members")
    l2 <- paste0(vn , " ", seastring, " - Forecast start: ", init.month, " ", fyr)
    l3 <- if (!is.null(obs)) {
        refname <- attr(obs, "dataset")
        paste0("Observing System: ", refname)
    } else {
        NULL  
    }     
    l4 <- if (is.null(year.target)) {
        paste0("Reference period: ", yrs)  
    } else {
        paste0("Reference period: ", yrs, " (excluding ", year.target, ")")  
    }
    main <- ifelse(!is.null(l3), paste(l2, l1, l3, l4, sep = "\n"), paste(l2, l1, l4, sep = "\n"))
    title(main = list(main, cex = .9, col = "blue", font = 1), adj = 0)
    geo <- paste("", northings, eastings, sep = "\n")
    title(main = list(geo, cex = .9, col = "blue", font = 1), adj = 1)
    message("[", Sys.time(),"] Done")        
}
# End


