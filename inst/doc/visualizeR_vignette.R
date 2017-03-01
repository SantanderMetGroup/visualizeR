## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                 message = FALSE,
                 warning = FALSE,
                 fig.align = "center",
                 tidy = TRUE,
                 cache = FALSE,
                 fig.width = 9,
                 fig.height = 5)

## ------------------------------------------------------------------------
library(visualizeR)
library(transformeR)

## ------------------------------------------------------------------------
# Load reanalysis
data(tas.ncep)
# Load hindcast
data(tas.cfs)
# Load operative predictions
data(tas.cfs.operative.2016)

## ---- echo=TRUE----------------------------------------------------------
(years <- unique(getYearsAsINDEX(tas.cfs)))

## ------------------------------------------------------------------------
obs <- subsetGrid(tas.ncep, years = years)

## ------------------------------------------------------------------------
hindcast <- tas.cfs
forecast <- tas.cfs.operative.2016

## ------------------------------------------------------------------------
# grid definition as a names list with 'x' and 'y' components
newgrid <- list(x = c(getGrid(hindcast)$x, 5), y = c(getGrid(hindcast)$y, 5))
hindcast <- interpGrid(hindcast, new.coordinates = getGrid(obs), method = "bilinear",
                       bilin.method = "fields", parallel = TRUE)
forecast <- interpGrid(forecast, new.coordinates = getGrid(obs), method = "bilinear",
                       bilin.method = "fields", parallel = TRUE)

## ----message=FALSE,warning=FALSE-----------------------------------------
# This is a subtitle that will be added to each plot
subtitle <- sprintf("Reference data: NCEP Reanalysis;  Hindcast: CFSv2 (%d members); %d-%d",
                    getShape(hindcast, dimension = "member"),
                    getYearsAsINDEX(hindcast)[1],
                    tail(getYearsAsINDEX(hindcast),1))

## ----message=FALSE,warning=FALSE-----------------------------------------
bubblePlot(hindcast = hindcast,
           obs = obs,
           forecast = forecast,
           size.as.probability = FALSE,
           bubble.size = 1.5,
           score = FALSE,
           subtitle = subtitle)

## ----message=FALSE,warning=FALSE-----------------------------------------
bubblePlot(hindcast,
           obs = obs,
           forecast = forecast,
           size.as.probability = TRUE,
           bubble.size = 1.5,
           score = FALSE,
           subtitle = subtitle)

## ----message=FALSE,warning=FALSE-----------------------------------------
bubblePlot(hindcast,
           obs,
           forecast = forecast,
           size.as.probability = TRUE,
           bubble.size = 1.5,
           score = TRUE,
           subtitle = subtitle)

## ----message=FALSE,warning=FALSE-----------------------------------------
bubblePlot(hindcast,
           obs,
           forecast = forecast,
           size.as.probability = TRUE,
           bubble.size = 1.5,
           score = TRUE,
           score.range = c(0.5, 1),
           subtitle = subtitle)

## ----message=FALSE,warning=FALSE-----------------------------------------
lon <- c(-90, 30)
lat <- c(35, 80)
hind.NA <- subsetGrid(hindcast, lonLim = lon, latLim = lat)
obs.NA <- subsetGrid(obs, lonLim = lon, latLim = lat)
forecast.NA <- subsetGrid(forecast, lonLim = lon, latLim = lat)
bubblePlot(hind.NA,
           obs.NA,
           forecast = forecast.NA,
           piechart = TRUE,
           score = TRUE,
           score.range = c(0.5,1),
           subtitle = subtitle)

## ------------------------------------------------------------------------
# El Nino 3.4 
crop.nino <- function(x) subsetGrid(x, lonLim = c(-170, -120), latLim = c(-5, 5))
hindcast.nino <- crop.nino(hindcast)
obs.nino <- crop.nino(obs)
forecast.nino <- crop.nino(forecast)

## ------------------------------------------------------------------------
tercilePlot(hindcast = hindcast.nino,
            obs = obs.nino,
            forecast = forecast.nino,
            subtitle = subtitle)

## ------------------------------------------------------------------------
# Europe (35-45N and -10-42)
crop.eu <- function(x) subsetGrid(x, lonLim = c(-10, 40), latLim = c(35, 47))
hindcast.eu <- crop.eu(hindcast)
obs.eu <- crop.eu(obs)
forecast.eu <- crop.eu(forecast)

## ------------------------------------------------------------------------
tercilePlot(hindcast.eu, obs.eu, forecast = forecast.eu, subtitle = subtitle)

## ---- echo=FALSE,eval=FALSE----------------------------------------------
#  # bubblePlot(hindcast.eu, obs.eu, forecast=forecast.eu, size.as.probability=T, bubble.size=1.5, score=T, subtitle=subtitle)
#  # bubblePlot(hindcast.eu, obs.eu, forecast=forecast.eu, piechart=T, score=T, subtitle=subtitle)
#  # tercileBarplot(hindcast.eu, obs.eu, forecast=forecast.eu, score.threshold= 0.3, subtitle=subtitle)

