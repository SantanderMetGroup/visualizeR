#' @title NCEP/NCAR reanalysis DJF mean temperature for the entire globe
#' @description NCEP/NCAR reanalysis DJF mean temperature for the entire globe (1949-2010). Data are annually aggregated.
#' @format A grid
#' @source Dowloaded from the ECOMS User Data Gateway \url{http://www.meteo.unican.es/ecoms-udg}
#' @name tas.ncep
#' @examples
#' data(tas.ncep)
#' require(transformeR)
#' plotClimatology(climatology(tas.ncep), backdrop.theme = "coastline")
NULL


#' @title NCEP CFSv2 hindcast of DJF mean temperature for the entire globe.
#' @description NCEP CFSv2 hindcast of DJF mean temperature for the entire globe (1983-2010). Data are annually aggregated.
#' Ensemble of 24 members, corresponding to the lead month 1 predictions (November initializations)
#' @format A grid
#' @description For further detail on ensemble member definition, see \url{http://www.meteo.unican.es/ecoms-udg/dataserver/datasets/CFSv2}
#' @source Dowloaded from the ECOMS User Data Gateway \url{http://www.meteo.unican.es/ecoms-udg}
#' @name tas.cfs
#' @references Saha, S. et al. 2013. The NCEP Climate Forecast System Version 2. J Clim 130925135638001. doi:10.1175/JCLI-D-12-00823.1
#' @examples \dontrun{
#' data(tas.cfs)
#' require(transformeR)
#' plotClimatology(climatology(tas.cfs), backdrop.theme = "coastline")
#' }
NULL


#' @title CFSv2 operative seasonal forecast of global DJF mean temperature (2016)
#' @description CFSv2 operative seasonal forecast of DJF mean temperature for the entire globe of year 2016
#'  (i.e. December 2015, Januery-February 2016). Data are annually aggregated. Ensemble of 24 members,
#'   corresponding to the lead month 1 predictions (November 2015 initializations).
#' @format A grid
#' @description For further detail on ensemble member definition, see \url{http://www.meteo.unican.es/ecoms-udg/dataserver/datasets/CFSv2}
#' @source Dowloaded from the ECOMS User Data Gateway \url{http://www.meteo.unican.es/ecoms-udg}
#' @name tas.cfs.operative.2016
#' @references Saha, S. et al. 2013. The NCEP Climate Forecast System Version 2. J Clim 130925135638001. doi:10.1175/JCLI-D-12-00823.1
#' @examples \dontrun{
#' data(tas.cfs.operative.2016)
#' require(transformeR)
#' plotClimatology(climatology(tas.cfs.operative.2016, parallel = TRUE), backdrop.theme = "coastline")
#' }
NULL

#' @title CFSv2 operative seasonal forecast of global DJF mean temperature (2017)
#' @description CFSv2 operative seasonal forecast of DJF mean temperature for the entire globe of year 2017
#'  (i.e. December 2016, Januery-February 2017). Data are annually aggregated. Ensemble of 24 members,
#'   corresponding to the lead month 1 predictions (November 2016 initializations).
#' @format A grid
#' @description For further detail on ensemble member definition, see \url{http://www.meteo.unican.es/ecoms-udg/dataserver/datasets/CFSv2}
#' @source Dowloaded from the ECOMS User Data Gateway \url{http://www.meteo.unican.es/ecoms-udg}
#' @name tas.cfs.operative.2017
#' @references Saha, S. et al. 2013. The NCEP Climate Forecast System Version 2. J Clim 130925135638001. doi:10.1175/JCLI-D-12-00823.1
#' @examples \dontrun{
#' data(tas.cfs.operative.2017)
#' require(transformeR)
#' plotClimatology(climatology(tas.cfs.operative.2017, parallel = TRUE), backdrop.theme = "coastline")
#' }
NULL

#' @title AR5 regions from the IPCC 5th Assessment Report.
#' @description Spatial Polygons of the geographical regions used in the IPCC 5th Assessment Report.
#' @format A \code{\link[sp]{SpatialPolygons-class}} object
#' @description For further detail, see \url{http://www.ipcc-data.org/guidelines/pages/ar5_regions.html}
#' @source Shapefile imported with function \code{\link[rgdal]{readOGR}} (\url{http://www.ipcc-data.org/guidelines/pages/ar5_regions.html})
#' @name AR5regions
#' @examples \dontrun{
#' data(AR5regions)
#' sp::plot(AR5regions, border = "red", axes = TRUE)
#' transformeR:::draw.world.lines()
#' title("AR5 World regions")
#' }
NULL

#' @title PRUDENCE regions.
#' @description Spatial Polygons of the geographical regions used in the PRUDENCE project.
#' @format A \code{\link[sp]{SpatialPolygons-class}} object
#' @description For further detail, see \url{http://ensemblesrt3.dmi.dk/quicklook/regions.html}
#' @name PRUDENCEregions
#' @examples \dontrun{
#' data(PRUDENCEregions)
#' sp::plot(PRUDENCEregions, border = "red", axes = TRUE)
#' transformeR:::draw.world.lines()
#' title("PRUDENCE European regions")
#' }
NULL
