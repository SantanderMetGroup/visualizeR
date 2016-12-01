#' @title NCEP/NCAR reanalysis DJF mean sea-level pressure for the entire globe
#' @description NCEP/NCAR reanalysis DJF mean sea-level pressure for the entire globe (1949-2010). Data are annually aggregated.
#' @format A grid
#' @source Dowloaded from the ECOMS User Data Gateway \url{http://www.meteo.unican.es/ecoms-udg}
#' @name psl.ncep
#' @examples
#' data(psl.ncep)
#' require(transformeR)
#' plotClimatology(climatology(psl.ncep), backdrop.theme = "coastline")
NULL


#' @title NCEP CFSv2 hindcast of DJF mean sea-level pressure for the entire globe.
#' @description NCEP CFSv2 hindcast for DJF mean sea-level pressure for the entire globe (1983-2010). Data are annually aggregated.
#' Ensemble of 24 members, corresponding to the lead month 1 predictions (November initializations)
#' @format A grid
#' @description For further detail on ensemble member definition, see \url{http://www.meteo.unican.es/ecoms-udg/dataserver/datasets/CFSv2}
#' @source Dowloaded from the ECOMS User Data Gateway \url{http://www.meteo.unican.es/ecoms-udg}
#' @name psl.cfs
#' @references Saha, S. et al. 2013. The NCEP Climate Forecast System Version 2. J Clim 130925135638001. doi:10.1175/JCLI-D-12-00823.1
#' @examples \dontrun{
#' data(psl.cfs)
#' require(transformeR)
#' plotClimatology(climatology(psl.cfs), backdrop.theme = "coastline")
#' }
NULL


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


#' @title NCEP/NCAR reanalysis DJF total precipitation amount for the entire globe.
#' @description NCEP/NCAR reanalysis DJF total precipitation amount for the entire globe (1949-2010). Data are annually aggregated.
#' @format A grid
#' @source Dowloaded from the ECOMS User Data Gateway \url{http://www.meteo.unican.es/ecoms-udg}
#' @name tp.ncep
#' @examples
#' data(tp.ncep)
#' require(transformeR)
#' plotClimatology(climatology(tp.ncep), backdrop.theme = "coastline")
NULL


#' @title NCEP CFSv2 hindcast of DJF total precipitation amount for the entire globe.
#' @description NCEP CFSv2 hindcast of DJF total precipitation amount for the entire globe (1983-2010). Data are annually aggregated.
#' Ensemble of 24 members, corresponding to the lead month 1 predictions (November initializations)
#' @format A grid
#' @description For further detail on ensemble member definition, see \url{http://www.meteo.unican.es/ecoms-udg/dataserver/datasets/CFSv2}
#' @source Dowloaded from the ECOMS User Data Gateway \url{http://www.meteo.unican.es/ecoms-udg}
#' @name tp.cfs
#' @references Saha, S. et al. 2013. The NCEP Climate Forecast System Version 2. J Clim 130925135638001. doi:10.1175/JCLI-D-12-00823.1
#' @examples \dontrun{
#' data(tp.cfs)
#' require(transformeR)
#' plotClimatology(climatology(tp.cfs), backdrop.theme = "coastline")
#' }
NULL


#' @title CFSv2 operative seasonal forecast of global DJF total precipitation amount (2016)
#' @description CFSv2 operative seasonal forecast of DJF total precipitation amount for the entire globe of year 2016
#'  (i.e. December 2015, Januery-February 2016). Data are annually aggregated. Ensemble of 24 members,
#'   corresponding to the lead month 1 predictions (November 2015 initializations).
#' @format A grid
#' @description For further detail on ensemble member definition, see \url{http://www.meteo.unican.es/ecoms-udg/dataserver/datasets/CFSv2}
#' @source Dowloaded from the ECOMS User Data Gateway \url{http://www.meteo.unican.es/ecoms-udg}
#' @name tp.cfs.operative.2016
#' @references Saha, S. et al. 2013. The NCEP Climate Forecast System Version 2. J Clim 130925135638001. doi:10.1175/JCLI-D-12-00823.1
#' @examples \dontrun{
#' data(tp.cfs.operative.2016)
#' require(transformeR)
#' plotClimatology(climatology(tp.cfs.operative.2016, parallel = TRUE), backdrop.theme = "coastline")
#' }
NULL

#' @title CFSv2 operative seasonal forecast of global DJF total precipitation amount (2017)
#' @description CFSv2 operative seasonal forecast of DJF total precipitation amount for the entire globe of year 2017
#'  (i.e. December 2016, Januery-February 2017). Data are annually aggregated. Ensemble of 24 members,
#'   corresponding to the lead month 1 predictions (November 2016 initializations).
#' @format A grid
#' @description For further detail on ensemble member definition, see \url{http://www.meteo.unican.es/ecoms-udg/dataserver/datasets/CFSv2}
#' @source Dowloaded from the ECOMS User Data Gateway \url{http://www.meteo.unican.es/ecoms-udg}
#' @name tp.cfs.operative.2017
#' @references Saha, S. et al. 2013. The NCEP Climate Forecast System Version 2. J Clim 130925135638001. doi:10.1175/JCLI-D-12-00823.1
#' @examples \dontrun{
#' data(tp.cfs.operative.2017)
#' require(transformeR)
#' plotClimatology(climatology(tp.cfs.operative.2017, parallel = TRUE), backdrop.theme = "coastline")
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

