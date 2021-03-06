% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rdas.R
\name{AR6_WGI_AtlasSynthesisRegions}
\alias{AR6_WGI_AtlasSynthesisRegions}
\title{AR6 WGI synthesis regions from the Atlas chapter of the IPCC 6th Assessment Report (land).}
\format{
A \code{\link[sp]{SpatialPolygons-class}} object
}
\description{
Spatial Polygons of the geographical regions used in the IPCC 6th Assessment Report.
}
\details{
Display region names : names(AR6_WGI_AtlasSynthesisRegions).
}
\examples{
\dontrun{
data(AR6_WGI_AtlasSynthesisRegions)

# All regions
sp::plot(AR6_WGI_AtlasSynthesisRegions, border = "red", axes = TRUE)
draw.world.lines()
title("AR6 World land regions")

# A single region (use region names for subsetting)
sp::plot(AR6_WGI_AtlasSynthesisRegions["Africa"], border = "red", axes = TRUE)
draw.world.lines()
title("AR6 Mediterranean region")
}
}
