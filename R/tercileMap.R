hindcast <- subsetGrid(CFS_Iberia_tas, years = 1983:2001)
forecast <- subsetGrid(CFS_Iberia_tas, years = 2002)
library(downscaleR)
tercileMap <- function(hindcast, forecast){
    hindyear <- aggregateGrid(hindcast, aggr.y = list(FUN = mean, na.rm = TRUE))
    foreyear <- aggregateGrid(forecast, aggr.y = list(FUN = mean, na.rm = TRUE))
    hind <- redim(downscaleR:::flatMemberDim(hindyear), drop = TRUE)
    a <- apply(hind$Data, MARGIN = c(2,3), FUN = quantile, probs = c(1/3, 2/3))
    nmem <- getShape(forecast)["member"]
    for (i in 1:nmem) {
      formem <- subsetGrid(foreyear, members = i)$data
    }
    
}
