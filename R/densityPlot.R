densityPlot <- function(...,
                        group.index = NULL,
                        lonLim = NULL,
                        latLim = NULL,
                        dplot.custom = list()) {
  obj.list <- list(...)
  if (gridDepth(obj.list) > 3) obj.list <- unlist(obj.list, recursive = FALSE)
  if (is.null(names(obj.list))) {
    nmes <- as.character(as.list(substitute(list(...)))[-1L])
    if (length(nmes) < length(obj.list)) nmes <- paste0(nmes, 1:length(obj.list))
    names(obj.list) <- nmes
  }
  
  obj.list <- lapply(obj.list, function(x) subsetGrid(x, lonLim = lonLim, latLim = latLim))
  obj.list <- lapply(obj.list, FUN = redim)
  if (is.null(group.index)) {
    group.index <- rep("", length(obj.list))
    bwplot.custom[["strip"]] <- FALSE
  }
  data <- lapply(obj.list, "[[", "Data")
  # bind to data frames
  df <- lapply(1:length(obj.list), function(x){
    df0 <- data.frame(as.vector(data[[x]]),
                      rep(x, length(as.vector(data[[x]]))),
                      rep(group.index[x], length(as.vector(data[[x]]))))
    colnames(df0) <- c("value","Group","Index")
    return(df0)
  })
  dff <- do.call("rbind", df)
  
  if (!is.null(dplot.custom[["plot.points"]])) dplot.custom[["plot.points"]] <- FALSE
  if (is.null(dplot.custom[["col"]])) dplot.custom[["auto.key"]] <- TRUE
  if (is.null(dplot.custom[["lwd"]])) dplot.custom[["lwd"]] <- 1
  # if (is.null(dplot.custom[["layout"]])) dplot.custom[["layout"]] <- c(1, length(unique(group.index)))
  dplot.custom[["xlab"]] <- obj.list[[1]]$Variable$varName
  # dplot.custom[["x"]] <- ~ value
  dplot.custom[["x"]] <- dff$value
  dplot.custom[["data"]] <- dff
  dplot.custom[["groups"]] <- dff$Group
  
  output <- do.call("densityplot", dplot.custom) 
  return(output)
}