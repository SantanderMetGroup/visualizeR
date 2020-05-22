#' @title Plot EOFs
#' @description Plots an arbitrary number of EOFs. Useful to have a quick overview of the main spatial modes
#'  of a (possibly multimember) grid.
#' @param prinCompObj A PCA object as returned by \code{\link{prinComp}}
#' @param var Character string indicating the variable whose EOFs are to be displayed. If the PCA analysis has
#' been applied to 1 single grid, this argument can be omitted.
#' @param n.eofs Number of EOFs to be displayed. Default to NULL, indicating that all computed EOFS
#' will be represented
#' @param member An integer indicating the position of the member whose EOFs are to be displayed. Default 1, 
#' corresponding to the first member. Ignored for non multimember grids.
#' @param ... Further arguments passed to \code{\link[visualizeR]{spatialPlot}}.
#' @return A plot with as many panels as EOFs requested, in the original units of the variable
#' @details
#' This function was formerly in transformeR. It is planned to de deprecated, and superseded by spatialPlot 
#' @export
#' @importFrom magrittr %>% 
#' @importFrom transformeR EOF2clim
#' @author J. Bedia 
#' @family pca
#' @examples \donttest{
#' require(climate4R.datasets) 
#' require(transformeR)
#' # Winter temperature at 850 mb isobaric surface pressure level is loaded (period 1981--2010):
#' data("NCEP_Iberia_hus850")
#' # PCA analysis, retaining the PCs that explain 90\% of the total variance:
#' pca <- prinComp(NCEP_Iberia_hus850, v.exp = .90)
#' # Plot of all EOFs
#' plotEOF(pca)
#' # Plot the first 4 EOFs:
#' plotEOF(pca, n.eofs = 4, backdrop.theme = "coastline")
#' # Plot just the second EOF (passing further arguments to spatialPlot):
#' plotEOF(pca, zcol = 2, backdrop.theme = "coastline", main = "2nd EOF ('zcol = 2')")
#' # Example with PCA analysis of a multigrid (multiple variables)
#' data("NCEP_Iberia_ta850", "NCEP_Iberia_psl")
#' multigrid <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_ta850, NCEP_Iberia_psl)
#' # PCA analysis, retaining the first 9 PCs of each variable:
#' pca2 <- prinComp(multigrid, n.eofs = 9)
#' names(pca2)
#' # EOFs for temperature 850mb
#' plotEOF(pca2, "ta@@850", backdrop.theme = "coastline")
#' }

plotEOF <- function(prinCompObj, var = NULL, member = 1, n.eofs = NULL, ...) {
    .Deprecated(msg = "plotEOF is Deprecated. Please consider using transformeR::EOF2clim in combination with visualizeR::spatialPlot")
    arg.list <- list(...)
    member <- as.integer(member)
    varNames <- attributes(prinCompObj)$names
    if (length(varNames) == 1) {
        ind.var <- 1
    } else {
        if (is.null(var)) {
            stop("The argument 'var' is missing, with no default", call. = FALSE)
        }
        ind.var <- match(var, varNames)
        if (is.na(ind.var)) {
            stop("Variable given in 'var' not found", call. = FALSE)
        }
        if (var == "COMBINED") {
            stop("It is not possible to display the combined EOF", call. = FALSE)
        }
    }
    if (length(prinCompObj[[1]]) == 1) {
        if (member != 1) {
            message("NOTE: Argument 'member' was ignored")
        }
        member <- 1
    }
    if (!(member %in% 1:length(prinCompObj[[1]]))) {
        stop("'member' value must be between 1 and the total number of members (", length(prinCompObj[[1]]), " in this case)", call. = FALSE)
    }
    tot.n.eofs <- ncol(prinCompObj[[ind.var]][[member]]$EOFs)
    if (is.null(n.eofs)) {
        n.eofs <- tot.n.eofs
    }
    if (!(n.eofs %in% 1:tot.n.eofs)) {
        stop("'n.eofs' value must be between 1 and the total number of possible EOFs (", tot.n.eofs, " in this case)", call. = FALSE)
    }
    clim <- EOF2clim(prinCompObj, ind.var, member, n.eofs) 
    arg.list[["grid"]] <- clim
    if (!"names.attr" %in% names(arg.list)) arg.list[["names.attr"]] <-  paste("EOF", 1:n.eofs, sep = "-")
    if (!"as.table" %in% names(arg.list)) arg.list[["as.table"]]  <-  TRUE
    do.call("spatialPlot", arg.list) %>% print()
}









