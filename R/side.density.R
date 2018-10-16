#' @title Add an estimate of the probability density function.
#' @description The function adds an estimate of the probability density function (PDF) 
#'   of the projections.
#' @param x Numeric vector with the values of the individual projections.
#' @param horiz Logical (default = TRUE). If TRUE the PDF is drawn horizontally.
#' @param col Color for the PDF.  
#' @param scale A vector indicating how much to scale the PDF. Default is 0.1 
#' @param alpha A vector indicating the transparency to be applied to col. 
#'   Alpha ranges from 0 (fully transparent) to 1 (opaque). Default is 0.3. 
#' @param shades Logical (default = FALSE). If TRUE add shades for the quantiles 
#'   defined in prob.
#' @param prob A vector giving the quantiles to be highlithed in the PDF. 
#'   Default is the quartiles.
#' @param density.offset A vector indicating how much the PDF should be shifted 
#'   relative to the bottom part of the plot. Default is 0.
#' @param density.reverse Logical (default = FALSE). If TRUE plot the PDF upside down.
#' @details The function is intended for internal use only. The function adds the PDF to the cascade plot.
#'  @author J. Fernandez and M.D. Frias 
#'  @keywords internal
#'  @export

side.density <- function(x, horiz=TRUE, col="black", scale=0.1, alpha=0.3, 
  shades = FALSE, prob=c(0.25,0.5,0.75), density.offset=0, density.reverse=FALSE)
{
  dens <- density(x, bw="sj")
  fcol <- col2rgb(col)/255
  fcol <- rgb(fcol[1], fcol[2], fcol[3], alpha=alpha)
  hplot <- diff(par("usr")[3:4])
  wplot <- diff(par("usr")[1:2])
  if (horiz) {
    xval <- dens$x
    yval <- par("usr")[3]+(scale*hplot*dens$y*wplot)-density.offset
    if (density.reverse){
      yval <- par("usr")[3]-(scale*hplot*dens$y*wplot)-density.offset
    }
  } else {
    xval <- par("usr")[1]+(scale*wplot*dens$y*hplot)
    yval <- dens$x
  }
  polygon(xval, yval, col=fcol, border=col,lwd=0.5)
  qs <- quantile(x, prob)
  if (shades) {
    nq <- length(qs) 
    nshade <- floor(nq/2)
    for (ishade in 1:nshade){
      if (horiz) {
        filter <- xval <= qs[nq-ishade+1] & xval >= qs[ishade]
        this.xval <- xval[filter]
        this.xval <- c(this.xval[1], this.xval, this.xval[length(this.xval)])
        this.yval <- c(par("usr")[3]-density.offset , yval[filter], par("usr")[3]-density.offset)
      } else {
        filter <- yval <= qs[nq-ishade+1] & yval >= qs[ishade]
        this.yval <- yval[filter]
        this.yval <- c(this.yval[1], this.yval, this.yval[length(this.yval)])
        this.xval <- c(par("usr")[1] , xval[filter], par("usr")[1])
      }
      polygon(this.xval, this.yval, col=fcol, border=NA,lwd=0.5)
    }
    # Plot the median if present
    if (any(grepl(0.5, prob))){
      prob <- 0.5
      qs <- quantile(x, prob)
      val <- scale*hplot
      if (density.reverse){
        val <- -val
      }
      segments(
        x0=ifelse(horiz, qs,            par("usr")[1]),
        y0=ifelse(horiz, par("usr")[3]-density.offset, qs),
        x1=ifelse(horiz, qs,            par("usr")[1]+scale*wplot),
        y1=ifelse(horiz, par("usr")[3]-density.offset+val,    qs),
        col=col, lwd=ifelse(prob==0.5, 3, 1), lty=1
      )
    }
  } else {
    horiz <- rep(horiz, length(qs))
    segments(
      x0=ifelse(horiz, qs,            par("usr")[1]),
      y0=ifelse(horiz, par("usr")[3]-density.offset, qs),
      x1=ifelse(horiz, qs,            par("usr")[1]+scale*wplot),
      y1=ifelse(horiz, par("usr")[3]-density.offset+scale*hplot,    qs),
      col=col, lwd=ifelse(prob==0.5, 3, 1), lty=1
    )
  }
}

