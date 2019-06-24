# 	cascadePlot.R
#
# 	Copyright (C) 2018 Santander Meteorology Group (http://www.meteo.unican.es)
#
# 	This program is free software: you can redistribute it and/or modify
# 	it under the terms of the GNU General Public License as published by
# 	the Free Software Foundation, either version 3 of the License, or
# 	(at your option) any later version.
#
# 	This program is distributed in the hope that it will be useful,
# 	but WITHOUT ANY WARRANTY; without even the implied warranty of
# 	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# 	GNU General Public License for more details.
#
# 	You should have received a copy of the GNU General Public License
# 	along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Cascade plot to visualize the climate change modelling uncertainty cascade.
#'
#' @description Cascade plot to decompose modelling uncertainty down to individual future climate projections.
#'
#' @param x Numeric vector with the values of the individual projections.
#' @param factors List with several factors classifying the data in x.
#' @param stages String vector with the names of the factors to be plotted 
#'   from bottom to top.
#' @param labels Logical (default = FALSE). Whether to plot individual labels 
#'   for each line. See label.names.
#' @param label.names Character vector with the names to be given to each line. 
#'   If NULL (default) names are taken from x.
#' @param colors Color vector to be used in the last aggregating step and all 
#'   lines leading to each main branch. If NULL (default) an appropriate rainbow 
#'   scale is used. Its length should be \code{length(levels(factors[[rev(stages)[1]]]))}.
#' @param title Title of the plot
#' @param line.width Width of the lines.
#' @param plot.density Logical (default = TRUE). Add an estimate of the probability
#'   density function (PDF).
#' @param density.offset A vector indicating how much the PDF should be shifted 
#'   relative to the bottom part of the plot. Default is 0.
#' @param density.reverse Logical (default = TRUE). If TRUE plot the PDF upside down.

#' @export
#' @importFrom grDevices rainbow
#' @importFrom graphics segments 

#' @family visualization functions

#' @author J. Fernández \email{jesus.fernandez@@unican.es} and M.D. Frías based on the
#' visualization proposed by Ed Hawkins on his Climate Lab Book http://www.climate-lab-book.ac.uk/2014/cascade-of-uncertainty
#'
#' @references Hawkins E. 2014. Climate Lab Book. http://www.climate-lab-book.ac.uk/2014/cascade-of-uncertainty
#' @references Fernández, J., Frías, M.D., Cabos, W.D. et al. 2018. Consistency of climate change
#' projections from multiple global and regional model intercomparison projects. Clim Dyn. https://doi.org/10.1007/s00382-018-4181-8
#'
#' @examples \dontrun{
#' # This code reproduces Figure 4 (top) from Fernandez et al. (2018)
#' load(url("http://meteo.unican.es/work/visualizeR/data/multimip_sdelta_es.rda"), verbose=T)
#' cat(multimip.README)
#' filter.members <- multimip.factors[["Method"]]=="GCM" & ! multimip.factors[["GCM"]] %in% c("CNRM-CM5-r8", "EC-EARTH-r3")
#' sdeltas <- multimip.prtas.sdeltas["tas",filter.members]
#' factors <- lapply(multimip.factors, function(x) factor(x[filter.members]))
#' title <- "Delta T (K)"
#' cascadePlot(sdeltas, factors, multimip.stages, title=title, density.offset=2)
#' } 

cascadePlot <- function(x, factors, stages = NULL, labels = FALSE, label.names = NULL, colors=NULL, title = NULL, line.width = NULL, plot.density = TRUE, density.offset=0, density.reverse=TRUE){
      if (is.null(stages)) {
        stages <- names(factors)
      }
      nstages <- length(stages)
      # The element to highlight must be the last element of stages
      highlight <- factors[[tail(stages, n=1)]]
      if (is.null(line.width)) {
        line.width <- 2
      }
      if (is.null(colors)) {
        colors <- rev(rainbow(length(levels(highlight))))
      }
      par(mar=c(13,7,4,9), xpd=TRUE)
      plot(1, type="n", xlim=c(0.5,3.2), ylim=c(0,nstages), xlab="", ylab="", yaxt="n", xaxt="n")
      axis(1, padj=-1.2, cex.axis=0.9)
      axis(2, at=0:(nstages-1), labels=stages, las=2)
      if (!is.null(title)) {
        axis(3, padj=1, cex.axis=0.9); mtext(title, side=3, line=2)
      }
      istage <- 0
      sta <- stages
      fac <- factors[sta]
      x1sum <- tapply(x, fac, sum)
      x1cnt <- tapply(x, fac, length)
      x1 <- x1sum/x1cnt
      if (labels) {
        if (!is.null(label.names)) {
          label.names <- label.names
        } else {
          label.names <- names(x)
        }
        text(x, par("usr")[3]-0.9, label.names, xpd=TRUE, adj=1, srt=45, cex=0.3, col=colors[highlight])
      }
      if (plot.density) {
        side.density(x, prob=c(0.05,0.25,0.5,0.75,0.95), shades=T, density.offset=density.offset, density.reverse=density.reverse)
      }      
      # Plot is built from bottom to top ...
      for (istage in 1:(nstages)) {
        sta <- sta[-1] # Pop out the lowermost remaining stage
        if (length(sta)!=0) {
          x1sum <- apply(x1sum,sta,sum, na.rm=T)
          x1cnt <- apply(x1cnt,sta,sum, na.rm=T)
        } else {
          x1sum <- sum(x1sum, na.rm=T)
          x1cnt <- sum(x1cnt, na.rm=T)
        }
        x0 <- x1
        x1 <- x1sum/x1cnt
        if (istage != nstages){
          nlev <- dim(x0)[1]                  # Flatten all dimensions but the 1st
          x0flat <- x0                        #
          dim(x0flat) <- c(nlev, length(x1))  #
          x0hi <- x0                      ################# dirty trick to get the colours ###
          for (lev in levels(highlight)){                                                    #
            eval(parse(text=sprintf(                                                         #
              "x0hi[%slev] <- lev", paste(rep(",",nstages-istage),sep="", collapse="")       #
            )))                                                                              #
          }                                                                                  #
          this.col <- colors[factor(c(x0hi), levels=levels(highlight))]                      #
          dim(this.col) <- dim(x0flat)                                      ##################
          #      for (ilev in 1:length(x1)) points(x1, rep(istage,length(x1)))
          for (ilev in 1:nlev) {
            segments(x0flat[ilev,], istage-1, c(x1), istage, col=this.col[ilev,], lwd=line.width)
          }
        } else segments(x0, istage-1, x1, istage, col=colors, lwd=line.width) # Tip of the pyramid
      }
      idx <- sort(tapply(x, highlight, mean),index.return=T)$ix
      n.legend.items <- length(levels(highlight))
      legend(
        par("usr")[2],par("usr")[4]+1.65,
        #"topright",
        pch=19, bty="n",
        xpd=NA,inset=c(-0.2,0),
        horiz=F,
        cex=0.75,
        legend=rev(levels(highlight)[idx]),
        col=rev(colors[(1:n.legend.items)[idx]])
      )
}
# End
