require(fields)

start.plot <- function(format, output) {
  if (format == "eps") {
    postscript(output, family='Palatino') }
  else {
    ## Fallback to PNG (works in most viewers)
    png(output, family='Palatino', width=650, height=650) }
  plot.new()
}

eplot1d <- function (title, subt, xlab, ylab,
                     input, output, logy, format) {
  ## read data
  pts   <- read.table(input, col.names=c('x','y','err','n'))
  pts$y <- pts$y

  ## plot
  start.plot(format, output)

  if (logy) {
    ## cannot compute log(x<0), so use min instead
    pts$y[pts$y <= 0] <- min(pts$y[pts$y > 0])
    ## compute relative error for log plots
    relerr = pts$err / pts$y
    pts$y <- log(pts$y, 10)
    lerr <- pts$y - relerr
    uerr <- pts$y + relerr
  } else {
    lerr <- pts$y - pts$err
    uerr <- pts$y + pts$err
  }

  ## define plot area
  plot.window(xlim=c(min(pts$x), max(pts$x)),
              ylim=c(min(pts$y, lerr), max(pts$y, uerr)),
              log=if (logy) '' else '')

  ## add points
  points(pts[1:2], col='blue', type='o', pch=3, lwd=0.5)

  ## add error lines
  arrows(pts$x, uerr, pts$x, lerr, code=3, length=0.02,
         col='darkgreen')

  ## add information
  axis(1)
  axis(2)

  title(main=title,
        sub=subt,
        col.main='darkblue', col.sub='darkblue',
        xlab=xlab, ylab=ylab)

  box()
  dev.off()
}


eplot2d <- function (title, subt, xlab, ylab, leglab,
                     input, output, logy, format, xylim, rows, cols) {
  ## read data
  pts <- read.table(input)
  pts = pts[1:rows,1:cols]

  ## plot
  start.plot(format, output)

  ## cannot compute log(0)
  if (logy) {
    # replace less than zero with 1
    zeros = pts <= 0
    pts[zeros] <- 1
    pts <- log(pts)
    pts[zeros] <- min(pts)
  }
  ## 2d image
  image.plot(x=seq(xylim[1],xylim[2],length=rows),
             y=seq(xylim[3],xylim[4],length=cols),
             z=t(as.matrix(pts)),
             main=title, sub=subt,
             col.main='darkblue', col.sub='darkblue',
             xlab=xlab, ylab=ylab, legend.lab=leglab,
             horizontal=T, legend.mar=4.5)


  dev.off()

}
