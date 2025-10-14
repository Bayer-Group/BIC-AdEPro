#' Draw Symbols (User Defined) on a Plot
#'
#' @description This function draws symbols on a plot. It is similar to the builtin symbols function with the difference
#' that it plots symbols defined by the user rather than a prespecified set of symbols.
#'
#' @usage
#' my_symbols(x, y=NULL, symb, inches=1, xsize, ysize,
#' add=TRUE,
#' vadj=0.5, hadj=0.5,
#' symb.plots=FALSE,
#' xlab=deparse(substitute(x)),
#' ylab=deparse(substitute(y)), main=NULL,
#' xlim=NULL, ylim=NULL, linesfun=graphics::lines,
#'..., MoreArgs)
#'
#' @param x,y The x and y coordinates for the position of the symbols to be plotted. These can be specified in any way which is accepted by xy.coords.
#' @param symb Either a matrix, list, or function defining the symbol to be plotted. If it is a
#' matrix or list it needs to be formatted that it can be passed directly to the lines
#' function. It then defines the shape of the symbol on on a range/domain of -1 to
#' 1. If this is a function it can either return a matrix or list as above (points on the
#' range/domain of -1 to 1), or it can do the plotting itself.
#' @param inches The size of the square containing the symbol in inches (note: unlike symbols
#' this cannot be FALSE). This is ignored if xsize or ysize is specified.
#' @param xsize The width of the bounding box(s) of the symbols in the same units as the x
#' variable. Computed from ysize or inches if not specified. Can be a single
#' value or a vector.
#' @param ysize The height of the bounding box(s) of the symbols in the same units as the y
#' variable. Computed from xsize or inches if not specified. Can be a single
#' value or a vector.
#' @param add if 'add' is 'TRUE' then the symbols are added to the existing plot, otherwise a
#' new plot is created.
#' @param vadj,hadj Numbers between 0 and 1 indicating how 'x' and 'y' specify the location of the
#' symbol. The defaults center the symbol at x,y; 0 means put the bottom/left at
#' x,y; and 1 means put the top/right of the symbol at x,y.
#' @param symb.plots If symb is a function that does its own plotting, set this to TRUE, otherwise it
#' should be FALSE.
#' @param xlab,ylab,main,xlim,ylim If 'add' is 'FALSE' these are passed to the plot function when setting up the
#' plot.
#' @param linesfun The function to draw the lines if the function does not do its own drawing. The
#' default is lines but could be replaced with polygon to draw filled polygons
#' @param ... Additional arguments will be replicated to the same length as x then passed to
#' symb (if symb is a function) and/or the lines function (one value per symbol
#' drawn).
#' @param MoreArgs {A list with any additional arguments to be passed to the symb function (as is,
#' without being replicated/split).}
#'
#' @details The symb argument can be a 2 column matrix or a list with components 'x' and 'y' that defines
#' points on the interval [-1,1] that will be connected with lines to draw the symbol. If you want a
#' closed polygon then be sure to replicate the 1st point as the last point.
#' If any point contains an NA then the line will not be drawn to or from that point. This can be used
#' to create a symbol with disjoint parts that should not be connected.
#' If symb is a function then it should include a '...' argument along with any arguments to define
#' the symbol. Any unmatched arguments that end up in the '...' argument will be replicated to the
#' same length as 'x' (using the rep function) then the values will be passed one at a time to the
#' symb function. If MoreArgs is specified, the elements of it will also be passed to symb without
#' modification. The symb function can either return a matrix or list with the points that will then be
#' passed to the lines function (see above). Or the function can call the plotting functions itself (set
#' symb.plots to TRUE). High level plotting can be done (plot, hist, and other functions), or low
#' level plotting functions (lines, points, etc) can be used; in this case they should add things to a
#' plot with 'x' and 'y' limits of -1 to 1.
#' The size of the symbols can be specified by using inches in which case the symbol will be set
#' inside of squares whose sizes are inches size based on the plotting device. The size can also be
#' set using xsize and/or ysize which use the same units as the x and/or y variables. If only one is
#' specified then the box will be square. If both are specified and they do not match the aspect ratio of
#' the plot then the bounding box will not be square and the symbol will be distorted.
#'
#' @return This function is run for its side effect of plotting, it returns an invisible NULL.
#'
#' @note Since the '...' argument is passed to both lines and symb, the symb function should have a '...'
#' argument so that it will ignore any additional arguments.
#' Arguments such as 'type' can be passed through the '...' argument if you want the symbol made of
#' something other than lines.
#' Plotting coordinates and sizes are based on the size of the device at the time the function is called.
#' If you resize the device after plotting, all bets are off.
#' Currently missing values in x or y are not handled well. It is best if remove all missing values first.
#'
#' @author Greg Snow
#'
#' @keywords internal


my_symbols <- function(x, y=NULL, symb, inches=1, xsize, ysize,
                       add=TRUE,
                       vadj=0.5, hadj=0.5,
                       symb.plots=FALSE,
                       xlab=deparse(substitute(x)),
                       ylab=deparse(substitute(y)), main=NULL,
                       xlim=NULL, ylim=NULL, linesfun=graphics::lines,
                       ..., MoreArgs ) {

  if(!add){
	plot(x,y, type='n', xlab=xlab,ylab=ylab,
             xlim=xlim,ylim=ylim,main=main)
  }

  xy <- grDevices::xy.coords(x,y,recycle=TRUE)

  pin <- graphics::par('pin')
  usr <- graphics::par('usr')
  usr.x <- usr[2] - usr[1]
  usr.y <- usr[4] - usr[3]

#  tmp <- cnvrt.coords(xy,input='usr')$plt
  tmp <- list()
  tmp$x <- graphics::grconvertX(xy$x, to='npc')
  tmp$y <- graphics::grconvertY(xy$y, to='npc')

  tmp.xlen <- length(tmp$x)

  if( (length(inches) != 1) && (length(inches) != tmp.xlen) ) {
    inches <- rep(inches, length.out=tmp.xlen)
  }
  if( (length(hadj) != 1) && (length(hadj) != tmp.xlen) ) {
    hadj <- rep(hadj, length.out=tmp.xlen)
  }
  if( (length(vadj) != 1) && (length(vadj) != tmp.xlen) ) {
    vadj <- rep(vadj, length.out=tmp.xlen)
  }

  if( missing(xsize) ) {
      if( missing(ysize) ) { # use inches
          x.low  <- tmp$x -    hadj *inches/pin[1]
          x.high <- tmp$x + (1-hadj)*inches/pin[1]
          y.low  <- tmp$y -    vadj *inches/pin[2]
          y.high <- tmp$y + (1-vadj)*inches/pin[2]
      } else { # ysize only
          y.low  <- tmp$y - vadj*ysize/usr.y
          y.high <- tmp$y + (1-vadj)*ysize/usr.y
          x.low  <- tmp$x - hadj/pin[1]*pin[2]/usr.y*ysize
          x.high <- tmp$x + (1-hadj)/pin[1]*pin[2]/usr.y*ysize
      }
  } else {
      if( missing(ysize) ) { # xsize only
          x.low  <- tmp$x - hadj*xsize/usr.x
          x.high <- tmp$x + (1-hadj)*xsize/usr.x
          y.low  <- tmp$y - vadj/pin[2]*pin[1]/usr.x*xsize
          y.high <- tmp$y + (1-vadj)/pin[2]*pin[1]/usr.x*xsize
      } else {  # both xsize and ysize
          x.low  <- tmp$x - hadj*xsize/usr.x
          x.high <- tmp$x + (1-hadj)*xsize/usr.x
          y.low  <- tmp$y - vadj*ysize/usr.y
          y.high <- tmp$y + (1-vadj)*ysize/usr.y
      }
  }


#  xy.low  <- cnvrt.coords(x.low,  y.low,  'plt')$fig
#  xy.high <- cnvrt.coords(x.high, y.high, 'plt')$fig

  xy.low <- list()
  xy.low$x <- graphics::grconvertX(x.low, from='npc', to='nfc')
  xy.low$y <- graphics::grconvertY(y.low, from='npc', to='nfc')

  xy.high <- list()
  xy.high$x <- graphics::grconvertX(x.high, from='npc', to='nfc')
  xy.high$y <- graphics::grconvertY(y.high, from='npc', to='nfc')


  plotfun <- if( is.function(symb) ) {
    if(symb.plots) {
      function(xlow,xhigh,ylow,yhigh,symb, ...) {
        op <- graphics::par(c('plt','usr','xpd'))
        on.exit(graphics::par(op))
        graphics::par(xpd=TRUE)
        graphics::par(plt=c(xlow,xhigh,ylow,yhigh), new=TRUE)
        graphics::par(usr=c(-1,1,-1,1))
        symb(...)
      }
    } else {
      function(xlow,xhigh,ylow,yhigh,symb, ...) {
        op <- graphics::par(c('plt','usr','xpd'))
        on.exit(graphics::par(op))
        graphics::par(xpd=TRUE)
        graphics::par(plt=c(xlow,xhigh,ylow,yhigh))
        graphics::par(usr=c(-1,1,-1,1))
        suppressWarnings(
            linesfun( symb(...), ... )
                       )
      }
    }
  } else {
    function(xlow,xhigh,ylow,yhigh,symb, ...) {
      op <- graphics::par(c('plt','usr','xpd'))
      on.exit(graphics::par(op))
      graphics::par(xpd=TRUE)
      graphics::par(plt=c(xlow,xhigh,ylow,yhigh))
      graphics::par(usr=c(-1,1,-1,1))
      linesfun(symb, ...)
    }
  }

  funargs <- list(xlow=xy.low$x, xhigh=xy.high$x,
                        ylow=xy.low$y, yhigh=xy.high$y)
  if( length(list(...)) ) {
    funargs <- c(funargs,
                 lapply(list(...), function(x) rep(x,length.out=tmp.xlen) )
                 )
  }

  funargs$FUN <- plotfun
  if (missing(MoreArgs)) {
    funargs$MoreArgs <- list(symb=symb)
  } else {
    funargs$MoreArgs <- c(MoreArgs, list(symb=symb))
  }

  do.call(mapply, funargs)

  invisible(NULL)
}
