#' Read frame from a ufmf format movie
#'
#' @details When reading multiple frames, the recommended idiom is to read the
#'   header and store it in an object, then pass that object to read.ufmf along
#'   with the integer index of the required frame. See \bold{examples}.
#'
#'   When present, bounding boxes for foreground pixels are returned in an N x 4
#'   array where the four columns specify (x, y, width , height).
#'
#' @param x Either a path to a file on disk, a \code{\link{connection}} or a
#'   parsed ufmf header object (as returned by \code{\link{read.ufmf.header}})
#' @param framei Integer index of the frame to read
#' @param return.boxes Whether to return an matrix describing the position of
#'   pixels that differ from the mean image (see details, default \code{FALSE}).
#' @return An array containing the image data with the frame timestamp as an
#'   attribute.
#' @seealso \code{\link{connection}}, \code{\link{read.ufmf.header}}
#' @export
#' @examples
#' \dontrun{
#' h=read.ufmf.header("movie.ufmf")
#' frames=lapply(1:5, function(i) read.ufmf(h, i))
#' }
#'
read.ufmf <- function(x, framei=NULL, return.boxes=FALSE){
  if(is.character(x))
    x=file(x, open='rb')

  if(inherits(x, "connection")) {
    h=read.ufmf.header(x)
  } else {
    h=x
    h$con=file(h$filename, open='rb')
  }
  on.exit(close(h$con))

  FRAME_CHUNK = 1;
  fp = h$con
  if(is.null(framei)){
    # read from the current location if no frame input
    loc = seek(fp);
    framei = which(h$frame2file==loc)
  } else {
    seek(fp, h$frame2file[framei])
  }
  # read in the chunk type: 1
  chunktype = readBin(fp, what = integer(), size=1L)
  if(chunktype != FRAME_CHUNK)
    stop("Expected chunktype = ", FRAME_CHUNK, " at start of keyframe.")

  # read in timestamp: 8
  timestamp = readBin(fp, what=numeric(), size = 8L, endian = 'little')
  if (h$version == 4) {
    # NB R can't read uint32
    npts = readBin(fp, what=integer(), size = 4L, endian = 'little')
  } else {
    npts = readBin(fp, what=integer(), size = 2L, endian = 'little', signed = F)
  }
  # sparse-matrix
  if (h$is_fixed_size){
    bb = readBin(fp, what=0L, n=npts*2, size=2, signed = F)
    bb = matrix(bb, ncol=2L)
    # read sideways
    bb = bb[,c(2,1)]
    data = readBin(fp, what=h$dataclass, n=npts*h$max_width*h$max_height,
                   size=h$bytes_per_pixel, signed = F, endian = 'little')
    # TODO: handle colorspaces other than MONO8 and RGB8
    data = array(data, c(h$ncolors,npts,h$max_height,h$max_width))
  } else {
    bb = matrix(0,npts,4);
    data = list()
    for (i in seq_len(npts)) {
      bb[i,] = readBin(fp, integer(), n=4, size=2L, signed = T, endian = 'little')
      width = bb[i,4]
      height = bb[i,3]
      # TODO: handle colorspaces other than MONO8 and RGB8
      data[[i]] = readBin(fp, what=h$dataclass,
                          n=width*height*h$ncolors,
                          size=1L, signed = F, endian = 'little')
      data[[i]] = array(data[[i]],c(h$ncolors, height, width))
    }
    # images are read sideways
    bb = bb[, c(2, 1, 4, 3)]
  }
  # convert to 1-indexed
  bb[,1:2] = bb[,1:2]+1

  # read in the mean image
  im = ufmf_read_mean(h, framei=framei, dopermute=F)
  if(!identical(h$dataclass, h$meandataclass))
    storage.mode(im)=storage.mode(h$dataclass)

  if (h$is_fixed_size) {
    # sparse image
    if (h$max_height == 1 && h$max_width == 1) {
      # FIXME when ncolors
      idx=sub2ind2(c(h$nr, h$nc), bb[, 1:2], h$ncolors)
      im[idx] = data
    } else {
      for (i in seq_len(npts)) {
        xidx=bb[i,2]:(bb[i,2]+h$max_height-1L)
        yidx=bb[i,1]:(bb[i,1]+h$max_width-1L)
        im[, xidx, yidx] = data[[i]]
      }
    }
  } else {
    # boxes
    for (i in seq_len(npts)) {
      xidx=bb[i,2]:(bb[i,2]+bb[i,4]-1L)
      yidx=bb[i,1]:(bb[i,1]+bb[i,3]-1L)
      im[, xidx, yidx] = data[[i]]
    }
  }
  im = aperm(im, c(3, 2, 1))
  attr(im, 'timestamp')=timestamp
  if(return.boxes)
    attr(im, 'boxes')=bb
  im
}

# simplified version of matlab/octave sub2ind for 2d indices + number of colours
sub2ind2 <- function (dims, indices, ncols)
{
  k = cumprod(c(1, ncols, dims[1]))
  i1=rep.int(indices[,1], rep.int(ncols, nrow(indices)))
  i2=rep.int(indices[,2], rep.int(ncols, nrow(indices)))
  ndx= 1:ncols + (i1-1)*k[2] + (i2-1)*k[3]
  ndx
}
