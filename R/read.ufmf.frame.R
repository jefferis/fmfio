#' Read frame from a ufmf format movie
#'
#' @details When reading multiple frames, the recommended idiom is to read the
#'   header and store it in an object, then pass that object to read.ufmf along
#'   with the integer index of the required frame.
#'
#' @param x Either a path to a file on disk, a \code{\link{connection}} or a
#'   parsed ufmf header object (as returned by \code{\link{read.ufmf.header}})
#' @param framei Integer index of the frame to read
#' @return A list containing the following elements \itemize{
#'
#'   \item{im} The requested video frame
#'
#'   \item{header} The header, which may have been modified
#'
#'   \item{timestamp} A timestamp (seconds in current epoch)
#'
#'   \item{bb} The location of the bounding boxes containing data significantly
#'   different from the background image.
#'
#'   \item{mu} The mean (background) image corresponding to this frame
#'
#'   }
#'
#' @seealso \code{\link{connection}}, \code{\link{read.ufmf.header}}
#' @export
#' @examples
#' \dontrun{
#' h=read.ufmf.header("movie.ufmf")
#' frames=lapply(1:5, function(i) read.ufmf(h, i))
#' }
#'
read.ufmf <- function(x, framei=NULL){
  if(is.character(x)){
    x=file(x, open='rb')
  }
  if(inherits(x, "connection"))
    h=read.ufmf.header(x)

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
    # number of points: 2
    npts = readBin(fp, what=integer(), size = 4L, endian = 'little', signed = F)
  }
  # sparse-matrix
  if (h$is_fixed_size){
    bb = readBin(fp, what=0L, n=npts*2, size=2, signed = F)
    bb = matrix(bb, ncol=2L)
    # read sideways
    bb = bb[,c(2,1)]
    data = readBin(fp, what=h$dataclass, n=npts*h$max_width*h$max_height,
                   size=h$bytes_per_pixel, signed = T, endian = 'little')
    # TODO: handle colorspaces other than MONO8 and RGB8
    data = array(data, c(h$ncolors,npts,h$max_height,h$max_width))
  } else {
    bb = matrix(0,npts,4);
    data = list()
    for (i in 1:npts) {
      bb[i,] = readBin(fp, integer(), n=4, size=2L, signed = T, endian = 'little')
      width = bb[i,4]
      height = bb[i,3]
      # TODO: handle colorspaces other than MONO8 and RGB8
      data[[i]] = readBin(fp, what=h$dataclass,
                          n=width*height*h$ncolors,
                          size=1L, signed = T, endian = 'little')
      data[[i]] = array(data[[i]],c(h$ncolors, height, width))
    }
    # images are read sideways
    bb = bb[, c(2, 1, 4, 3)]
  }
  # convert to 1-indexed
  bb[,1:2] = bb[,1:2]+1

  # read in the mean image
  r = ufmf_read_mean(h, framei=framei,dopermute=F)
  im=r$im
  h=r$h
  if(!identical(h$dataclass,h$meandataclass))
    storage.mode(im)=storage.mode(h$dataclass)

  if (h$is_fixed_size) {
    # sparse image
    if (h$max_height == 1 && h$max_width == 1) {
      # GJ TODO: Double check conversion of 2d indices matches matlab sub2ind
      idx=h$nr*bb[,2]+bb[,1]
      im[idx] = data
    } else {
      for (i in 1:npts) {
        # GJ TODO - why -1 here?
        xidx=bb[i,2]:bb[i,2]+h$max_height-1L
        yidx=bb[i,1]:bb[i,1]+h$max_width-1L
        im[, xidx, yidx] = data[,i,,]
      }
    }
  } else {
    # boxes
    for (i in 1:npts) {
      xidx=bb[i,2]:bb[i,2]+bb[i,4]-1L
      yidx=bb[i,1]:bb[i,1]+bb[i,3]-1L
      im[, xidx, yidx] = data[[i]]
    }
  }
  im = aperm(im, c(3, 2, 1))
  mu = aperm(r$im, c(3, 2, 1))
  list(im=im, header=h, timestamp=timestamp, bb=bb, mu=mu)
}
