#' Read FlyMovieFormat (fmf) files
#'
#' @description  \code{read.fmf.frame} reads a single frame from an fmf file
#'   using the header information to seek to the appropriate file location.
#' @param x Path to fmf file (or for \code{read.fmf.frame} an open connection).
#' @param i Integer (1-indexed) indicating the frame to read
#' @param h Optional header information in the form returned by
#'   \code{read.fmf.header}
#' @param what Character vector indicating whether to return the frame as a raw
#'   or integer matrix
#' @export
#' @name read.fmf
#' @aliases read.fmf.frame
read.fmf.frame<-function(x, i, h=NULL, what=c("raw","integer")) {
  if(!inherits(x,'connection')) {
    x=file(x, open='rb')
    on.exit(close(x))
  }
  if(is.null(h))
    h=read.fmf.header(summary.connection(x)$description)
  if(i>h$n_frames)
    stop("fmf file only has: ", h$n_frames,"!")
  what=match.arg(what)
  seek(x, h$data_offset+(i-1L)*h$chunksize)
  ts=readBin(x, what='double', size=8L, endian = 'little')
  if(h$bpp!=8L)
    stop("I can only read 8 bit data at the moment")
  data=readBin(x, what=what, n=h$chunksize-8L, size=1L, endian = 'little', signed=F)
  m=matrix(data,ncol=h$framesize[2], byrow = T)
  attr(m,'timestamp')=ts
  m
}


#' @description \code{read.fmf.header} reads the initial header information of
#'   an fmf file including the number of frames and the frame size.
#'
#' @references see \url{http://code.astraw.com/projects/motmot/fmf-format-spec.html}
#' @rdname read.fmf
read.fmf.header<-function(x) {
  f=file(x, 'rb')
  on.exit(close(f))
  h=list()
  h$version=readBin(f, what = 'int', size = 4, endian = 'little')
  if(!h$version%in%c(1L,3L))
    stop("Unrecognised fmf version!")
  if(h$version==3L) {
    h$lenformat=readBin(f, what = 'int', size = 4, endian = 'little')
    h$format=readChar(f, nchars = h$lenformat)
    h$bpp=readBin(f,what = 'int', size = 4, endian = 'little')
    h$framesize=readBin(f, what = 'int', size = 4, endian = 'little', n=2)
    h$chunksize=readBin(f, what = 'int', size = 8, endian = 'little')
    h$n_frames=readBin(f, what = 'int', size = 8, endian = 'little')
    h$data_offset=seek(f)
  }
  h
}


#' Read the timestamp information from all frames in an fmf file
#'
#' @param x Path to fmf file
#' @return Numeric vector of timestamps (seconds in current epoch)
#' @export
read.fmf.timestamps<-function(x) {
  h=read.fmf.header(x)
  f=file(x, open='rb')
  on.exit(close(f))
  timestamps=double(h$n_frames)
  seek(f, where=h$data_offset)
  for(i in seq_len(h$n_frames)) {
    # nb -8 because readBin moves the file pointer by 8 bytes when reading
    # the timestamp information
    if(i!=1)
      seek(f, where=h$chunksize-8L, origin="current")
    timestamps[i]=readBin(f, what='double', size=8L, endian = 'little')
  }
  timestamps
}
