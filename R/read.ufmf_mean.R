# private function to read in one or more mean (background) frames from a ufmf
# file
ufmf_read_mean <- function(header, meani=NULL, framei=NULL, dopermute=TRUE) {
  fp = header$con
  meani = c(meani, header$frame2mean[framei]);
  if(!length(meani)) {
    im=ufmf_read_mean_helper(fp, header)
  } else {
    if(length(meani) > 1) {
      im = array(header$meandataclass, dim=c(header$ncolors,header$nr,header$nc,length(meani)))
      for(i in seq_along(meani)){
        id=meani[i]
        im[,,,i]=ufmf_read_mean_helper(fp, header, meani = id)
      }
    } else im=ufmf_read_mean_helper(fp, header, meani = meani)
  }
  if(dopermute){
    perm=c(2,3,1)
    if(length(dim(im))==4)
      perm=c(perm,4)
    im = aperm(im, perm)
  }
  im
}


# function [im,header,timestamp] =
ufmf_read_mean_helper <- function(fp, header, meani=NULL) {
  # % Keyframe file format:
  #   %
  # % 0 (chunk type)                       uchar
  # % 4 (length of keyframe type)          uchar
  # % 'mean' (keyframe type)               char x 4
  # % timestamp                            double
  # % number of boxes/points               ushort
  # % dtype ('f' for float, 'B' for uint8) char
  # % width                                ushort
  # % height                               ushort
  # % timestamp                            double
  # % background mean                      dtype x width x height x ncolors
  # % (iterate over colors,
  #    %  followed by columns in sideways im,
  #    %  followed by rows)

  KEYFRAME_CHUNK = 0
  MEAN_KEYFRAME_TYPE = 'mean'

  if(is.null(meani)){
    loc = seek(fp)
    meani = which(header$mean2file == loc)
    if(!length(meani))
      stop('Could not find current file location in mean2file index')
  } else {
    seek(fp, header$mean2file[meani])
  }
  im=ufmf_cache_fetch(header$cache, meani)
  if(!is.null(im)) return(im)

  # % chunktype: 1
  chunktype = readBin(fp, what = integer(), size=1L)
  if(chunktype != KEYFRAME_CHUNK)
    stop("Expected chunktype = ", KEYFRAME_CHUNK, " at start of keyframe.")

  # % keyframe type
  l = readBin(fp, what = integer(), size=1L, signed = F)
  keyframe_type = readChar(fp, nchars = l, useBytes = T)

  if (!identical(keyframe_type,"mean"))
    stop('Expected keyframe type = "mean" at start of mean keyframe')

  # % data type
  dtypechar = readChar(fp, nchars = 1L)
  dtl=data_type(dtypechar)

  # % images are sideways: swap width and height
  # % width, height
  sz = readBin(fp, what = integer(), n=2L, size = 2L, signed = F, endian = "little")
  height = sz[1]; width = sz[2]

  # % timestamp
  timestamp = readBin(fp, what = numeric(), n = 1L, size = 8L, endian = "little")

  # % actual frame data
  # NB matlab code has header$bytes_per_pixel here, which appears to be the same
  # for currently supported video formats, but surely need not be.
  im = readBin(fp, what=dtl$type, n=width*height*header$ncolors, size=dtl$length, signed = dtl$signed)
  # % TODO: handle colorspaces other than RGB8 and MONO8
  if (!tolower(header$coding)%in% c('mono8','rgb8'))
    stop('Colorspace ', header$coding,' not yet supported. Only MONO8 and RGB8 allowed.')

  dim(im)=c(header$ncolors,height,width)
  attr(im, 'timestamp')=timestamp

  # % store in cache
  ufmf_cache_store(header$cache, im, id=meani)
  im
}
