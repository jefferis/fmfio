#' Read header for a ufmf (compressed FlyMovieFormat) movie
#'
#' @param x Path to ufmf file
#' @return a list containing fields describing the location of frames in the
#' movie file.
#' @export
read.ufmf.header <- function(x) {
  h=list()
  h$con <- file(x, open = "rb")
  on.exit(close(h$con))
  magic=readChar(h$con, nchars = 4, useBytes = T)
  if(!identical(magic, 'ufmf'))
    stop("This is not a ufmf file!")

  h$version=readBin(h$con, what=integer(), n=1L, size = 4L)
  if(!isTRUE(h$version%in%2:4))
    stop("ufmf version must be between 2 and 4!")

  h$indexloc=readBin(h$con,what=integer(), n=1L, size=8L)
  h$max_imdims=readBin(h$con, what=integer(), n=2L, size=2L)

  if(h$version >= 4)  h$is_fixed_size = readBin(h$con, what=1L, size=1L)
  else h$is_fixed_size = FALSE

  l = readBin(h$con,what = 1L, size = 1L);
  h$coding = readChar(h$con, nchars = l)
  if(identical(tolower(h$coding), "mono8")){
    h$ncolors = 1L
    h$bytes_per_pixel = 1L
  } else if(identical(tolower(h$coding), "rgb24")) {
    h$ncolors = 3L
    h$bytes_per_pixel = 3L
  } else {
    stop("Unknown encoding!")
  }
  h$dataclass = integer()

  seek(h$con, h$indexloc)
  index=read_dict(h$con)

  # frame number to loc
  h$frame2file = index$frame$loc
  h$nframes = length(h$frame2file)
  h$timestamps = index$frame$timestamp

  # mean number to loc
  h$mean2file = index$keyframe$mean$loc
  h$nmeans = length(h$mean2file);
  h$mean_timestamps = index$keyframe$mean$timestamp

  # frame number to mean loc
  h$frame2mean = findInterval(h$timestamps, h$mean_timestamps)
  h$frame2meanloc = h$mean2file[h$frame2mean]

  #   # get the frame size: read in the first mean image
  #   [mean1,header] = ufmf_read_mean(header,'meani',1);
  #   [header.nr,header.nc,~] = size(mean1);
  #   header.meandataclass = class(mean1);
  #
  #   % cache some means
  #   % allocate cache
  #   nmeanscached = min(MAXNMEANSCACHED,header.nmeans);
  #   header.cachedmeans = zeros([header.ncolors,header.nr,header.nc,nmeanscached],header.dataclass);
  #   header.cachedmeans_idx = zeros(1,nmeanscached);
  #   header.cachedmeans_accesstime = -inf(1,nmeanscached);
  #   % read in the means; this automatically stores them in the cache
  #   for i = 1:nmeanscached,
  #   [~,header] = ufmf_read_mean(header,'meani',i,'dopermute',false);
  #   end

  h
}

# Private function to read in a ufmf dictionary
#
read_dict <- function(con, chunktype=NULL){
  DICT_START_CHAR = 'd'
  ARRAY_START_CHAR = 'a'
  if(is.null(chunktype))
    chunktype = readChar(con, 1L, useBytes = T)
  if(!identical(chunktype,DICT_START_CHAR))
    stop('Error reading index: dictionary does not start with ',DICT_START_CHAR,'!')
  nkeys = readBin(con, what=integer(), n = 1L, size = 1L)
  index=list()
  for(j in 1:nkeys){
    l = readBin(con, what = integer(), size = 2L, signed = F, endian = "little")
    key = readChar(con, l, useBytes = T)
    # read the next letter to tell if it is an array or another dictionary
    chunktype = readChar(con, 1L, useBytes = T)
    if(identical(chunktype, DICT_START_CHAR)){
      index[[key]] = read_dict(con, chunktype = chunktype)
    } else if(chunktype == ARRAY_START_CHAR) {
      # read in the data type
      dtypechar = readChar(con, nchars = 1, useBytes = T)
      dtl=data_type(dtypechar)

      # read in number of bytes
      l = readBin(con, what = integer(), n = 1L, size = 4L, endian = "little")
      n = l / dtl$length
      if (n != round(n))
        stop('Length in bytes ',l,'is not divisible by bytes per element ',
             dtl$length)
      # read in the index array
      index[[key]] = readBin(con, n, what = dtl$type, size=dtl$length,
                             signed = dtl$signed, endian = "little")

    } else {
      stop('Error reading dictionary: ', key)
    }
  }
  index
}

# Private function to convert a character specifier into R data_type with
# signed and element size information
data_type <- function(x) {
  lx=tolower(x)
  int_lengths=c(1,2,4,4,8)
  names(int_lengths)=c("b", "h", "i", "l", "q")
  ds=T
  if(x %in% c('c','s','p')) {
    dt=character()
    dl=1L
  } else if(lx %in% names(int_lengths)) {
    dt=integer()
    dl=int_lengths[lx]
    # signed is lower case
    ds = lx==x
  } else if(lx %in% c("f", "d")) {
    dl=switch(x, f=4L, d=8L)
    dt=numeric()
  } else {
    stop("Bad data type: ", x)
  }
  list(type=dt, length=dl, signed=ds)
}
