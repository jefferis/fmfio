# Make an empty ufmf cache
# @param max Maximum number of images to cache
ufmf_cache_init <- function(max=5) {
  e=new.env()
  e$max=5
  e$ims=list()
  e$ids=integer()
  e$accesstimes=numeric()
  e
}

ufmf_cache_full <- function(e) {
  length(e$ims)==e$max
}

ufmf_cache_store <- function(e, im, id) {
  if(ufmf_cache_full(e)){
    # we need to find oldest
    i=which.min(e$accesstimes)
  } else i=length(e$ims)+1
  e$ims[[i]]=im
  e$accesstimes[i] = as.numeric(Sys.time())
  e$ids[i]=id
}

ufmf_cache_contains <- function(e, id) {
  id %in% e$ids
}

ufmf_cache_fetch <- function(e, id) {
  if(ufmf_cache_contains(e, id)){
    i=which(e$ids==id)
    e$accesstimes[i] = as.numeric(Sys.time())
    e$ims[[i]]
  } else NULL
}
