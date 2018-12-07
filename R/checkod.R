checkod <-
function (origins.id, destinations.id, origins.addr = NULL, destinations.addr = NULL) {
  
  origins_freq <- table(as.character(origins.id))
  
  if (any(origins_freq > 1)) {
    stop("Origins vector contains duplicate origin IDs", call. = FALSE)
  }
  
  destinations_freq <- table(as.character(destinations.id))
  
  if (any(destinations_freq > 1)) {
    stop("Destinations vector contains duplicate destination IDs", call. = FALSE)
  }
  
  if (!is.null(origins.addr)) {
    
    if (any(is.na(origins.addr))) {
      stop("Origins address vector contains NAs", call. = FALSE)
    }
    
    if (length(origins.id) != length(origins.addr)) {
      stop("Origin IDs and origin address vectors differ in length", call. = FALSE)
    }
    
  }
  
  if (!is.null(destinations.addr)) {
    
    if (any(is.na(destinations.addr))) {
      stop("Destinations address vector contains NAs", call. = FALSE)
    }
    
    if (length(destinations.id) != length(destinations.addr)) {
      stop("Destination IDs and destination address vectors differ in length", call. = FALSE)
    }
    
  }
  
}