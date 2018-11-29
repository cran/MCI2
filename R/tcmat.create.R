tcmat.create <-
function (origins.id, origins.addr, 
                          destinations.id, destinations.addr, 
                          tc.type = "airline", tc.unit = "km", addr.format = "stradr", 
                          tc.constant = 0, show_proc = FALSE) {
  
  if (show_proc == TRUE) {
  
  cat ("\n")
  cat ("Transport costs matrix")
  cat ("\n")
  
  }
 

  if (addr.format == "stradr") {


    if (show_proc == TRUE) {
      cat ("Geocoding origins ... ")
    }
    
    i <- 0
    coords_origins <- data.frame(matrix (nrow = length(origins.id), ncol = 4))
    
    for (i in 1:length(origins.id)) {
      
      coords <- geocode_OSM(as.character(origins.addr[i]))
      
      coords_origins[i,1] <- as.character(origins.id[i])
      coords_origins[i,2] <- as.character(origins.addr[i])
      coords_origins[i,3] <- coords$coords[1]
      coords_origins[i,4] <- coords$coords[2]
    }
    
    colnames(coords_origins) <- c("origins.id", "origins.addr", "origins.x_lon", "origins.y_lat")

    if (show_proc == TRUE) {
      cat("OK", "\n")
    }
    
    

    if (show_proc == TRUE) { 
      cat ("Geocoding destinations ... ")
    }
    
    i <- 0
    coords_destinations <- data.frame(matrix (nrow = length(destinations.id), ncol = 4))
    
    for (i in 1:length(destinations.id)) {
      
      coords <- geocode_OSM(as.character(destinations.addr[i]))
      
      coords_destinations[i,1] <- as.character(destinations.id[i])
      coords_destinations[i,2] <- as.character(destinations.addr[i])
      coords_destinations[i,3] <- coords$coords[1]
      coords_destinations[i,4] <- coords$coords[2]
    }
    
    colnames(coords_destinations) <- c("destinations.id", "destinations.addr", "destinations.x_lon", "destinations.y_lat")

    
    if (show_proc == TRUE) { cat("OK", "\n") }
    
  }
  
  

  
  if (tc.type == "street") {

    
    if (show_proc == TRUE) {
      cat ("Query of travel times ... ")
    }
    
    tc_single_list <- osrmTable(src = coords_origins[1:nrow(coords_origins),c("origins.id","origins.x_lon","origins.y_lat")],
                                dst = coords_destinations[1:nrow(coords_destinations),c("destinations.id","destinations.x_lon","destinations.y_lat")])

    if (show_proc == TRUE) {
      cat("OK", "\n")
      cat("\n")
    }
    
    tc_single <- as.data.frame(tc_single_list$durations)
    
    tc_single$start_id <- coords_origins[1:nrow(coords_origins),1] 
    
    tcmat <- melt(tc_single, id=c("start_id"))
    
    colnames(tcmat) <- c("from", "to", "tc")
    
    tcmat$from_to <- paste0(tcmat$from, "-", tcmat$to)
    

    tcmat <- tcmat[,c(1,2,4,3)]

  }
  
  else {

    if (show_proc == TRUE) {
      cat ("Calculating distances ... ")
    }
    
    tcmat <- dist.mat (startpoints = coords_origins, sp_id = "origins.id", lat_start = "origins.y_lat", lon_start = "origins.x_lon",
                       endpoints = coords_destinations, ep_id = "destinations.id", lat_end = "destinations.y_lat", lon_end = "destinations.x_lon", 
                       unit = tc.unit)
    
    colnames(tcmat) <- c("from", "to", "from_to", "tc")
    
    if (show_proc == TRUE) {
      cat("OK", "\n")
      cat("\n")
    }
    

  }
  
  tcmat$tc <- tcmat$tc+tc.constant

  
  tc.mode <- list(tc.type = tc.type, tc.unit = tc.unit, tc.constant = tc.constant)
  
  invisible(list (tcmat = tcmat, coords_origins = coords_origins, coords_destinations = coords_destinations, tc.mode = tc.mode))
  
}
