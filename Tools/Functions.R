# Function to rename columns

rename <- function(old_name, new_name, list) {
  lapply(list, function(x){ colnames(x)[grep(old_name, colnames(x))] <- new_name
  return(x)
  })
}

#####

# Fix date format
fix_date <- function(date_as_character) {
  tmp <- strsplit(date_as_character, '/')
  nas <- sapply(tmp, length) != 3
  tmp2 <- tmp[!nas]

  d1 <- sapply(tmp2, function(x) x[1])
  d2 <- sapply(tmp2, function(x) x[2])
  d3 <- sapply(tmp2, function(x) x[3])
  d3[nchar(d3) == 2] <- paste0('20', d3[nchar(d3) == 2])
  swap <- which(as.numeric(d1) > 12)
  d1_swap <- d1[swap]
  d1[swap] <- d2[swap]
  d2[swap] <- d1_swap
  res_dates <- as.Date(paste0(d1,'/', d2, '/', d3), format = '%m/%d/%Y')
  res <- as.Date(rep(NA, length(date_as_character)))
  res[!nas] <- res_dates
  return(res)
}

#####



# Calculate daily rainfall from a dataset with columns 'valid' and 'rainfall'

calculate_daily <- function(x) {
  require(tidyverse)
  options(dplyr.summarise.inform = FALSE) # added
  if (!all(c('rainfall', 'valid') %in% colnames(x))) stop('columns named (rainfall) and (valid) are needed.')
  precip <- x %>%
    filter(!is.na(rainfall)) %>%
    select(valid, rainfall) %>%
    mutate(date = with_tz(as.POSIXct(valid, tz='UCT'),
                          ""),
           date = date-dst(date)*3600,
           year = year(date),
           month = month(date),
           day = day(date),
           hour = hour(date),
           minute = minute(date)) %>%
    group_by(year, month, day, hour)

  modalminute <- precip %>%
    arrange(desc(minute), .by_group=TRUE) %>%
    mutate(maxminute = minute[which.max(rainfall)]) %>%
    {which.max(tabulate(.$maxminute))}

  prec <- precip %>%
    filter(minute <= modalminute) %>%
    summarize(hourlyprecip = max(rainfall, na.rm=TRUE)) %>%
    group_by(year, month, day) %>%
    summarize('rainfall' = sum(hourlyprecip, na.rm=TRUE))
  prec <- as.data.frame(prec)
  prec$date <- date(ISOdate(prec$year, prec$month, prec$day))
  prec[,5:4]
}

#####

# Get weather data from ASOS/AWOS network through https://mesonet.agron.iastate.edu

getWeatherASOS <- function(time_period = NA, network = "IA_ASOS", sid = NA) {
  require(jsonlite)
  time_period <- as.Date(time_period)
  if (is.na(sid) | is.na(time_period)[1]) {
    uri <- paste("https://mesonet.agron.iastate.edu/geojson/network/", network, ".geojson", sep = "")
    data <- url(uri)
    jdict <- fromJSON(data)
    return(jdict$features$properties)
  } else {
    if(time_period[1] > time_period[2]) stop('error: time period 1 must be before 2')
    service <- "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?"
    service <- paste(service, "data=all&tz=Etc/UTC&format=comma&latlon=yes&", sep = "")
    service <- paste(service, "year1=", year(time_period)[1], "&month1=", month(time_period)[1], "&day1=", mday(time_period)[1], "&", sep = "")
    service <- paste(service, "year2=", year(time_period)[2], "&month2=", month(time_period)[2], "&day2=", mday(time_period)[2], "&", sep = "")

    uri2 <- paste(service, "station=", sid, sep = '')
    data_table <- read.table(uri2, header = T, sep = ',')

    return(data_table)
  }
}

# Get weather station information from ASOS/AWOS

getStationASOS <- function(network){
  stations <- getWeatherASOS(network = network)
  if (is.null(stations[1])) stop('network not found')
  stations$lat <- NA
  stations$lon <- NA
  for (i in 1:nrow(stations)){
    tmp <- getWeatherASOS(time_period = c('2018-10-10', '2018-10-10'), network, stations$sid[i])
    stations$lat[i] <- tmp$lat[1]
    stations$lon[i] <- tmp$lon[1]
  }
  stations[,c('elevation', 'sname', 'county', 'state', 'country', 'sid', 'lat', 'lon')]
}

# Get weather data from NOAA

getWeatherNOAA <- function(time_period = c('2018-01-01', '2018-12-31'), sid = 'ZI000067983', dataid = 'GHCND') {
  sid2 <- paste0(dataid, ':', sid)
  weather <- list()
  tmp_offset <- 0
  while(T){
    tmp <- ncdc(datasetid = dataid, stationid = sid2, startdate = time_period[1], enddate = time_period[2], limit = 1000, offset = tmp_offset)$data
    tmp_offset <- tmp_offset + 1000
    weather[[length(weather) + 1]] <- tmp
    if (nrow(tmp) < 1000) break
  }
  weather <- as.data.frame(do.call(rbind, weather))
  dtypes <- grep('PRCP|TMAX|TMIN|TOBS', unique(weather$datatype), value = T)
  dweather <- data.frame(date = unique(weather$date))
  for (ty in dtypes){
    tmpw <- weather[weather$datatype == ty, c('date', 'value')]
    dweather[,ty] <- tmpw$value[match(dweather$date, tmpw$date)] / 10
  }
  return(dweather)
}

################

# Function to calculate GDD from weather data

calcGDD <- function(wdata, phenotype, envCol = 'env', intCol = c('date_plant', 'date_silking'), basetemp = 10) {
  # works with celsius degrees only
  if (!(envCol %in% colnames(wdata) & envCol %in% colnames(phenotype))) stop('envCol must be in colnames of wdata and phenotype')

  phenotype <- phenotype[order(phenotype$env),]
  phenotype$GDD <- NA
  phenotype[,intCol[1]] <- as.Date(phenotype[,intCol[1]])
  phenotype[,intCol[2]] <- as.Date(phenotype[,intCol[2]])
  # Remove missing dates and environments
  phenotype <- phenotype[!is.na(phenotype[,intCol[1]]),]
  phenotype <- phenotype[!is.na(phenotype[,intCol[2]]),]
  phenotype <- phenotype[phenotype[,envCol] %in% unique(wdata[,envCol]),]

  for (env in unique(phenotype$env)) {
    envi <- wdata[wdata$env == env, c('date', 'temp')]
    if (nrow(envi) > 0){
      # missing mean temperature is filled in with the environment's mean temp
      envi$temp[is.na(envi$temp)] <- mean(envi$temp, na.rm = T)
      envi$temp[envi$temp > 30] <- 30
      envi$temp[envi$temp < basetemp] <- basetemp
      envi$gdd <- envi$temp - basetemp
    }
    for (i in which(phenotype$env == env)){
      envrows <- match(as.Date(phenotype[i, intCol[1]]:phenotype[i, intCol[2]], '1970-1-1'), as.Date(envi$date))
      phenotype$GDD[i] <- sum(envi$gdd[envrows], na.rm = T)
    }
  }
  return(phenotype)
}

#===================================================================

search_apsimx2 <- function(tmp, keyword, return_lvls = FALSE) {
  lvls <- vector()
  for (i in 1:5) {
    sapp <- sapply(tmp$Children, function(x) length(grep(keyword, unlist(capture.output(str(x))))) > 0)
    if (length(sapp) == 0) break
    if (length(sapp) == 1) if (!sapp) break
    lvls[i] <- which(sapp)
    tmp <- tmp$Children[[lvls[i]]]
  }
  if (return_lvls) {
    return(lvls)
  } else {
    return(tmp)
  }
}

#search_apsimx <- function(tmp, keyword, return_lvls = FALSE) {
#  lvls <- vector()
#  for (i in 1:5) {
#    sapp <- sapply(tmp$Children, function(x) length(grep(keyword, unlist(capture.output(str(x))))) > 0)
#    if (length(sapp) == 0) break
#    lvls[i] <- which(sapp)
#    tmp <- tmp$Children[[lvls[i]]]
#  }
#  if (return_lvls) {
#    return(lvls)
#  } else {
#    return(tmp)
#  }
#}

#===================================================================
# New version of edit APSIM
# file=simfile; src.dir=simdir; node = 'Cultivar'; overwrite = T; verbose = F; parm = 'Name'; value = 'Custom'
# wrt.dir=NULL; soil.child="Metadata"; manager.child = NULL;  edit.tag = "-edited"; parm.path = NULL
edit_apsimx_new <- function (file, src.dir = ".", wrt.dir = NULL,
                             node = c("Clock", 'Report', 'Cultivar',
                                      "Weather", "Soil", "SurfaceOrganicMatter",
                                      "MicroClimate", "Crop", "Manager", "Other"),
                             soil.child = c("Metadata", "Water", "SoilWater",
                                            "Organic", "Physical", "Analysis",
                                            "Chemical", "InitialWater", "Sample"),
                             manager.child = NULL, parm = NULL, value = NULL, overwrite = FALSE,
                             edit.tag = "-edited", parm.path = NULL, root, verbose = TRUE)
{
  #.check_apsim_name(file)
  if (missing(wrt.dir))
    wrt.dir <- src.dir
  file.names <- dir(path = src.dir, pattern = ".apsimx$",
                    ignore.case = TRUE)
  if (length(file.names) == 0) {
    stop("There are no .apsimx files in the specified directory to edit.")
  }
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  edited.child <- "none"
  file <- match.arg(file, file.names)
  if (apsimx_filetype(file = file, src.dir = src.dir) != "json")
    stop("This function only edits JSON files")
  apsimx_json <- jsonlite::read_json(paste0(src.dir, "/",
                                            file))
  wcore <- grep("Core.Simulation", apsimx_json$Children)
  if (length(wcore) > 1) {
    if (missing(root)) {
      cat("Simulation structure: \n")
      str_list(apsimx_json)
      stop("more than one simulation found and no root node label has been specified \n select one of the children names above")
    }
    else {
      if (length(root) == 1) {
        wcore1 <- grep(as.character(root), apsimx_json$Children)
        if (length(wcore1) == 0 || length(wcore1) > 1)
          stop("no root node label found or root is not unique")
        parent.node <- apsimx_json$Children[[wcore1]]$Children
      }
      else {
        root.node.0.names <- sapply(apsimx_json$Children,
                                    function(x) x$Name)
        wcore1 <- grep(as.character(root[1]), root.node.0.names)
        root.node.0 <- apsimx_json$Children[[wcore1]]
        root.node.0.child.names <- sapply(root.node.0$Children,
                                          function(x) x$Name)
        wcore2 <- grep(as.character(root[2]), root.node.0.child.names)
        parent.node <- apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Children
      }
    }
  }else {
    parent.node <- apsimx_json$Children[[wcore]]$Children
  }

  if (node == "Clock") {
    parm.choices <- c("Start", "End")
    parm <- match.arg(parm, choices = parm.choices, several.ok = TRUE)
    wlc <- function(x) grepl("Clock", x$Name)
    wlcl <- sapply(parent.node, FUN = wlc)
    start <- grep("Start", names(parent.node[wlcl][[1]]),
                  ignore.case = TRUE, value = TRUE)
    end <- grep("End", names(parent.node[wlcl][[1]]),
                ignore.case = TRUE, value = TRUE)
    if (length(parm) == 1) {
      if (parm == "Start") {
        parent.node[wlcl][[1]][start] <- value
      }
      if (parm == "End") {
        parent.node[wlcl][[1]][end] <- value
      }
    }
    if (length(parm) == 2) {
      if (parm[1] == "Start") {
        parent.node[wlcl][[1]][start] <- value[1]
      }
      if (parm[2] == "End") {
        parent.node[wlcl][[1]][end] <- value[2]
      }
    }
    apsimx_json$Children[[1]]$Children <- parent.node
  }
  if (node == "Weather") {
    wlw <- function(x) grepl("Weather", x$Name)
    wlwl <- sapply(parent.node, FUN = wlw)
    parent.node[wlwl][[1]]$FileName <- value
  }
  wcz <- grepl("Models.Core.Zone", parent.node)
  core.zone.node <- parent.node[wcz][[1]]$Children
  if (node == "Soil") {
    wsn <- grepl("Models.Soils.Soil", core.zone.node)
    soil.node <- core.zone.node[wsn]
    soil.node0 <- soil.node[[1]]$Children
    if (soil.child == "Metadata") {
      edited.child <- soil.child
      metadata.parms <- c("RecordNumber", "ASCOrder",
                          "ASCSubOrder", "SoilType", "LocalName",
                          "Site", "NearestTown", "Region",
                          "State", "Country", "NaturalVegetation",
                          "ApsoilNumber", "Latitude", "Longitude",
                          "LocationAccuracy", "DataSource",
                          "Comments")
      if (!all(parm %in% metadata.parms))
        stop("parm name(s) might be wrong")
      for (i in seq_along(parm)) {
        soil.node[[1]][[parm[i]]] <- value[i]
      }
    }
    if (soil.child == "Water") {
      edited.child <- soil.child
      wwn <- grep("^Water", sapply(soil.node[[1]]$Children, function(x) x$Name))
      soil.water.node <- soil.node[[1]]$Children[[wwn]]
      if (soil.water.node$Name != "Water") {
        stop("Wrong node (Soil Water)")
      }
      crop.parms <- c("XF", "KL", "LL")
      if (parm %in% crop.parms) {
        for (i in 1:length(soil.water.node$Children[[1]][[parm]])) {
          soil.water.node$Children[[1]][[parm]][[i]] <- value[i]
        }
      }
      else {
        for (i in 1:length(soil.water.node[[parm]])) {
          soil.water.node[[parm]][[i]] <- value[i]
        }
      }
      soil.node[[1]]$Children[[wwn]] <- soil.water.node
    }
    if (soil.child == "Physical") {
      wpn <- grep("^Physical", sapply(soil.node[[1]]$Children, function(x) x$Name))
      if (!is.list(value)) value <- as.list(value)
      depth_length <- length(soil.node[[1]]$Children[[wpn]]$Depth)
      if (length(value) != depth_length) value <- value[1:depth_length]
      soil.node[[1]]$Children[[wpn]][[parm]] <- value
    }
    if (soil.child == "SoilWater") {
      edited.child <- soil.child
      wswn <- grep("^SoilWater", sapply(soil.node[[1]]$Children,
                                        function(x) x$Name))
      soil.soilwater.node <- soil.node[[1]]$Children[[wswn]]
      soilwat.parms <- c("SummerDate", "SummerU",
                         "SummerCona", "WinterDate", "WinterU",
                         "WinterCona", "DiffusConst", "DiffusSlope",
                         "Salb", "CN2Bare", "CNRed",
                         "CNCov", "Slope", "DischargeWidth",
                         "CatchmentArea")
      if (parm %in% soilwat.parms) {
        for (i in seq_along(parm)) {
          soil.soilwater.node[[parm[i]]] <- value[i]
        }
      }
      else {
        if (!parm %in% c("SWCON", "KLAT"))
          stop("parameter is likely incorrect")
        for (i in 1:length(soil.soilwater.node[[parm]])) {
          soil.soilwater.node[[parm]][[i]] <- value[i]
        }
      }
      soil.node[[1]]$Children[[wswn]] <- soil.soilwater.node
    }
    if (soil.child == "Nitrogen") {
      wnn <- grepl("Nitrogen", soil.node0)
      soil.nitrogen.node <- soil.node0[wnn][[1]]
      for (i in 1:length(soil.nitrogen.node[[parm]])) {
        soil.nitrogen.node[[parm]][[i]] <- value[i]
      }
      soil.node[[1]]$Children[wnn][[1]] <- soil.nitrogen.node
    }
    if (soil.child == "Organic") {
      edited.child <- "Organic"
      wsomn <- grepl("Organic", soil.node0)
      soil.om.node <- soil.node0[wsomn][[1]]
      som.parms1 <- c("RootCN", "EnrACoeff",
                      "EnrBCoeff")
      if (parm %in% som.parms1) {
        soil.om.node[[parm]] <- value
      }
      else {
        for (i in 1:length(soil.om.node[[parm]])) {
          soil.om.node[[parm]][[i]] <- value[i]
        }
      }
      soil.node[[1]]$Children[wsomn][[1]] <- soil.om.node
    }
    if (soil.child == "Analysis" || soil.child == "Chemical") {
      edited.child <- soil.child
      wan <- grepl(soil.child, soil.node0)
      soil.analysis.node <- soil.node0[wan][[1]]
      #if (parm != "PH")
      #  stop("only PH can be edited, use 'edit_apsimx_replace_soil_profile instead")
      #if (parm == "PH") {
      for (i in 1:length(soil.analysis.node[[parm]])) {
        soil.analysis.node[[parm]][[i]] <- value[i]
        #  }
      }
      soil.node[[1]]$Children[wan][[1]] <- soil.analysis.node
    }
    if (soil.child == "InitialWater") {
      edited.child <- "InitialWater"
      wiwn <- grepl("InitialWater", soil.node0)
      soil.initialwater.node <- soil.node0[wiwn][[1]]
      siw.parms <- c("PercentMethod", "FractionFull",
                     "DepthWetSoil")
      parm <- match.arg(parm, choices = siw.parms)
      soil.initialwater.node[[parm]] <- value
      soil.node[[1]]$Children[wiwn][[1]] <- soil.initialwater.node
    }
    if (soil.child == "Sample") {
      edited.child <- "Sample"
      wsn <- grepl("Sample", soil.node0)
      soil.sample.node <- soil.node0[wsn][[1]]
      for (i in 1:length(soil.sample.node[[parm]])) {
        soil.sample.node[[parm]][[i]] <- value[i]
      }
      soil.node[[1]]$Children[wsn][[1]] <- soil.sample.node
    }
    core.zone.node[wsn] <- soil.node
  }
  if (node == "SurfaceOrganicMatter") {
    wsomn <- grepl("Models.Surface.SurfaceOrganicMatter",
                   core.zone.node)
    som.node <- core.zone.node[wsomn][[1]]
    if (som.node$Name != "SurfaceOrganicMatter") {
      stop("Wrong node")
    }
    som.node[[parm]] <- value
    core.zone.node[wsomn][[1]] <- som.node
  }
  if (node == "MicroClimate") {
    wmcn <- grepl("Models.MicroClimate", core.zone.node)
    microclimate.node <- core.zone.node[wmcn][[1]]
    if (microclimate.node$Name != "MicroClimate") {
      stop("Wrong node")
    }
    microclimate.node[[parm]] <- value
    core.zone.node[wmcn][[1]] <- microclimate.node
  }
  if (node == "Crop") {
    wmmn <- grepl("Models.Manager", core.zone.node)
    manager.node <- core.zone.node[wmmn]
    wcn <- grepl("CultivarName", manager.node)
    crop.node <- manager.node[wcn][[1]]$Parameters
    for (i in 1:length(crop.node)) {
      if (crop.node[[i]]$Key == parm) {
        crop.node[[i]]$Value <- value
      }
    }
    core.zone.node[wmmn][wcn][[1]]$Parameters <- crop.node
  }
  if (node == "Other") {
    upp <- strsplit(parm.path, ".", fixed = TRUE)[[1]]
    upp.lngth <- length(upp)
    if (upp.lngth < 5)
      stop("Parameter path too short?")
    if (upp.lngth > 10)
      stop("Cannot handle this yet")
    if (apsimx_json$Name != upp[2])
      stop("Simulation root name does not match")
    wl3 <- which(upp[3] == sapply(apsimx_json$Children, function(x) x$Name))
    if (length(wl3) == 0)
      stop("Could not find parameter at level 3")
    n3 <- apsimx_json$Children[[wl3]]
    wl4 <- which(upp[4] == sapply(n3$Children, function(x) x$Name))
    if (length(wl4) == 0)
      stop("Could not find parameter at level 4")
    if (upp.lngth == 5) {
      if (upp[5] %in% names(n3$Children[[wl4]])) {
        apsimx_json$Children[[wl3]]$Children[[wl4]][[upp[5]]] <- value
      }
      else {
        wl5 <- which(upp[5] == sapply(n3$Children[[wl4]]$Children,
                                      function(x) x$Name))
        if (length(wl5) == 0)
          stop("Could not find parameter at level 5")
        apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]][[upp[5]]] <- value
      }
    }
    if (upp.lngth == 6) {
      n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
      wl5 <- which(upp[5] == sapply(n4$Children, function(x) x$Name))
      if (length(wl5) == 0)
        stop("Could not find parameter at level 5")
      if (upp[6] %in% names(n4$Children[[wl5]])) {
        apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]][[upp[6]]] <- value
      }
      else {
        if ("Parameters" %in% names(n4$Children[[wl5]])) {
          wp <- grep(upp[6], n4$Children[[wl5]]$Parameters)
          if (length(wp) == 0)
            stop("Could not find parameter at level 6 (Parameter)")
          apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Parameters[[wp]]$Value <- value
        }
        else {
          wl6 <- which(upp[6] == sapply(n4$Children[[wl5]]$Children,
                                        function(x) x$Name))
          if (length(wl6) == 0)
            stop("Could not find parameter at level 6")
          apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]][[upp[6]]] <- value
        }
      }
    }
    if (upp.lngth == 7) {
      n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
      wl5 <- grep(upp[5], n4$Children)
      n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
      wl6 <- grep(upp[6], n4$Children)
      apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]][[upp[7]]] <- value
    }
    if (upp.lngth == 8) {
      n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
      wl5 <- grep(upp[5], n4$Children)
      n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
      wl6 <- grep(upp[6], n5$Children)
      n6 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]
      wl7 <- grep(upp[7], n6$Children)
      apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]][[upp[8]]] <- value
    }
    if (upp.lngth == 9) {
      n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
      wl5 <- grep(upp[5], n4$Children)
      n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
      wl6 <- grep(upp[6], n5$Children)
      n6 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]
      wl7 <- grep(upp[7], n6$Children)
      n7 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]
      wl8 <- grep(upp[8], n7$Children)
      apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]][[upp[9]]] <- value
    }
    if (upp.lngth == 10) {
      n4 <- apsimx_json$Children[[wl3]]$Children[[wl4]]
      wl5 <- grep(upp[5], n4$Children)
      n5 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]
      wl6 <- grep(upp[6], n5$Children)
      n6 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]
      wl7 <- grep(upp[7], n6$Children)
      n7 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]
      wl8 <- grep(upp[8], n7$Children)
      n8 <- apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]]
      wl9 <- grep(upp[9], n8$Children)
      apsimx_json$Children[[wl3]]$Children[[wl4]]$Children[[wl5]]$Children[[wl6]]$Children[[wl7]]$Children[[wl8]]$Children[[wl9]][[upp[10]]] <- value
    }
  }
  if (node == 'Cultivar') {
    lvls <- search_apsimx2(apsimx_json, 'Models.PMF.Plant, Models', T)

    if(length(grep('Models.PMF.Cultivar, Models', capture.output(str(apsimx_json)), ignore.case = T)) == 0) {
      x <- list(list())
      x[[1]]$`$type` <- 'Models.PMF.Cultivar, Models'
      x[[1]]$Command <- list('[Phenology].Juvenile.Target.FixedValue = 110',
                             '[Phenology].GrainFilling.Target.FixedValue = 550')
      x[[1]]$Name <- value
      x[[1]]$Children <- list()
      x[[1]]$IncludeInDocumentation <- T
      x[[1]]$Enabled <- T
      x[[1]]$ReadOnly <- F
      apsimx_json$Children[[lvls[1]]]$Children[[lvls[2]]]$Children[[lvls[3]]]$Children <- x
      parm <- 'New edited cultivar created'
      value <- 'Example values added'
    } else {
      # Edit existing cultivar
      if (parm == 'Command') if (!is.list(value)) value <- as.list(value)
      apsimx_json$Children[[lvls[1]]]$Children[[lvls[2]]]$Children[[lvls[3]]]$Children[[1]][[parm]] <- value
    }
    node <- "Other"
    parm.path <- ""
    print.path <- F
  }
  if (node == "Manager") {
    wmmn <- grepl("Models.Manager", core.zone.node)
    manager.node <- core.zone.node[wmmn]
    manager.node.names <- sapply(manager.node, FUN = function(x) x$Name)
    if (missing(manager.child))
      stop("need to specify manager.child")
    edited.child <- manager.child
    wmc <- grep(manager.child, manager.node.names)
    if (length(wmc) == 0) {
      manager.node.names <- sapply(manager.node[[1]]$Children,
                                   FUN = function(x) x$Name)
      wmc2 <- grep(manager.child, manager.node.names)
      manager.child.node <- manager.node[[1]]$Children[[wmc2]]$Parameters
    }
    else {
      manager.child.node <- manager.node[[wmc]]$Parameters
    }
    for (i in 1:length(manager.child.node)) {
      if (manager.child.node[[i]]$Key == parm) {
        manager.child.node[[i]]$Value <- value
      }
    }
    if (length(wmc) == 0) {
      manager.node[[1]]$Children[[wmc2]]$Parameters <- manager.child.node
    }
    else {
      manager.node[[wmc]]$Parameters <- manager.child.node
    }
    core.zone.node[wmmn] <- manager.node
  }

  if (node == "Report") {
    if (!parm %in% c("VariableNames", "EventNames"))
      stop ('When node = "Report", parm must be either "VariableNames" or "EventNames".')
    if (class(value) != 'list') {
      warning('value should be a list. It will be coerced.')
      value <- as.list(value)
    }
    tmp <- search_apsimx2(apsimx_json, parm)
    lvls <- search_apsimx2(apsimx_json, parm, T)

    if (length(lvls) == 2)
      apsimx_json$Children[[lvls[1]]]$Children[[lvls[2]]][[parm]] <- value
    if (length(lvls) == 3)
      apsimx_json$Children[[lvls[1]]]$Children[[lvls[2]]]$Children[[lvls[3]]][[parm]] <- value
    if (length(lvls) == 4)
      apsimx_json$Children[[lvls[1]]]$Children[[lvls[2]]]$Children[[lvls[3]]]$Children[[lvls[4]]][[parm]] <- value
    if (length(lvls) == 5)
      apsimx_json$Children[[lvls[1]]]$Children[[lvls[2]]]$Children[[lvls[3]]]$Children[[lvls[4]]]$Children[[lvls[5]]][[parm]] <- value

    node <- "Other"
    parm.path <- ""
    print.path <- F
  }

  if (node != "Other") {
    parent.node[wcz][[1]]$Children <- core.zone.node
    if (length(wcore) > 1) {
      if (length(root) == 1) {
        apsimx_json$Children[[wcore1]]$Children <- parent.node
      }
      else {
        apsimx_json$Children[[wcore1]]$Children[[wcore2]]$Children <- parent.node
      }
    }
    else {
      apsimx_json$Children[[wcore]]$Children <- parent.node
    }
  }
  if (overwrite == FALSE) {
    wr.path <- paste0(wrt.dir, "/", tools::file_path_sans_ext(file),
                      edit.tag, ".apsimx")
  }
  else {
    wr.path <- paste0(wrt.dir, "/", file)
  }
  jsonlite::write_json(apsimx_json, path = wr.path, pretty = TRUE,
                       digits = NA, auto_unbox = TRUE, null = "null")
  if (verbose) {
    cat("Edited (node): ", node, "\n")
    cat("Edited (child): ", edited.child, "\n")
    cat("Edited parameters: ", parm, "\n")
    cat("New values: ", unlist(value), "\n")
    cat("Created: ", wr.path, "\n")
  }
}


# New inspect_apsimx

inspect_apsimx_new <- function (file = "", src.dir = ".", node = c("Clock",
                                                                   "Weather", "Soil", "SurfaceOrganicMatter", "Cultivar",
                                                                   "MicroClimate", "Crop", "Manager", "Other", "Report"),
                                soil.child = c("Metadata", "Water", "InitialWater",
                                               "Chemical", "Physical", "Analysis",
                                               "SoilWater", "InitialN", "CERESSoilTemperature",
                                               "Sample", "Nutrient", "Organic"), parm = NULL,
                                digits = 3, print.path = FALSE, root)
{
  apsimx:::.check_apsim_name(file)
  file.names <- dir(path = src.dir, pattern = ".apsimx$",
                    ignore.case = TRUE)
  if (length(file.names) == 0) {
    stop("There are no .apsimx files in the specified directory to inspect.")
  }
  node <- match.arg(node)
  soil.child <- match.arg(soil.child)
  if (soil.child %in% c("Nutrient"))
    stop("Not implemented yet")
  file <- match.arg(file, file.names)
  apsimx_json <- jsonlite::read_json(paste0(src.dir, "/",
                                            file))
  parm.path.0 <- paste0(".", apsimx_json$Name)
  fcsn <- grep("Models.Core.Simulation", apsimx_json$Children,
               fixed = TRUE)
  if (length(fcsn) > 1) {
    if (missing(root)) {
      cat("Simulation structure: \n")
      str_list(apsimx_json)
      stop("more than one simulation found and no root node label has been specified \n select one of the children names above")
    }
    else {
      if (length(root) == 1) {
        nms <- vapply(apsimx_json$Children, FUN = function(x) x$Name,
                      FUN.VALUE = "character")
        fcsn <- grep(as.character(root), nms)
        parm.path.1 <- paste0(parm.path.0, ".",
                              apsimx_json$Children[[fcsn]]$Name)
        parent.node <- apsimx_json$Children[[fcsn]]$Children
        if (length(fcsn) == 0 || length(fcsn) > 1)
          stop("no root node label found or root is not unique")
      }
      else {
        nms1 <- vapply(apsimx_json$Children, FUN = function(x) x$Name,
                       FUN.VALUE = "character")
        fcsn1 <- grep(as.character(root[1]), nms1)
        root.node.0 <- apsimx_json$Children[[fcsn1]]
        root.node.0.child.names <- vapply(root.node.0$Children,
                                          function(x) x$Name, FUN.VALUE = "character")
        fcsn2 <- grep(as.character(root[2]), root.node.0.child.names)
        parent.node <- apsimx_json$Children[[fcsn1]]$Children[[fcsn2]]$Children
        parm.path.1 <- paste0(parm.path.0, ".",
                              apsimx_json$Children[[fcsn1]]$Children[[fcsn2]])
      }
    }
  }
  else {
    parent.node <- apsimx_json$Children[[fcsn]]$Children
    parm.path.1 <- paste0(parm.path.0, ".", apsimx_json$Children[[fcsn]]$Name)
  }

  search_apsimx <- function(tmp, keyword, return_lvls = FALSE) {
    lvls <- vector()
    for (i in 1:5) {
      sapp <- sapply(tmp$Children, function(x) length(grep(keyword, unlist(capture.output(str(x))))) > 0)
      if (length(sapp) == 0) break
      lvls[i] <- which(sapp)
      tmp <- tmp$Children[[lvls[i]]]
    }
    if (return_lvls) {
      return(lvls)
    } else {
      return(tmp)
    }
  }

  if (node == "Clock") {
    wlc <- function(x) grepl("Clock", x$Name, ignore.case = TRUE)
    wlcl <- sapply(parent.node, FUN = wlc)
    clock.node <- as.list(parent.node[wlcl])[[1]]
    start.name <- grep("start", names(clock.node),
                       ignore.case = TRUE, value = TRUE)
    end.name <- grep("end", names(clock.node), ignore.case = TRUE,
                     value = TRUE)
    cat("Start:", clock.node[[start.name]], "\n")
    cat("End:", clock.node[[end.name]], "\n")
    parm.path <- paste0(parm.path.1, ".", parent.node[wlcl][[1]]$Name)
  }
  if (node == "Weather") {
    wlw <- function(x) grepl("Weather", x$Name)
    wlwl <- sapply(parent.node, FUN = wlw)
    weather.node <- parent.node[wlwl]
    gf1 <- function(x) grep(".met$", x, value = TRUE)
    cat("Met file:", as.character(sapply(weather.node,
                                         gf1)), "\n")
    parm.path <- paste0(parm.path.1, ".", parent.node[wlwl][[1]]$Name)
  }
  wcz <- grepl("Models.Core.Zone", parent.node)
  core.zone.node <- parent.node[wcz][[1]]$Children
  parm.path.2 <- paste0(parm.path.1, ".", parent.node[wcz][[1]]$Name)
  if (node == "Soil") {
    wsn <- grepl("Models.Soils.Soil", core.zone.node)
    soil.node <- core.zone.node[wsn]
    parm.path.2.1 <- paste0(parm.path.2, ".", soil.node[[1]]$Name)
    cat("Soil Type: ", soil.node[[1]]$SoilType, "\n")
    cat("Latitude: ", soil.node[[1]]$Latitude, "\n")
    cat("Longitude: ", soil.node[[1]]$Longitude, "\n")
    if (length(soil.node) != 1)
      stop("soil.node not equal to one")
    soil.children.names <- sapply(soil.node[[1]]$Children,
                                  function(x) x$Name)
    cat("Soil children:", soil.children.names, "\n")
    if (soil.child == "Metadata") {
      parm.path <- parm.path.2.1
      metadata <- NULL
      for (i in names(soil.node[[1]])) {
        if (i %in% c("Name", "Children",
                     "IncludeInDocumentation", "Enabled",
                     "ReadOnly"))
          next
        val <- as.character(ifelse(is.null(soil.node[[1]][[i]]),
                                   NA, soil.node[[1]][[i]]))
        if (!is.na(val) && nchar(val) > options()$width -
            30)
          val <- paste(strtrim(val, options()$width -
                                 30), "...")
        metadata <- rbind(metadata, data.frame(parm = i,
                                               value = val))
      }
      if (missing(parm)) {
        print(knitr::kable(metadata, longtable = FALSE))
      }
      else {
        if (!(parm %in% metadata[["parm"]]))
          stop("parm does not match a parameter in metadata")
        print(knitr::kable(metadata[metadata$parm ==
                                      parm, ]))
      }
    }
    else {
      wsc <- grep(soil.child, soil.children.names)
      if (length(wsc) == 0)
        stop("soil.child likely not present")
      selected.soil.node.child <- soil.node[[1]]$Children[wsc]
    }
    first.level.soil <- c("Water", "Physical",
                          "Chemical", "Analysis", "InitialWater",
                          "InitialN", "SoilWater", "Analysis",
                          "CERESSoilTemperature", "Organic")
    if (soil.child %in% first.level.soil) {
      parm.path <- paste0(parm.path.2.1, ".", selected.soil.node.child[[1]]$Name)
      enms <- c("IncludeInDocumentation", "Enabled",
                "ReadOnly", "Children", "Name")
      cnms <- setdiff(names(selected.soil.node.child[[1]]),
                      enms)
      soil.d1 <- NULL
      soil.d2 <- NULL
      col.nms <- NULL
      for (ii in cnms) {
        tmp <- selected.soil.node.child[[1]][ii][[1]]
        if (length(tmp) == 0)
          next
        if (length(tmp) == 1) {
          soil.d1 <- rbind(soil.d1, data.frame(parm = ii,
                                               value = as.character(tmp)))
        }
        if (length(tmp) > 1) {
          col.nms <- c(col.nms, ii)
          vals <- as.vector(unlist(tmp))
          soil.d2 <- cbind(soil.d2, vals)
        }
      }
      if (missing(parm)) {
        if (!is.null(soil.d1))
          print(knitr::kable(soil.d1, digits = digits))
        if (!is.null(soil.d2)) {
          soil.d2 <- as.data.frame(soil.d2)
          names(soil.d2) <- col.nms
          print(knitr::kable(soil.d2, digits = digits))
        }
      }
      else {
        if (!is.null(soil.d1))
          print(knitr::kable(soil.d1[soil.d1$parm ==
                                       parm, ], digits = digits))
        if (!is.null(soil.d2)) {
          soil.d2 <- as.data.frame(soil.d2)
          names(soil.d2) <- col.nms
          print(knitr::kable(soil.d2[soil.d2$parm ==
                                       parm, ], digits = digits))
        }
      }
    }
  }
  if (node == "SurfaceOrganicMatter") {
    wsomn <- grepl("Models.Surface.SurfaceOrganicMatter",
                   core.zone.node)
    som.node <- core.zone.node[wsomn][[1]]
    parm.path <- paste0(parm.path.2, ".", som.node$Name)
    som.d <- data.frame(parm = names(som.node)[2:8], value = as.vector(unlist(som.node)[2:8]))
    print(knitr::kable(som.d, digits = digits))
  }
  if (node == "MicroClimate") {
    wmcn <- grepl("Models.MicroClimate", core.zone.node)
    microclimate.node <- core.zone.node[wmcn][[1]]
    parm.path <- paste0(parm.path.2, ".", microclimate.node$Name)
    microclimate.d <- data.frame(parm = names(microclimate.node)[2:9],
                                 value = as.vector(unlist(microclimate.node)[2:9]))
    print(knitr::kable(microclimate.d, digits = digits))
  }
  if (node == "Crop") {
    wmmn <- grepl("Models.Manager", core.zone.node)
    manager.node <- core.zone.node[wmmn]
    wcn <- grepl("CultivarName", manager.node)
    crop.node <- manager.node[wcn][[1]]$Parameters
    parm.path <- paste0(parm.path.2, ".", manager.node[wcn][[1]]$Name)
    mat <- matrix(NA, nrow = length(crop.node), ncol = 2,
                  dimnames = list(NULL, c("parm", "value")))
    j <- 1
    for (i in 1:length(crop.node)) {
      mat[j, 1] <- crop.node[[i]]$Key
      mat[j, 2] <- crop.node[[i]]$Value
      j <- j + 1
    }
    print(knitr::kable(as.data.frame(mat), digits = digits))
  }
  if (node == 'Cultivar') {
    if (length(grep('Models.PMF.Cultivar, Models', capture.output(str(apsimx_json)), ignore.case = T)) > 0) {
      # There is at least one edited cultivar
      tmp <- search_apsimx2(apsimx_json, keyword = 'Models.PMF.Cultivar, Models')
      cat('Name = ', tmp$Name)
      cat("\n")
      print(knitr::kable(data.frame(Command = unlist(tmp$Command))))
      cat("\n")
    } else {
      cat("No edited cultivars. Use function edit_apsimx() to add one.\n")
    }
    parm.path <- ""
    print.path <- F
  }
  if (node == "Manager") {
    wmmn <- grepl("Models.Manager", core.zone.node)
    manager.node <- core.zone.node[wmmn]
    parm.path <- parm.path.2
    manager.node.names <- sapply(manager.node, FUN = function(x) x$Name)
    cat("Management Scripts: ", manager.node.names,
        "\n\n")
    if (!is.null(parm)) {
      parm1 <- parm[[1]]
      position <- parm[[2]]
      find.manager <- grep(parm1, manager.node.names, ignore.case = TRUE)
      selected.manager.node <- manager.node.names[find.manager]
      parm.path <- paste0(parm.path.2, ".", selected.manager.node)
      if (is.na(position)) {
        ms.params <- manager.node[[find.manager]]$Parameters
        if (length(ms.params) == 0)
          warning("parameter not found")
        mat <- matrix(NA, ncol = 2, nrow = length(ms.params),
                      dimnames = list(NULL, c("parm", "value")))
        if (length(ms.params) > 0) {
          for (j in 1:length(ms.params)) {
            mat[j, 1] <- ms.params[[j]]$Key
            mat[j, 2] <- ms.params[[j]]$Value
          }
        }
        cat("Name: ", selected.manager.node, "\n")
        print(knitr::kable(as.data.frame(mat), digits = digits))
        cat("\n")
      }
      if (!is.na(position)) {
        ms.params <- manager.node[[find.manager]]$Parameters
        if (length(ms.params) == 0)
          warning("no parameters found")
        mat <- matrix(NA, ncol = 2, nrow = length(position),
                      dimnames = list(NULL, c("parm", "value")))
        k <- 1
        for (j in 1:length(ms.params)) {
          if (j == position) {
            mat[k, 1] <- ms.params[[j]]$Key
            mat[k, 2] <- ms.params[[j]]$Value
            k <- k + 1
          }
        }
        cat("Name: ", selected.manager.node, "\n")
        parm2 <- ms.params[[position]]$Key
        cat("Key:", ms.params[[position]]$Key,
            "\n")
        print(knitr::kable(as.data.frame(mat), digits = digits))
        cat("\n")
      }
    }
  }
  if (node == "Other") {
    tmp <- core.zone.node
    parm.path.2.1 <- parm.path.2
    if (is.null(parm))
      stop("'parm' should be provided when node = 'Other'")
    if (length(parm) == 1L)
      stop("'parm' should be a list of length 2 or more")
    for (i in 1:(length(parm) - 1)) {
      nms <- sapply(tmp, function(x) x$Name)
      wcp <- grep(parm[[i]], nms)
      if (length(wcp) == 0) {
        cat("Names: ", nms, "\n")
        cat("parm[[i]]", parm[[i]], "\n")
        stop("Parameter not found")
      }
      tmp <- tmp[[wcp]]
      if (!is.null(tmp$Children))
        tmp <- tmp$Children
      parm.path.2.1 <- paste0(parm.path.2.1, ".",
                              nms[wcp])
    }
    if (!is.null(tmp$Parameters)) {
      wp <- grep(parm[[length(parm)]], tmp$Parameters)
      tmp2 <- tmp$Parameters[[wp]]
      parm.path <- paste0(parm.path.2.1, ".", tmp2$Key)
      print(knitr::kable(as.data.frame(tmp2)))
    }
    else {
      parm.path <- parm.path.2.1
      unpack_node(tmp)
    }
  }
  if (node == "Report") {
    tmp <- search_apsimx2(apsimx_json, keyword = 'VariableNames')
    print(knitr::kable(data.frame(VariableNames = unlist(tmp$VariableNames))))
    cat("\n")
    print(knitr::kable(data.frame(EventNames = unlist(tmp$EventNames))))
    parm.path <- ""
    print.path <- F
  }
  if (print.path && node != "Other") {
    if (!missing(parm)) {
      if (length(parm) == 1) {
        parm.path <- paste0(parm.path, ".", parm)
      }
      else {
        if (!is.na(position)) {
          parm.path <- paste0(parm.path, ".", parm2)
        }
      }
    }
    cat("Parm path:", parm.path, "\n")
  }
  else {
    if (print.path)
      cat("Parm path:", parm.path, "\n")
  }
  invisible(parm.path)
}

# ========================
# Plot Yield predictions with conditional probabilities
# Function to plot
plot_conditional <- function(x, y, quantiles = c(.2, .5, .8), xpos = .05, ypos = .05, cex.text=0.7, ...) {
  index <- !is.na(x) & !is.na(y)
  x <- x[index]
  y <- y[index]
  xq <- quantile(x, probs = quantiles)
  yq <- quantile(y, probs = quantiles)
  rho <- sprintf('%.3f', as.numeric(cor(x, y)))
  plot(x, y, col = 'grey', ...)
  abline(0,1, col = 2)
  text(min(x), max(y) - 0.05*diff(range(y)), labels=bquote(rho * ' = ' * .(rho)), col = "green4", pos = 4)

  for(i in 1:length(xq)) {
    abline(h = yq[i], col = 4, lty = 3)
    abline(v = xq[i], col = 4, lty = 3)
    #
    chr_x_space <- diff(par("usr")[1:2]) * xpos
    chr_y_space <- diff(par("usr")[3:4]) * ypos
    tmp_x_pos <- par("usr")[1] + nchar(names(yq)[i]) * chr_x_space
    tmp_y_pos <- par("usr")[3] + chr_y_space
    #
    #rect(xleft = par("usr")[1] + chr_x_space * .3, xright = tmp_x_pos,
    #     ybottom = yq[i] - chr_y_space, yq[i] + chr_y_space, col = 'white', border = NA)
    text(tmp_x_pos, yq[i], labels = names(yq)[i], col = 4, pos = 2)
    #
    #rect(xleft = xq[i] - chr_x_space, xright = xq[i] + chr_x_space,
    #     ybottom = tmp_y_pos - chr_y_space, tmp_y_pos + chr_y_space, col = 'white', border = NA)
    text(xq[i], tmp_y_pos, labels = names(xq)[i], col = 4)
  }
  x_cut <- cut(x, c(min(x)-1, xq, max(x)))
  y_cut <- cut(y, c(min(y)-1, yq, max(y)))
  for (i in 1:length(levels(x_cut))) {
    for (j in 1:length(levels(y_cut))) {
      tmp <- x_cut == levels(x_cut)[i] & y_cut == levels(y_cut)[j]
      tmp_x_lim <- as.numeric(gsub('\\[|\\]|[()]','', unlist(strsplit(levels(x_cut)[i], ','))))
      tmp_y_lim <- as.numeric(gsub('\\[|\\]|[()]','', unlist(strsplit(levels(y_cut)[j], ','))))
      text(mean(tmp_x_lim), mean(tmp_y_lim), round(sum(tmp) / table(x_cut)[i], 3),cex=cex.text)
    }
  }
}

#--------------
make_heatmap <- function(ec, W, layer=NULL, cluster_rows=TRUE, cluster_cols=TRUE, ...)
{
  phase <- unlist(lapply(strsplit(colnames(W),"_"),function(x)x[1])) # Phases
  Phases <- unique(phase)

  ec0 <- unlist(lapply(strsplit(colnames(W),"_"),function(x)x[2])) # Phases
  namesEC <- unlist(lapply(strsplit(ec0,"\\."),function(x)x[length(x)]))
  namesEC <- gsub("Evaporation","Evap",namesEC)
  namesEC <- gsub("Potential","Pot",namesEC)

  ec <- grep(paste(ec,collapse="|"),namesEC,value=T)
  drop <- grep("FlowNO3|PAWmm",ec)
  if(length(drop)>0) ec <- ec[-drop]

  if(!is.null(layer)){
    tmp <- (1:10)[!(1:10) %in% layer]
    drop <- grep(paste(paste0("(",tmp,")"),collapse="|"),ec)
    if(length(drop)>0) ec <- ec[-drop]
  }

  index <- which(namesEC %in% ec)

  namesPhases <- paste0("P",1:length(Phases)); names(namesPhases) <- Phases
  ec2 <- paste0(namesPhases[phase][index],"-",namesEC[index])

  W0 <- W[,index]
  colnames(W0) <- ec2

  annot <- data.frame(Phase=factor(namesPhases[phase][index],levels=namesPhases))
  rownames(annot) <- colnames(W0)

  pheatmap(cor(W0),cluster_rows = cluster_rows, cluster_cols = cluster_cols,
            show_rownames = T, show_colnames = T,
            annotation_col=annot, annotation_row=annot, ...)
}
