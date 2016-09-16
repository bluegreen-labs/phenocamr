# phenocamr shiny interface server back end
# this is the dynamic version which switches to
# a local mode for debugging on my own systems

# load required libraries
require(shiny, quietly = TRUE)
require(shinydashboard, quietly = TRUE)
require(leaflet, quietly = TRUE)
require(plotly, quietly = TRUE)
require(DT, quietly = TRUE)
require(RCurl,quietly = TRUE)
require(jsonlite)

# grab the OS info
OS = Sys.info()[1]
machine = Sys.info()[4]

# When on the machine of the developer, sideload the code locally
# for quick reviewing of changes to the GUI
if (machine == "squeeze" | machine == "Pandora.local") {
  # load all functions
  files = list.files(
    "/data/Dropbox/Research_Projects/code_repository/bitbucket/phenocamr/R/",
    "*.r",
    full.names = TRUE
  )
  for (i in files) {
    source(i)
  }
  
  # set data path
  path = "./data"
  
} else{
  # create temporary directory and move into it
  if (!dir.exists("~/phenocam_cache")) {
    dir.create("~/phenocam_cache")
  }
  
  # set the base directory to the cache directory
  # this prevents any temporary files interfering with
  # local data
  setwd("~/phenocam_cache")
  
  # location of the data
  path = "~/phenocam_cache"
  
  source('download.phenocam.r')
  source('detect.outliers.r')
  source('optimal.span.r')
}

# set constants pertaining to the transition date
# thresholds (% of amplitude), these will be used
# in the calculation of the transtion dates (dynamically)
lower.thresh = 0.1
middle.thresh = 0.25
upper.thresh = 0.5

# grab the latest roi list using jsonlite
# this should work across all platforms regardless
df = fromJSON("https://phenocam.sr.unh.edu/webcam/roi/roilistinfo/")

df = df[,c("site",
           "veg_type",
           "roi_id_number",
           "first_date",
           "last_date",
           "site_years",
           "lat","lon",
           "description",
           "missing_data_pct")]

# download metadata, and select useful columns for the explorer
metadata = fromJSON("https://phenocam.sr.unh.edu/webcam/network/siteinfo_combined/")

# use daymet in case of missing site specific MAT / MAP
metadata$MAT = ifelse(is.na(metadata$MAT_site),metadata$MAT_gridded,metadata$MAT_site)
metadata$MAP = ifelse(is.na(metadata$MAP_site),metadata$MAP_gridded,metadata$MAP_site)

# what variables do I retain
metadata = metadata[,c("site",
                       "ecoregion",
                       "koeppen_geiger",
                       "landcover_igbp",
                       "elev",
                       "MAT",
                       "MAP")]

# merge the roi list with the short metadata based on sitename
df = merge(df,metadata,by="site")

# introduce jitter on lat long coordinates
# this avoids that site locations with different PFTs
# will overlap, although not geographically accurate
# it helps visualize things
df$lat_j = df$lat + rnorm(length(df$lat)) * 0.00005
df$lon_j = df$lon + rnorm(length(df$lat)) * 0.00005

# subset data to exclude certain PFT / ROI classes which are irrelevant
# (no vegetation, bad ROI, mixed data types)
df = df[-which(
  df$veg_type == "XX" | df$veg_type == "MX" |
  df$veg_type == "UN" | df$veg_type == "NV" | 
  df$veg_type == "RF" ),]
row.names(df) = 1:dim(df)[1]

# create a character field with html to call as marker
# popup. This includes a thumbnail of the site!
df$preview = unlist(lapply(df$site, function(x)
  paste(
    "<table width=150px, border=0px>",
    "<tr>",
    "<td><b>",
    x,
    "</b></td>",
    "</tr>",
    "<tr>",
    "<td>",
    "<img src=https://phenocam.sr.unh.edu/data/latest/thumbs/",
    x,
    ".thumb.jpg>",
    "</td>",
    "</tr>",
    "</table>",
    sep = ""
  )))

# find the column locations of certain variables (for later data table manipulations)
colloc = c(which(colnames(df) == "lat_j"), which(colnames(df) == "lon_j"))

# load MODIS land cover legend
mylegend = read.csv("MODIS.LC.legend.csv",sep = ",")

# start server routine
server = function(input, output, session) {
  # Reactive expression for the data subsetted
  # to what the user selected
  v1 = reactiveValues()
  v2 = reactiveValues()
  reset = reactiveValues()
  
  # the list holding the the filenames and sizes of
  # the icons in the overview map
  vegIcons = iconList(
    WL = makeIcon("wetland_o.png", "wetland_o.png", 18, 18),
    DB = makeIcon("broadleaf_o.png", "broadleaf_o.png", 18, 18),
    EN = makeIcon("conifer_o.png", "conifer_o.png", 18, 18),
    DN = makeIcon("conifer_o.png", "conifer_o.png", 18, 18),
    EB = makeIcon("tropical_o.png", "tropical_o.png", 18, 18),
    GR = makeIcon("grass_o.png", "grass_o.png", 18, 18),
    SH = makeIcon("grass_o.png", "grass_o.png", 18, 18),
    TN = makeIcon("grass_o.png", "grass_o.png", 18, 18),
    AG = makeIcon("agriculture_o.png", "agriculture_o.png", 18, 18)
  )
  
  # function to subset the site list based upon coordinate locations
  filteredData = function() {
    if (!is.null(isolate(v2$lat))) {
      if (input$colors == "ALL") {
        tmp = df[which(
          df$lat < isolate(v1$lat) &
            df$lat > isolate(v2$lat) &
            df$lon > isolate(v1$lon) & df$lon < isolate(v2$lon)
        ),]
        unique(tmp)
      } else{
        df[which(
          df$lat < isolate(v1$lat) &
            df$lat > isolate(v2$lat) &
            df$lon > isolate(v1$lon) &
            df$lon < isolate(v2$lon) & df$veg_type == input$colors
        ),]
      }
    } else{
      if (input$colors == "ALL") {
        unique(df)
      } else{
        df[df$veg_type == input$colors,]
      }
    }
  }
  
  getValueData = function(table) {
     
    # calculate the total number of site years
    # and sites in the dataset
    total_site_years = sum(table$site_years)
    total_sites = length(unique(table$site))
    
    output$site_count = renderInfoBox({
    valueBox(total_sites,
               "Sites",
               icon = icon("list"),
               color = "blue")
    })
    
    output$year_count = renderInfoBox({
    valueBox(total_site_years,
               "Site Years",
               icon = icon("list"),
               color = "blue")
    })
  }
  
  # fill site count etc fields
  getValueData(df)
  
  # create function to plot the climatology
  # occurs too many times to repeat the code
  updateClimatology = function(){
    output$climate = renderPlot({
      par(mar = c(4, 4, 1, 1))
      plot(
        1,
        1,
        type = 'n',
        xaxt = 'n',
        yaxt = 'n',
        xlab = '',
        ylab = '',
        bty = 'n',
        xlim = c(0, 100),
        ylim = c(0, 100)
      )
      plot(MAP~MAT,data=filteredData(),
           xlab=expression("MAT ("*degree*"C)"),
           ylab="MAP (mm)",
           pch=19,
           col=rgb(0.5,0.5,0.5,0.3),
           xlim=c(-15,30),
           ylim=c(0,3000)
      )
    }, height = function() {
      session$clientData$output_climate_height
    })
  }
  
  # Use leaflet() here, and only include aspects of the map that
  # won't need to change dynamically (at least, not unless the
  # entire map is being torn down and recreated).
  output$map = renderLeaflet({
    map = leaflet(df) %>%
      addTiles(
        "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg",
        attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community',
        group = "World Imagery"
      ) %>%
      addWMSTiles(
        "http://webmap.ornl.gov/ogcbroker/wms?",
        layers = "10004_31",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        attribution = "MODIS Land Cover (MCD12Q1) &copy NASA",
        group = "MODIS Land Cover"
      ) %>%
      addProviderTiles(
        "OpenTopoMap",
        group = "Open Topo Map"
        ) %>%
      addMarkers(
        lat = ~ lat_j,
        lng = ~ lon_j,
        icon = ~ vegIcons[veg_type],
        popup =  ~ preview
      ) %>%
      # Layers control
      addLayersControl(
        baseGroups = c("World Imagery","MODIS Land Cover","Open Topo Map"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      setView(lng = 11,
              lat = 45,
              zoom = 2)
  })
  
  # Incremental changes to the map. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      addMarkers(
        lat = ~ lat_j,
        lng = ~ lon_j,
        icon = ~ vegIcons[veg_type],
        popup =  ~ preview
      )
    
    # update the data table in the explorer
    output$table = DT::renderDataTable({
      tmp = filteredData()#[,-DT_drop]
      tmp = tmp[,-c((ncol(tmp)-2):ncol(tmp))]
      return(tmp)
    },
    selection = "single",
    options = list(lengthMenu = list(c(5, 10), c('5', '10')),
                   pom = list('site_years')),
    extensions = c('Responsive'))
    
    # update value box
    getValueData(filteredData())
    
    # update climatology plot
    updateClimatology()
  })
  
  # grab the bounding box, by clicking the map
  observeEvent(input$map_click, {
    # if clicked once reset the bounding box
    # and show all data
    if (!is.null(isolate(v2$lat))) {
      # set bounding box values to NULL
      v1$lat = NULL
      v2$lat = NULL
      v1$lon = NULL
      v2$lon = NULL
      
      leafletProxy("map", data = filteredData()) %>%
        clearMarkers() %>%
        clearShapes() %>%
        addMarkers(
          lat = ~ lat_j,
          lng = ~ lon_j,
          icon = ~ vegIcons[veg_type],
          popup =  ~ preview
        )
      
      getValueData(filteredData())
      
      # update climatology plot
      updateClimatology()
      
    } else{
      # grab bounding box coordinates
      # TODO: validate the topleft / bottom right order
      if (!is.null(isolate(v1$lat))) {
        v2$lat = input$map_click$lat
        v2$lon = input$map_click$lng
      } else{
        v1$lat = input$map_click$lat
        v1$lon = input$map_click$lng
        leafletProxy("map", data = filteredData()) %>%
          clearMarkers() %>%
          addMarkers(
            lat = ~ lat_j,
            lng = ~ lon_j,
            icon = ~ vegIcons[veg_type],
            popup =  ~ preview
          ) %>%
          addCircleMarkers(
            lng = isolate(v1$lon),
            lat = isolate(v1$lat),
            color = "red",
            radius = 3,
            fillOpacity = 1,
            stroke = FALSE
          )
      }
    }
    
    # if the bottom right does exist
    if (!is.null(isolate(v2$lat))) {
      
      # subset data based upon topleft / bottomright
      # first put all data in tmp data table
      tmp = filteredData()
      
      # check if the dataset is not empty
      if (dim(tmp)[1] != 0) {
        # update the map
        leafletProxy("map", data = tmp) %>%
          clearMarkers() %>%
          addMarkers(
            lat = ~ lat_j,
            lng = ~ lon_j,
            icon = ~ vegIcons[veg_type],
            popup =  ~ preview
          ) %>%
          addRectangles(
            lng1 = isolate(v1$lon),
            lat1 = isolate(v1$lat),
            lng2 = isolate(v2$lon),
            lat2 = isolate(v2$lat),
            fillColor = "transparent",
            color = "red"
          )
        
        # update the data table in the explorer
        output$table = DT::renderDataTable({
          tmp = filteredData()#[,-DT_drop]
          tmp = tmp[,-c((ncol(tmp)-2):ncol(tmp))]
          return(tmp)
        },
        selection = "single",
        options = list(lengthMenu = list(c(5, 10), c('5', '10')),
                       pom = list('site_years')),
        extensions = c('Responsive'))
        
        # update the value box
        getValueData(filteredData())
        
        # update climatology plot
        updateClimatology()
        
      } else{
        # set bounding box values to NULL
        v1$lat = NULL
        v2$lat = NULL
        v1$lon = NULL
        v2$lon = NULL
        
        leafletProxy("map", data = filteredData()) %>%
          clearMarkers() %>%
          clearShapes() %>%
          addMarkers(
            lat = ~ lat_j,
            lng = ~ lon_j,
            icon = ~ vegIcons[veg_type],
            popup =  ~ preview
          )
        
        # then stuff it back into the shiny output
        # object
        output$table = DT::renderDataTable({
          tmp = filteredData()
          tmp = tmp[,-c((ncol(tmp)-2):ncol(tmp))]
          return(tmp)
        },
        selection = "single",
        options = list(lengthMenu = list(c(5, 10), c('5', '10')),
                       pom = list('site_years')),
        extensions = c('Responsive'))
        
        # update climatology plot
        updateClimatology()
      }
    }
  })
  
  downloadData = function(myrow, frequency, percentile) {
    # if nothing selected return NULL
    if (length(myrow) == 0) {
      return(NULL)
    }
    
    # Create a Progress object
    # Make sure it closes when we exit this reactive, even if there's an error
    progress = shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making plot", value = 0)
    
    # grab the necessary parameters to download the site data
    site = as.character(df$site[as.numeric(myrow)])
    veg_type = as.character(df$veg_type[as.numeric(myrow)])
    roi_id = as.numeric(df$roi_id_number[as.numeric(myrow)])
    
    # formulate the filename of the data we need to download
    data_file = sprintf("%s/%s_%s_%04d_%sday_v4.csv",
                        path,
                        site,
                        veg_type,
                        roi_id,
                        frequency)
    
    # check if the data is on file, if so assume it is
    # checked for outliers and has smoothed data
    if (file.exists(data_file)) {
      data = read.table(data_file, header = TRUE, sep = ',')
    } else{
      # kick start progress bar
      progress$set(value = 0.2, detail = "Downloading PhenoCam data")
      
      # download phenocam data from the server
      # do not detect outliers and smooth in this step
      # do this separately to allow for proper progresss
      # bar updating (keep people busy as this takes a while)
      download.phenocam(
        site = site,
        vegetation = veg_type,
        roi_id = roi_id,
        frequency = frequency,
        outlier_detection = FALSE,
        smooth = FALSE,
        out_dir = path
      )
      
      # detect outliers
      progress$set(value = 0.3, detail = "detecting outliers")
      status = try(detect.outliers(data_file), silent = TRUE)
      if (inherits(status, "try-error")) {
        return(NULL)
      }
      
      # smooth data
      progress$set(value = 0.6, detail = "smoothing data")
      status = try(smooth.ts(data_file), silent = TRUE)
      if (inherits(status, "try-error")) {
        return(NULL)
      }
      
      # read the data to plot_data
      data = read.table(data_file, header = TRUE, sep = ',')
    }
    
    # grab the transition dates
    # trap errors, mainly if no dates can be detected return an empty
    # string (NAs) to prevent plotting errors further down.
    # code this up in the transition.dates() function TODO TODO TODO
    spring = transition.dates(data,
                              percentile = percentile,
                              lower.thresh = lower.thresh,
                              middle.thresh = middle.thresh,
                              upper.thresh = upper.thresh,
                              reverse = FALSE)
    
    fall = transition.dates(data,
                            percentile = percentile,
                            lower.thresh = lower.thresh,
                            middle.thresh = middle.thresh,
                            upper.thresh = upper.thresh,
                            reverse = TRUE)

    # Final plot preparations
    progress$set(value = 0.7, detail = "preparing final plot")
    
    # formulate variable names dynamically
    gcc_val = sprintf("gcc_%s", percentile)
    rcc_val = sprintf("rcc_%s", percentile)
    
    #out_val = data[,sprintf("outlierflag_gcc_%s", percentile)]
    col_val = colnames(data)
    
    smooth_val_gcc = sprintf("smooth_gcc_%s", percentile)
    smooth_val_rcc = sprintf("smooth_rcc_%s", percentile)
    
    ci_val = sprintf("smooth_ci_gcc_%s", percentile)
    
    # stuff things in reactive value
    date = data$date
    doy = data$doy
    year = data$year
    
    # gcc / bcc / rcc time series
    gcc = data[, which(col_val == gcc_val)]
    bcc = data$midday_b / (data$midday_r + data$midday_g + data$midday_b)
    rcc = data$midday_r / (data$midday_r + data$midday_g + data$midday_b)
    
    out = data[,sprintf("outlierflag_gcc_%s", percentile)]
    int_flag = data$int_flag
    
    # rename outlier values with proper descriptive names
    gcc_smooth = data[, which(col_val == smooth_val_gcc)]
    
    ci = data[, which(col_val == ci_val)]
    
    # mark gaps
    # gcc_smooth[!is.na(int_flag)] = NA
    # ci[!is.na(int_flag)] = NA
    
    # create data frame
    plot_data = data.frame(date, year, doy, gcc, rcc, bcc, out, gcc_smooth, ci, int_flag)

    # return data
    return(list(plot_data, spring, fall))
  }
  
  # observe the state of the table, if changed update the data
  inputData = reactive({
    downloadData(
      as.numeric(input$table_row_last_clicked),
      as.numeric(input$frequency),
      input$percentile
    )
  })
  
  # plot the data ---------------
  output$time_series_plot = renderPlotly({
    
    # grab plotting data
    data = inputData()
    
    # grab some site specific data (sitename etc)
    myrow = as.numeric(input$table_row_last_clicked)
    site = as.character(df$site[as.numeric(myrow)])
    veg = as.character(df$veg_type[as.numeric(myrow)])
    roi = as.numeric(df$roi_id_number[as.numeric(myrow)])
    
    # axis styling
    ax = list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    
    # plotting routine
    if (is.null(data)) {
      # populate the download handler with error message
      output$downloadData <- downloadHandler(
        filename = sprintf(
          'phenology_data_%s_%s_%04d_%s.csv',
          site,
          veg,
          roi,
          Sys.Date()
        ),
        content = function(file = filename) {
          write.table(
            "NO DATA",
            file,
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE,
            sep = ','
          )
        }
      )
      
      # plot error message
      p = plot_ly(
        x = 0,
        y = 0,
        text = "NO DATA - SELECT A (DIFFERENT) SITE",
        mode = "text"
      ) %>% layout(xaxis = ax, yaxis = ax)
      
    } else{
      # split up the data into true plotting data (time series) and annotation (phenology dates)
      plot_data = data[[1]]
      spring = data[[2]]
      fall = data[[3]]
      
      # bind spring and fall phenology data in a coherent format
      phenology = rbind(spring,fall)
      rising_length = dim(spring)[1]
      falling_length = dim(fall)[1]
      direction = c(rep("rising", rising_length),
                    rep("falling", falling_length))
      sitename = rep(site,rising_length + falling_length)
      veg_type = rep(veg,rising_length + falling_length)
      roi_id = rep(sprintf("%04d",roi),rising_length + falling_length)
      
      phenology[, 1:9] = apply(phenology[, 1:9], 2, function(x)
        as.character(as.Date(x)))
      
      # bind in new labels
      phenology = cbind(sitename,veg_type,roi_id,direction,phenology)
      
      # drop NA lines
      phenology = na.omit(phenology)
      
      # create header with output information
      phenology_header = matrix("#",12,1)
      
      # populate the header file
      phenology_header[2,1] = "# Transition date estimates"
      phenology_header[4,1] = sprintf("# Sitename: %s",site)
      phenology_header[5,1] = sprintf("# Vegetation Type: %s",veg)
      phenology_header[6,1] = sprintf("# ROI ID: %04d",roi)
      phenology_header[7,1] = sprintf("# Aggregation period: %s", as.numeric(input$frequency))
      phenology_header[8,1] = sprintf("# Year min: %s",
                                      min(strptime(as.matrix(phenology[, 5:13]),"%Y-%m-%d")$year +
                                            1900),
                                      na.rm = TRUE)
      phenology_header[9,1] = sprintf("# Year max: %s",
                                      max(strptime(as.matrix(phenology[, 5:13]),"%Y-%m-%d")$year +
                                            1900),
                                      na.rm = TRUE)
      phenology_header[10,1] = sprintf("# Creation Date: %s", Sys.Date())
      phenology_header[11,1] = sprintf("# Creation Time: %s", format(Sys.time(), "%H:%M:%S"))
      
      # populate the download handler with phenology data
      output$downloadData <- downloadHandler(
        filename = sprintf(
          'phenology_data_%s_%s_%04d_%s.csv',
          site,
          veg,
          roi,
          Sys.Date()
        ),
        content = function(file = filename) {
          write.table(
            phenology_header,
            file,
            row.names = FALSE,
            col.names = FALSE,
            quote = FALSE
          )
          
          write.table(
            phenology,
            file,
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE,
            sep = ',',
            append = TRUE
          )
        }
      )
      
      # add an ID column to the phenology data
      # for easy reshaping and plotting
      spring = data.frame(id=1:nrow(spring),spring)
      fall = data.frame(id=1:nrow(fall),fall)
      
      # convert to long format for confidence intervals
      if (nrow(na.omit(spring)) == 0) {
        
        names(spring) = sub("_lower","", names(spring)) # rename columns
      
        } else{
        
        spring_sos = reshape(
          spring[, grep(sprintf("_%s",lower.thresh*100), names(spring))],
          #idvar = sprintf("threshold_%s",lower.thresh*100),
          idvar = "id",
          direction = "long",
          varying = list(c(2, 3))
        )
        
        spring_eos = reshape(
          spring[, grep(sprintf("_%s",upper.thresh*100), names(spring))],
          #idvar = sprintf("threshold_%s",upper.thresh*100),
          idvar = "id",
          direction = "long",
          varying = list(c(2, 3))
        )
        
        spring_mos = reshape(
          spring[, grep(sprintf("_%s",middle.thresh*100), names(spring))],
          #idvar = sprintf("threshold_%s",middle.thresh*100),
          idvar = "id",
          direction = "long",
          varying = list(c(2, 3))
        )
        
        # merge data
        spring = cbind(spring_sos,spring_mos,spring_eos)
        spring = spring[,-grep("time",names(spring))]
        names(spring) = sub("_lower","", names(spring)) # rename columns

      }
      
      if (nrow(na.omit(fall)) == 0) { # when no data is available, should check this
        
        names(fall) = sub("_lower","", names(fall)) # rename columns
        
      } else{
        fall_sos =  reshape(
          fall[, grep(sprintf("_%s",upper.thresh*100), names(fall))],
          #idvar = sprintf("threshold_%s",upper.thresh*100),
          idvar = "id",
          direction = "long",
          varying = list(c(2, 3))
        )
        
        fall_mos =  reshape(
          fall[, grep(sprintf("_%s",middle.thresh*100), names(fall))],
          #idvar = sprintf("threshold_%s",middle.thresh*100),
          idvar = "id",
          direction = "long",
          varying = list(c(2, 3))
        )
        
        fall_eos =  reshape(
          fall[, grep(sprintf("_%s",lower.thresh*100), names(fall))],
          #idvar = sprintf("threshold_%s",lower.thresh*100),
          idvar = "id",
          direction = "long",
          varying = list(c(2, 3))
        )

        # merge data
        fall = cbind(fall_sos,fall_mos,fall_eos)
        fall = fall[,-grep("time",names(fall))]
        names(fall) = sub("_lower","", names(fall)) # rename columns
        
      }

      # full time series
      if (input$plot_type == "bydate") {
        
        # duplicate outlier flag data, for plotting
        plot_data$outlier_symbols = plot_data$out
        plot_data$colours = plot_data$out

        plot_data$colours[plot_data$colours == 0] = rgb(0.3, 0.3, 0.3)
        plot_data$colours[plot_data$colours == 1] = rgb(1, 0, 0)
                
        # rename the outliers, make it look nice
        plot_data$out[plot_data$out == 0] = "Gcc ok"
        plot_data$out[plot_data$out == 1] = "Gcc outlier"
        
        # rename the outliers symbols
        plot_data$outlier_symbols[plot_data$outlier_symbol == 0] = "circle"
        plot_data$outlier_symbols[plot_data$outlier_symbol == 1] = "circle-open"
        
        # when showing multiple time series
        # remove the transition dates
        if (input$rccbcc == "FALSE") {
          # base plot shows the Gcc confidence interval
          # switch order for cleaner plotting
          p = plot_ly(
            data = plot_data,
            x = date,
            y = gcc_smooth + ci,
            mode = "lines",
            fill = "none",
            line = list(width = 0, color = "rgb(200,200,200)"),
            showlegend = FALSE,
            name = "95% CI"
          ) %>%
            add_trace(
              data = plot_data,
              x = date,
              y = gcc_smooth - ci,
              mode = "lines",
              fill = "tonexty",
              line = list(width = 0, color = "rgb(200,200,200)"),
              showlegend = TRUE,
              name = "Gcc 95% CI"
            ) %>%
            add_trace(
              data = plot_data,
              x = date,
              y = gcc_smooth,
              mode = "lines",
              line = list(width = 2, color = "rgb(120,120,120)"),
              name = "Gcc loess fit",
              showlegend = TRUE
            ) %>%
            add_trace(
              data = plot_data,
              x = date,
              y = gcc,
              color = as.factor(out),
              mode = "markers",
              marker=list(color=colours),
              showlegend = TRUE
            ) %>%
            # SOS spring
            # 10%
            add_trace(
              x = as.Date(spring[,grep(sprintf("^transition_%s$",lower.thresh*100),names(spring))]),
              y = spring[,grep(sprintf("threshold_%s",lower.thresh*100),names(spring))],
              mode = "markers",
              marker = list(color = "#7FFF00", symbol = "circle"),
              name = sprintf("SOS (%s %%)",lower.thresh*100)
            ) %>%
            add_trace(
              x = as.Date(spring[,grep(sprintf("^transition_%s_ci$",lower.thresh*100),names(spring))]),
              y = spring[,grep(sprintf("threshold_%s",lower.thresh*100),names(spring))],
              mode = "lines",
              group = spring$id,
              showlegend = FALSE,
              line = list(color = "#7FFF00")
            ) %>%
            # 25 %
            add_trace(
              x = as.Date(spring[,grep(sprintf("^transition_%s$",middle.thresh*100),names(spring))]),
              y = spring[,grep(sprintf("threshold_%s",middle.thresh*100),names(spring))],
              mode = "markers",
              marker = list(color = "#66CD00", symbol = "square"),
              name = sprintf("SOS (%s %%)",middle.thresh*100)
            ) %>%
            add_trace(
              x = as.Date(spring[,grep(sprintf("^transition_%s_ci$",middle.thresh*100),names(spring))]),
              y = spring[,grep(sprintf("threshold_%s",middle.thresh*100),names(spring))],
              mode = "lines",
              group = spring$id,
              showlegend = F,
              line = list(color = "#66CD00")
            ) %>%

            # 50 %
            add_trace(
              x = as.Date(spring[,grep(sprintf("^transition_%s$",upper.thresh*100),names(spring))]),
              y = spring[,grep(sprintf("threshold_%s",upper.thresh*100),names(spring))],
              mode = "markers",
              marker = list(color = "#458B00", symbol = "diamond"),
              name = sprintf("SOS (%s %%)",upper.thresh*100)
            ) %>%
            add_trace(
              x = as.Date(spring[,grep(sprintf("^transition_%s_ci$",upper.thresh*100),names(spring))]),
              y = spring[,grep(sprintf("threshold_%s",upper.thresh*100),names(spring))],
              mode = "lines",
              group = spring$id,
              showlegend = FALSE,
              line = list(color = "#458B00")
            ) %>%

            # EOS fall
            # 50%
            add_trace(
              x = as.Date(fall[,grep(sprintf("^transition_%s$",upper.thresh*100),names(fall))]),
              y = fall[,grep(sprintf("threshold_%s",upper.thresh*100),names(fall))],
              mode = "markers",
              marker = list(color = "#FFB90F", symbol = "diamond"),
              name = sprintf("EOS (%s %%)",upper.thresh*100)
            ) %>%
            add_trace(
              x = as.Date(fall[,grep(sprintf("^transition_%s_ci$",upper.thresh*100),names(fall))]),
              y = fall[,grep(sprintf("threshold_%s",upper.thresh*100),names(fall))],
              mode = "lines",
              group = fall$id,
              showlegend = FALSE,
              line = list(color = "#FFB90F")
            ) %>%
            # 25 %
            add_trace(
              x = as.Date(fall[,grep(sprintf("^transition_%s$",middle.thresh*100),names(fall))]),
              y = fall[,grep(sprintf("threshold_%s",middle.thresh*100),names(fall))],
              mode = "markers",
              marker = list(color = "#CD950C", symbol = "square"),
              name = sprintf("EOS (%s %%)",middle.thresh*100)
            ) %>%
            add_trace(
              x = as.Date(fall[,grep(sprintf("^transition_%s_ci$",middle.thresh*100),names(fall))]),
              y = fall[,grep(sprintf("threshold_%s",middle.thresh*100),names(fall))],
              mode = "lines",
              group = fall$id,
              showlegend = FALSE,
              line = list(color = "#CD950C")
            ) %>%

            # 10 %
            add_trace(
              x = as.Date(fall[,grep(sprintf("^transition_%s$",lower.thresh*100),names(fall))]),
              y = fall[,grep(sprintf("threshold_%s",lower.thresh*100),names(fall))],
              mode = "markers",
              marker = list(color = "#8B6508", symbol = "circle"),
              name = sprintf("EOS (%s %%)",lower.thresh*100)
            ) %>%
            add_trace(
              x = as.Date(fall[,grep(sprintf("^transition_%s_ci$",lower.thresh*100),names(fall))]),
              y = fall[,grep(sprintf("threshold_%s",lower.thresh*100),names(fall))],
              mode = "lines",
              group = fall$id,
              showlegend = FALSE,
              line = list(color = "#8B6508")
            ) %>%
            layout(xaxis = list(title = "Date"),
                   yaxis = list(title = "Gcc"))
        } else {
           
          p = plot_ly(
               data = plot_data,
               x = date,
               y = bcc,
               mode = "markers",
               marker = list(color = rgb(0, 0, 1), symbol = "circle"),
               name = "Bcc",
               showlegend = TRUE
             ) %>%
            add_trace(
              data = plot_data,
              x = date,
              y = rcc,
              mode = "markers",
              marker = list(color = rgb(1, 0, 0), symbol = "circle"),
              name = "Rcc",
              showlegend = TRUE
            ) %>%
            add_trace(
              data = plot_data,
              x = date,
              y = gcc_smooth,
              mode = "lines",
              line = list(width = 2, color = "#66CD00"),
              name = "Gcc loess fit",
              showlegend = TRUE
            ) %>%
            add_trace(
              data = plot_data,
              x = date,
              y = gcc,
              mode = "markers",
              marker = list(color = "#66CD00", symbol = outlier_symbols),
              name = "Gcc",
              showlegend = TRUE
            ) %>%
            layout(xaxis = list(title = "Date"),
                   yaxis = list(title = "Gcc / Rcc / Bcc"))
        }
        
      } else{
        # # condense to one plot along DOY
        if (input$plot_type == "byyear") {
          # delete interpolated data
          # NA values == data available
          plot_data$gcc_smooth[!is.na(plot_data$int_flag)] = NA
          
          gcc_smooth_ltm = by(plot_data$gcc_smooth,
                              INDICES = plot_data$doy,
                              mean,
                              na.rm = TRUE)
          gcc_smooth_sd = by(plot_data$gcc_smooth,
                             INDICES = plot_data$doy,
                             sd,
                             na.rm = TRUE)
          doy = as.numeric(names(gcc_smooth_ltm))
          
          p = plot_ly(
            x = doy,
            y = gcc_smooth_ltm + gcc_smooth_sd,
            mode = "lines",
            fill = "none",
            line = list(width = 0, color = "rgb(200,200,200)"),
            showlegend = FALSE,
            name = "1 SD"
          ) %>%
            add_trace(
              x = doy,
              y = gcc_smooth_ltm - gcc_smooth_sd,
              mode = "lines",
              fill = "tonexty",
              line = list(width = 0, color = "rgb(200,200,200)"),
              showlegend = TRUE,
              name = "1 SD"
            ) %>%
            add_trace(
              x = doy,
              y = gcc_smooth_ltm,
              line = list(
                width = 2,
                dash = "dot",
                color = "black"
              ),
              name = "LTM",
              showlegend = TRUE
            ) %>%
            add_trace(
              data = plot_data,
              x = doy,
              y = gcc_smooth,
              group = year,
              colors = "Set1",
              mode = "lines"
            ) %>%
            layout(xaxis = list(title = "DOY"),
                   yaxis = list(title = "Gcc"))
        } else {
          # these are the transition date regression plots
          
          if (dim(fall)[1] < 9 & dim(spring)[1] < 9) {
            p = plot_ly(
              x = 0,
              y = 0,
              text = "TOO FEW (<9) DATES FOR MEANINGFUL REGRESSION ANALYSIS",
              mode = "text"
            ) %>% layout(xaxis = ax, yaxis = ax)
          } else {
            
            # grab dates from the fall and spring matrices
            fall_date = unique(as.Date(fall[,grep(sprintf("^transition_%s$",upper.thresh*100),names(fall))]))
            spring_date = unique(as.Date(spring[,grep(sprintf("^transition_%s$",upper.thresh*100),names(spring))]))
            
            fall_doy = as.numeric(format(fall_date, "%j"))
            fall_year = as.numeric(format(fall_date, "%Y"))
            
            spring_doy = as.numeric(format(spring_date, "%j"))
            spring_year = as.numeric(format(spring_date, "%Y"))
            
            # # regression stats
            reg_spring = lm(spring_doy ~ spring_year)
            reg_fall = lm(fall_doy ~ fall_year)
            
            # summaries
            reg_spring_sum = summary(reg_spring)
            reg_fall_sum = summary(reg_fall)
            
            # r-squared and slope
            r2_spring = round(reg_spring_sum$r.squared, 2)
            slp_spring = round(reg_spring_sum$coefficients[2, 1], 2)
            r2_fall = round(reg_fall_sum$r.squared, 2)
            slp_fall = round(reg_fall_sum$coefficients[2, 1], 2)
            
            p1 = plot_ly(
              x = spring_year,
              y = spring_doy,
              marker = list(color = "#66CD00", symbol = "square"),
              mode = "markers",
              name = "Spring",
              yaxis = "y1",
              title = "PhenoCam Phenology (DOY)"
            ) %>%
              add_trace(
                x = fall_year,
                y = fall_doy,
                marker = list(color = "#CD950C", symbol = "square"),
                mode = "markers",
                name = "Autumn",
                yaxis = "y1"
              ) %>%
              add_trace(
                x = spring_year,
                y = reg_spring$fitted.values,
                mode = "lines",
                name = sprintf("R2: %s| slope: %s", r2_spring, slp_spring),
                line = list(width = 2, color = "#66CD00"),
                yaxis = "y1"
              ) %>%
              add_trace(
                x = fall_year,
                y = reg_fall$fitted.values,
                mode = "lines",
                name = sprintf("R2: %s| slope: %s", r2_fall, slp_fall),
                line = list(width = 2, color = "#CD950C"),
                yaxis = "y1"
              ) %>%
              layout(xaxis = list(title = "Year"),
                     yaxis = list(title = "DOY"))
          }
        }
      }
    }
  }) # plotly action end
} # server function end