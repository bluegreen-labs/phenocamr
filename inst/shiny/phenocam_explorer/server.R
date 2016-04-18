# Dynamic version

# load required libraries
require(shiny, quietly = TRUE)
require(shinydashboard, quietly = TRUE)
require(leaflet, quietly = TRUE)
require(plotly, quietly = TRUE)
require(DT, quietly = TRUE)
source('detect.outliers.r')
source('optimal.span.r')

# grab the OS info
OS = Sys.info()[1]
machine = Sys.info()[4]

# When on the machine of the developer, sideload the code locally
# for quick reviewing of changes to the GUI
if (machine == "squeeze" | machine == "Pandora.local"){
  
  # load all functions
  files = list.files("/data/Dropbox/Research_Projects/code_repository/bitbucket/PhenoCam/PhenoCam/R/","*.r",full.names = TRUE)
  for (i in files){
    source(i)
  }
  
  # set data path
  path = "./data"
  
}else{
  
  # create temporary directory and move into it
  if (!dir.exists("~/phenocam_cache")){
    dir.create("~/phenocam_cache")
  }
  
  # set the base directory to the cache directory
  # this prevents any temporary files interfering with
  # local data
  setwd("~/phenocam_cache")
  
  # location of the data
  path = "~/phenocam_cache"
  
}

# # grab the latest roi list
# url = "http://phenocam.sr.unh.edu/webcam/roi/roilistinfo/?format=csv"
# df = read.csv(url(url),header=TRUE)
df = read.csv("sites.csv")

# introduce jitter on lat long coordinates
# this avoids that site locations with different PFTs
# will overlap, although not geographically accurate
# it helps visualize things
df$lat_j = df$lat+rnorm(length(df$lat))*0.00005
df$lon_j = df$lon+rnorm(length(df$lat))*0.00005

# subset data to exclude certain PFT / ROI classes which are irrelevant
# (no vegetation, bad ROI, mixed data types)
df = df[-which(df$veg_type=="XX" | df$veg_type == "MX" | df$veg_type == "UN" | df$veg_type == "NV" | df$veg_type == "RF"),]
row.names(df)=1:dim(df)[1]

# create a character field with html to call as marker
# popup. This includes a thumbnail of the site!
df$preview = unlist(lapply(df$site,function(x)paste(
  "<table width=150px, border=0px>",
  "<tr>",
  "<td><b>",
  x,
  "</b></td>",
  "</tr>",
  "<tr>",
  "<td>",
  "<img src=http://phenocam.sr.unh.edu/data/latest/thumbs/",
  x,
  ".thumb.jpg>",
  "</td>",
  "</tr>",
  "</table>",sep="")))

# find the column locations of certain variables (for later data table manipulations)
colloc = c(which(colnames(df)=="lat_j"),which(colnames(df)=="lon_j"))

# start server routine
server = function(input, output,session){
  
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
  filteredData = function(){
    if (!is.null(isolate(v2$lat))) {
      if (input$colors=="ALL"){
        tmp = df[which(df$lat < isolate(v1$lat) & df$lat > isolate(v2$lat) & df$lon > isolate(v1$lon) & df$lon < isolate(v2$lon)),]
        unique(tmp)
      }else{
        df[which(df$lat < isolate(v1$lat) & df$lat > isolate(v2$lat) & df$lon > isolate(v1$lon) & df$lon < isolate(v2$lon) & df$veg_type == input$colors),]
      }
    }else{
      if (input$colors=="ALL"){
        unique(df)
      }else{
        df[df$veg_type == input$colors,]
      }
    }
  }
  
  getValueData = function(table){
    
    nr_sites = length(unique(table$site))
    
    output$site_count = renderInfoBox({
      valueBox(nr_sites,"Sites",
               icon = icon("list"),
               color = "blue"
      )
    })
    
    output$year_count = renderInfoBox({
      valueBox(nr_sites,"Site Years",
               icon = icon("list"),
               color = "blue"
      )
    })
    
    output$season_count = renderInfoBox({
      valueBox(nr_sites,"# Growing Seaons",
               icon = icon("list"),
               color = "blue"
      )
    })
  }
  
  # Use leaflet() here, and only include aspects of the map that
  # won't need to change dynamically (at least, not unless the
  # entire map is being torn down and recreated).
  output$map = renderLeaflet({  
    map = leaflet(df) %>%
      addTiles("http://otile3.mqcdn.com/tiles/1.0.0/sat/{z}/{x}/{y}.jpg",
               attribution='Tiles Courtesy of <a href="http://www.mapquest.com/">MapQuest</a> &mdash; Portions Courtesy NASA/JPL-Caltech and U.S. Depart. of Agriculture, Farm Service Agency',
               group = "JPL") %>%
      addProviderTiles("OpenStreetMap.BlackAndWhite",group = "OSM") %>%
      addMarkers(lat = ~lat_j,lng = ~lon_j, icon = ~vegIcons[veg_type],popup=~preview) %>%
      
      # Layers control
      addLayersControl(
        baseGroups = c("OSM", "JPL"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      setView(lng = 11, lat = 45, zoom = 2)
  })
  
  # Incremental changes to the map. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      addMarkers(lat = ~lat_j,lng = ~lon_j, icon = ~vegIcons[veg_type],popup=~preview)
    
    # update the data table in the explorer
    output$table = DT::renderDataTable({
      tmp = filteredData()[,-c(8:10)]
      return(tmp) },
      selection="single",
      options = list(lengthMenu = list(c(5,10), c('5','10')),pom=list('MAT')), 
      extensions = c('Responsive')
    )
    
    # update value box
    getValueData(filteredData())
    
    # update the climatology plot
    output$test = renderPlot({
      par(mar=c(4,4,1,1))
      plot(1,1,type='n',xaxt='n',yaxt='n',xlab='',ylab='',bty='n',
           xlim=c(0,100),
           ylim=c(0,100))
      text(50,50,"To be completed")
#       plot(MAP~MAT,data=filteredData(),
#            xlab=expression("MAT ("*degree*"C)"),
#            ylab="MAP (mm)",
#            pch=19,
#            col=rgb(0.5,0.5,0.5,0.3),
#            xlim=c(-15,30),
#            ylim=c(0,3000)
#       )
    },height = function() {
      session$clientData$output_test_height
    })
  })
  
  # grab the bounding box, by clicking the map
  observeEvent(input$map_click, {
    # if clicked once reset the bounding box
    # and show all data
    if (!is.null(isolate(v2$lat))) {
      
      # set bounding box values to NULL
      v1$lat = NULL; v2$lat = NULL; v1$lon = NULL; v2$lon = NULL
      
      leafletProxy("map", data = filteredData()) %>%
        clearMarkers() %>%
        clearShapes() %>%
        addMarkers(lat = ~lat_j,lng = ~lon_j, icon = ~vegIcons[veg_type],popup=~preview)
      
      getValueData(filteredData())
      
      # update the climatology plot
      output$test = renderPlot({
        par(mar=c(4,4,1,1))
        plot(MAP~MAT,data=filteredData(),
             xlab=expression("MAT ("*degree*"C)"),
             ylab="MAP (mm)",
             pch=19,
             col=rgb(0.5,0.5,0.5,0.3),
             xlim=c(-15,30),
             ylim=c(0,3000)
        )
      },height = function() {
        session$clientData$output_test_height
      })
      
    }else{
      # grab bounding box coordinates
      # TODO: validate the topleft / bottom right order
      if (!is.null(isolate(v1$lat))) {
        v2$lat = input$map_click$lat
        v2$lon = input$map_click$lng
      }else{
        v1$lat = input$map_click$lat
        v1$lon = input$map_click$lng
        leafletProxy("map", data = filteredData()) %>%
          clearMarkers() %>% 
          addMarkers(lat = ~lat_j,lng = ~lon_j, icon = ~vegIcons[veg_type],popup=~preview) %>%
          addCircleMarkers(lng=isolate(v1$lon),lat=isolate(v1$lat),color="red",radius=3,fillOpacity=1,stroke=FALSE)
      }
    }
    
    # if the bottom right does exist
    if (!is.null(isolate(v2$lat))) {
      
      # subset data based upon topleft / bottomright
      tmp = filteredData()
      
      # check if the dataset is not empty
      if( dim(tmp)[1]!=0 ){
        
        # update the map
        leafletProxy("map", data = tmp) %>%
          clearMarkers() %>% 
          addMarkers(lat = ~lat_j,lng = ~lon_j, icon = ~vegIcons[veg_type],popup=~preview) %>%
          addRectangles(
            lng1=isolate(v1$lon), lat1=isolate(v1$lat),
            lng2=isolate(v2$lon), lat2=isolate(v2$lat),
            fillColor = "transparent",
            color="grey")
        
        # update the data table in the explorer
        output$table = DT::renderDataTable({
          tmp = filteredData()[,-c(8:10)]
          return(tmp) },
          selection="single",
          options = list(lengthMenu = list(c(5,10), c('5','10')),pom=list('MAT')), 
          extensions = c('Responsive')
        )
        
        # update the value box
        getValueData(filteredData())
        
        # update the climatology plot
        output$test = renderPlot({
          par(mar=c(4,4,1,1))
          plot(MAP~MAT,data=filteredData(),
               xlab=expression("MAT ("*degree*"C)"),
               ylab="MAP (mm)",
               pch=19,
               col=rgb(0.5,0.5,0.5,0.3),
               xlim=c(-15,30),
               ylim=c(0,3000)
          )
        },height = function() {
          session$clientData$output_test_height
        })
        
      }else{
        # set bounding box values to NULL
        v1$lat = NULL; v2$lat = NULL; v1$lon = NULL; v2$lon = NULL
        
        leafletProxy("map", data = filteredData()) %>%
          clearMarkers() %>%
          clearShapes() %>%
          addMarkers(lat = ~lat_j,lng = ~lon_j, icon = ~vegIcons[veg_type],popup=~preview)
        
        # update the climatology plot
        output$test = renderPlot({
          par(mar=c(4,4,1,1))
          plot(MAP~MAT,data=filteredData(),
               xlab=expression("MAT ("*degree*"C)"),
               ylab="MAP (mm)",
               pch=19,
               col=rgb(0.5,0.5,0.5,0.3),
               xlim=c(-15,30),
               ylim=c(0,3000)
          )
        },height = function() {
          session$clientData$output_test_height
        })
      }
    }
  })
  
  downloadData = function(myrow,frequency,percentile){
    
    # if nothing selected return NULL
    if (length(myrow)==0){
      return(NULL)
    }
    
    # Create a Progress object
    # Make sure it closes when we exit this reactive, even if there's an error
    progress = shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making plot", value = 0)
    
    # grab the necessary parameters to download the site data
    site = df$site[as.numeric(myrow)]
    veg_type = df$veg_type[as.numeric(myrow)]
    roi_id = df$roi_id_number[as.numeric(myrow)]
    
    # formulate the filename of the data we need to download
    data_file = sprintf("%s/%s_%s_%04d_%sday_v4.csv",path,site,veg_type,roi_id,frequency)
    
    # check if the data is on file, if so assume it is
    # checked for outliers and has smoothed data
    if (file.exists(data_file)){
      data = read.table(data_file,header=TRUE,sep=',')
    }else{
      
      # kick start progress bar
      progress$set(value = 0.2,detail = "Downloading PhenoCam data")
      
      # download phenocam data from the server
      # do not detect outliers and smooth in this step
      # do this separately to allow for proper progresss
      # bar updating (keep people busy as this takes a while)
      download.phenocam(site = site,
                        vegetation = veg_type,
                        roi_id = roi_id,
                        frequency = frequency,
                        outlier_detection = FALSE,
                        smooth = FALSE,
                        out_dir = path)
      
      # detect outliers
      progress$set(value=0.3,detail = "detecting outliers")
      status = try(detect.outliers(data_file),silent=TRUE)
      if(inherits(status,"try-error")){
        return(NULL)
      }
      
      # smooth data
      progress$set(value=0.6,detail = "smoothing data")
      status = try(smooth.ts(data_file),silent=TRUE)
      if(inherits(status,"try-error")){
        return(NULL)
      }
      
      # read the data to plot_data
      data = read.table(data_file,header=TRUE,sep=',')
    }
    
    # grab the transition dates
    spring = transition.dates(data,percentile = percentile,reverse = FALSE)
    spring = spring[,c(grep("*sos*",colnames(spring)),7,8)]
    fall = transition.dates(data,percentile = percentile,reverse = TRUE)
    fall = fall[,c(grep("*eos*",colnames(fall)),7,8)]
    
    # Final plot preparations
    progress$set(value=0.7,detail = "preparing final plot")
    
    # formulate variable names dynamically
    gcc_val = sprintf("gcc_%s",percentile)
    out_val = sprintf("outlierflag_gcc_%s",percentile)
    col_val = names(data)
    smooth_val = sprintf("smooth_gcc_%s",percentile)
    ci_val = sprintf("smooth_ci_gcc_%s",percentile)
    
    # stuff things in reactive value
    date = data$date
    doy = data$doy
    year = data$year
    gcc = data[,which(col_val==gcc_val)]
    out = as.numeric(data[,which(col_val==out_val)])
    int_flag = data$int_flag
    
    # rename outlier values with proper descriptive names
    gcc_smooth = data[,which(col_val==smooth_val)]
    ci = data[,which(col_val==ci_val)]
    
    # mark gaps
    #gcc_smooth[!is.na(int_flag)]=NA
    #ci[!is.na(int_flag)]=NA
    
    # create data frame
    plot_data = data.frame(date,year,doy,gcc,out,gcc_smooth,ci)
    
    # return data
    return(list(plot_data,spring,fall))
  }
  
  # observe the state of the table, if changed update the data
  inputData = reactive({downloadData(as.numeric(input$table_row_last_clicked),as.numeric(input$frequency),input$percentile)})

  # plot the data 
  output$time_series_plot = renderPlotly({
    
    # grab plotting data
    data = inputData()
    
    if (is.null(data)){
      ax = list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      )
      p = plot_ly(x = 0, y = 0, text = "NO DATA - SELECT A (DIFFERENT) SITE", mode = "text") %>% layout(xaxis = ax, yaxis = ax)
    }else{
      
      # split up the data into true plotting data (time series) and annotation (phenology dates)
      plot_data = data[[1]]
      spring = data[[2]]
      fall = data[[3]]
      
      print(spring)
      print(fall)
      
      # convert to long format for confidence intervals
      if (dim(spring)[1] == 1){
        spring_long = spring
      }else{
        spring_long = reshape(spring,idvar="spring_min_threshold",direction="long",varying=list(c(2,3)))
      }
      
      if (dim(fall)[1] == 1){
        fall_long = fall
      }else{
        fall_long =  reshape(fall,idvar="autumn_max_threshold",direction="long",varying=list(c(2,3)))
      }
      
      # full time series
      if (input$plot_type == "daily"){
        
        # colour scheme for the scatterplot
        if(!any(plot_data$out!=0)){
          set = c(rgb(0.3,0.3,0.3)) 
        }else{
          set = c(rgb(1,0,0),rgb(0.3,0.3,0.3)) 
        }
        
        # rename the outliers, make it look nice
        plot_data$out[plot_data$out==0] = "ok"
        plot_data$out[plot_data$out==1] = "outlier"

        p = plot_ly(data = plot_data, x=date, y = gcc_smooth + ci, mode = "lines",
                    fill = "none", line=list(width=0,color="rgb(200,200,200)"), showlegend = FALSE, name="95% CI") %>%
          add_trace(data = plot_data, x= date, y = gcc_smooth - ci, mode = "lines",
                    fill = "tonexty", line=list(width=0,color="rgb(200,200,200)"), showlegend = TRUE, name="95% CI") %>%
          add_trace(data=plot_data,x=date, y = gcc_smooth, mode="lines",line=list(color="rgb(120,120,120)"), name = "loess fit") %>%
          add_trace(data = plot_data,x=date,y=gcc,color=as.factor(out),mode="markers",colors=set) %>%
          
          # add phenology
          add_trace(data = spring,x=as.Date(spring_sos), y = spring_min_threshold, mode="markers",marker=list(color="green",symbol="square"),name="SOS") %>%
          add_trace(data=spring_long,
                    x = as.Date(spring_sos_lower_ci),
                    y = spring_min_threshold,
                    mode = "lines",
                    group = spring_min_threshold,
                    showlegend = F, line = list(color = "green")) %>%
          add_trace(data = fall,x=as.Date(autumn_eos), y = autumn_min_threshold, mode="markers",marker=list(color="orange",symbol="square"),name="EOS") %>%
          add_trace(data=fall_long,
                    x = as.Date(autumn_eos_lower_ci),
                    y = autumn_min_threshold,
                    mode = "lines",
                    group = autumn_min_threshold,
                    showlegend = F, line = list(color = "orange")) %>%
          
          layout(xaxis = list(title="Date"), yaxis = list(title="Gcc"))
        
      }else{ # condensed overlapping yearly plots
        
        p = plot_ly(data = plot_data,
                    x=doy,
                    y=gcc_smooth,
                    group=year,
                    colors="Set1",
                    mode="lines") %>%
          layout(xaxis = list(title="DOY"), yaxis = list(title="Gcc"))
      }
    }
  })
  
} # server function end