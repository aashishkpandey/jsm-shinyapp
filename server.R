#################################################
#               Joint Space Map                 #
#################################################


library("shiny")
library("fmsb")

build.spiderplot = function(dat, title){  # input DF with top 2 rows being maxmins
  
  radarchart(dat,
             axistype = 1,     # 0 means no axis label. 1 means center axis label only. 2 means around-the-chart label only. Upto 5
             seg = round(max(dat)-min(dat))+1 ,          # The number of segments for each axis (default 4).
             
             plty = 1:(nrow(dat)-2),         # plot ka line type.
             plwd = 1:(nrow(dat)-2),
             pcol = 1:(nrow(dat)-2),
             pdensity = c(20, 40),  # A vector of filling density of polygons: Default NULL, which is repeatedly used.
             pangle = c(10, 45),  # A vector of the angles of lines used as filling polygons: Default 45, which is repeatedly used.
             pfcol = 1:(nrow(dat)-2),   # plot.fill.color, vector of color codes for filling polygons: Default NA
             vlabels = colnames(dat), #c("Total\nQOL", "Physical\naspects",   # look at the way line break \n is used inside the vlabels
             # "Phychological\naspects", "Social\naspects", "Environmental\naspects"),
             title = title, # "(axis=1, 5 segments, with specified vlabels)",
             vlcex=1)  # Font size magnification for vlabels. If NULL, the font size is fixed at text()'s default. Default NULL.
  
  legend("bottomright",
         legend = rownames(dat)[3:nrow(dat)],
         # pch = c(15,16),
         lwd = c(1:(nrow(dat)-2)),
         col = c(1:(nrow(dat)-2)),
         lty = c(1:(nrow(dat)-2)))
}

# --- write the JSM func ---

JSM <- function(inp1, prefs,k0,k1){
  
  # inp1 = perception matrix with row and column headers
  # brands in rows and attributes in columns
  # prefs = preferences matrix
  
  par(pty="m")
  
  fit = prcomp(inp1, scale.=TRUE) # extract prin compts
  
  plot(fit$rotation[,1:2], # use only top 2 prinComps
       
       type ="n",xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), # plot parms
       
       main ="Joint Space map ") # plot title
  
  abline(h=0); abline(v=0) # build horiz & vert axes
  
  attribnames = colnames(inp1)
  
  brdnames = rownames(inp1)
  
  # <-- insert attrib vectors as arrows--
  
  for (i1 in 1:nrow(fit$rotation)){
    
    arrows(0,0, x1=fit$rotation[i1,1]*fit$sdev[1], y1=fit$rotation[i1,2]*fit$sdev[2], col="blue", lwd=1.5);
    
    text(x=fit$rotation[i1,1]*fit$sdev[1],y=fit$rotation[i1,2]*fit$sdev[2], labels=attribnames[i1],col="blue", cex=1.1)}
  
  # <--- make co-ords within (-1,1) frame #
  
  fit1 = fit
  
  fit1$x[,1] = fit$x[,1]/apply(abs(fit$x),2,sum)[1]
  
  fit1$x[,2] = fit$x[,2]/apply(abs(fit$x),2,sum)[2]
  
  points(x = fit1$x[,1]*k0, y = fit1$x[,2]*k0, pch = 19, col ="red")
  
  text(x = fit1$x[,1]*k0, y = fit1$x[,2]*k0, labels = brdnames,col ="black", cex = 1.1)
  
  # --- add preferences to map ---#
  
  k1 = k1; #scale-down factor
  
  pref = data.matrix(prefs)# make data compatible
  
  pref1 = pref %*% fit1$x[, 1:2]
  
  for (i1 in 1:nrow(pref1)){
    
    segments(0, 0, x1 = pref1[i1,1]*k1, y1 = pref1[i1,2]*k1, col="maroon2", lwd=1.25)
    
    points(x = pref1[i1,1]*k1, y = pref1[i1,2]*k1, pch=19, col="maroon2")
    
    text(x = pref1[i1,1]*k1, y = pref1[i1,2]*k1, labels = rownames(pref)[i1], adj = c(0.5, 0.5), col ="maroon2", cex = 1.1)
    
  }
  
  
} 					# JSM func ends


shinyServer(function(input, output) {
  
  #++_____________++
  ### Data import:
  Dataset <- reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    Dataset <- read.csv(input$file$datapath ,header=TRUE)
    row.names(Dataset) = Dataset[,1]; Dataset= Dataset[,2:ncol(Dataset)]
    #Dataset = t(Dataset)
    return(Dataset)
  })
  
  #++_____________++
  # Select variables:
  output$varselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    # Variable selection:
    
    checkboxGroupInput("Attr", "Choose Attributes (At least 3 attributes must be selected)",
                       rownames(Dataset()), rownames(Dataset()))
    
  })
  
  #++_____________++
  output$varselect2 <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    # Variable selection:
    
    checkboxGroupInput("rows", "Choose Firms (At least 2 Firms must be selected) - Only for Spider Chart",
                       colnames(Dataset()), colnames(Dataset()))
    
  })
  
  #++_____________++
  Dataset1 <- reactive({
    if (is.null(input$file1)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    Dataset1 <- read.csv(input$file1$datapath ,header=TRUE)
    row.names(Dataset1) = Dataset1[,1]; Dataset1= Dataset1[,2:ncol(Dataset1)]
    return(Dataset1)
  })
  
  #++_____________++
  # Select variables:
  output$varselect1 <- renderUI({
    if (identical(Dataset1(), '') || identical(Dataset1(),data.frame())) return(NULL)
    # Variable selection:
    
    checkboxGroupInput("users", "Choose Users (At least 1 user must be selected)",
                       rownames(Dataset1()), head(rownames(Dataset1())))
    
  })
  
  #++_____________++
  output$table <- renderTable({
    if (is.null(input$Attr) || length(input$Attr)==0) return(NULL)
    d = Dataset()[input$Attr,]
    d$Attributes = row.names(d)
    d = d[,c(ncol(d),1:(ncol(d)-1))]
    return(d)
  })
  
  #++_____________++
  output$table1 <- renderTable({
    if (is.null(input$users) || length(input$users)==0) return(NULL)
    d1 = Dataset1()[input$users,]
    d1$user = row.names(d1)
    d1 = d1[,c(ncol(d1),1:(ncol(d1)-1))]
    return(d1)
  })
  
  #++_____________++
  output$plot = renderPlot({  
    
    if (is.null(input$file))
      return(NULL)
    
    
    
    mydata = read.csv(input$file$datapath ,header=TRUE)
    row.names(mydata) = mydata[,1];
    mydata = mydata[,2:ncol(mydata)]; 
    mydata = t(mydata)
    mydata = mydata[,input$Attr]
    
    
    # mydata = t(as.data.frame(read.table(input$file$datapath ,header=TRUE)))
    # mydata = mydata[,input$Attr]
    
    if (is.null(input$file1)){
      pref = matrix(0,2,nrow(mydata))
    }
    
    else {
      pref = read.csv(input$file1$datapath ,header=TRUE)
      row.names(pref) = pref[,1]
      pref = pref[,2:ncol(pref)]
      pref = pref[input$users,]
    }
    ### --- Para 3 of code ---- ###

    JSM(mydata, pref,k0 =input$k0 ,k1 = input$k1)
    
  })
  
  # Show table:
  output$plot1 = renderPlot({  
    
    # if (is.null(input$file))
    #   return(NULL)
    # 
    # if (is.null(input$file1))
    #   return(NULL)
    
    mydata2 = read.csv(input$file$datapath ,header=TRUE)
    row.names(mydata2) = mydata2[,1];
    mydata2 = mydata2[,2:ncol(mydata2)]; 
    mydata2 = t(mydata2)
    mydata2 = mydata2[,input$Attr]
    fit = prcomp(mydata2) # extract prin compts
    plot(fit, "Variance of PCA")
    
  })
  
  output$spiderplot = renderPlot({  
    
    if (is.null(input$file))
      return(NULL)
    # if (is.null(input$file1))
    #   return(NULL)
    # 
    
    mydata3 = read.csv(input$file$datapath ,header=TRUE)
    row.names(mydata3) = mydata3[,1];
    mydata3 = mydata3[,2:ncol(mydata3)]; 
    mydata3 = t(mydata3)
    mydata3 = mydata3[input$rows,input$Attr]
    
    #mydata3 = mydata3[input$rows,input$Attr]
    
    brand = t(mydata3)
    brd.mean = apply(brand, 2, mean)
    
    b0 = t(brand)
    max1 = apply(b0, 2, max); min1 = apply(b0, 2, min);
    # max1; min1
    maxmin.df =  data.frame(rbind(max1, min1));
    # maxmin.df
    colnames(b0) = colnames(maxmin.df)
    brd.df = data.frame(rbind(maxmin.df, b0))
    #brd.df
    
    build.spiderplot(brd.df, "Spider Chart")
  })
  
  
  # Download data files
  output$downloadData1 <- downloadHandler(
    filename = function() { "officestar perceptual.csv" },
    content = function(file) {
      write.csv(read.csv("data/officestar perceptual.csv"), file, row.names=F)
    }
  )
  output$downloadData2 <- downloadHandler(
    filename = function() { "officestar preference.csv" },
    content = function(file) {
      write.csv(read.csv("data/officestar preference.csv"), file, row.names=F)
    }
  )

  output$downloadData3 <- downloadHandler(
    filename = function() { "JSM Cities Perception.csv" },
    content = function(file) {
      write.csv(read.csv("data/JSM Cities Perception.csv"), file, row.names=F)
    }
  )
  
  output$downloadData4 <- downloadHandler(
    filename = function() { "JSM Cities Preferece.csv" },
    content = function(file) {
      write.csv(read.csv("data/JSM Cities Preferece.csv"), file, row.names=F)
    }
  )
  
  
})