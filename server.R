#################################################
#               Joint Space Map                 #
#################################################


library("shiny")
#library("foreign")

shinyServer(function(input, output) {
  ### Data import:
  Dataset <- reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    Dataset <- as.data.frame(read.table(input$file$datapath ,header=TRUE))
    #Dataset = t(Dataset)
    return(Dataset)
  })
  
  # Select variables:
  output$varselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    # Variable selection:
    
    checkboxGroupInput("Attr", "Choose Attributes (At least 2 attributes must be selected)",
                       rownames(Dataset()), rownames(Dataset()))
    
      })
  
  Dataset1 <- reactive({
    if (is.null(input$file1)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    Dataset1 <- as.data.frame(read.table(input$file1$datapath ,header=TRUE))
    return(Dataset1)
  })
  
  # Select variables:
  output$varselect1 <- renderUI({
    if (identical(Dataset1(), '') || identical(Dataset1(),data.frame())) return(NULL)
    # Variable selection:
    
    checkboxGroupInput("users", "Choose Users (At least 1 user must be selected)",
                       rownames(Dataset1()), head(rownames(Dataset1())))
    
  })
  
  output$table <- renderTable({
    if (is.null(input$Attr) || length(input$Attr)==0) return(NULL)
    return((Dataset()[input$Attr,]))
  })
  
  output$table1 <- renderTable({
    if (is.null(input$users) || length(input$users)==0) return(NULL)
    return((Dataset1()[input$users,]))
  })
 
  output$plot = renderPlot({  
  
  if (is.null(input$file))
  return(NULL)
  
  if (is.null(input$file1))
  return(NULL)
    
  mydata = t(as.data.frame(read.table(input$file$datapath ,header=TRUE)))
  mydata = mydata[,input$Attr]
  
  pref = as.data.frame(read.table(input$file1$datapath ,header=TRUE))
  pref = pref[input$users,]
  
  
  
  ### --- Para 3 of code ---- ###
  # --- write the JSM func ---
  
  JSM <- function(inp1, prefs){
    
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
    
    points(x = fit1$x[,1], y = fit1$x[,2], pch = 19, col ="red")
    
    text(x = fit1$x[,1], y = fit1$x[,2], labels = brdnames,col ="black", cex = 1.1)
    
    # --- add preferences to map ---#
    
    k1 = 2; #scale-down factor
    
    pref = data.matrix(prefs)# make data compatible
    
    pref1 = pref %*% fit1$x[, 1:2]
    
    for (i1 in 1:nrow(pref1)){
      
      segments(0, 0, x1 = pref1[i1,1]/k1, y1 = pref1[i1,2]/k1, col="maroon2", lwd=1.25)
      
      points(x = pref1[i1,1]/k1, y = pref1[i1,2]/k1, pch=19, col="maroon2")
      
      text(x = pref1[i1,1]/k1, y = pref1[i1,2]/k1, labels = rownames(pref)[i1], adj = c(0.5, 0.5), col ="maroon2", cex = 1.1)
      
    }
    
    # voila, we're done! #
    
  } 					# JSM func ends
  
  JSM(mydata, pref)
  
  })
  
  # Show table:
  output$plot1 = renderPlot({  
    
    if (is.null(input$file))
      return(NULL)
    
    if (is.null(input$file1))
      return(NULL)
    
    mydata2 = t(as.data.frame(read.table(input$file$datapath ,header=TRUE)))
    mydata2 = mydata2[,input$Attr]
    fit = prcomp(mydata2) # extract prin compts
  plot(fit, "Variance of PCA")
  
  })
  
})