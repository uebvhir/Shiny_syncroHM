library(shiny)
library(gplots)
library(RColorBrewer)
library(ggplot2)

function(input, output) {
  
  ##############################################################
  #LOAD NUMERIC DATA (FILE 1)
  dd1 <- reactive({ 
    inFile1 <- input$file1
    if (is.null(inFile1)) return(invisible(NULL))
    else {read.csv2(inFile1$datapath, sep=input$sep1, dec=input$dec1, header=TRUE)}
  })
  
  ##############################################################
  #LOAD CATEGORIC DATA (FILE 2)
  dd2 <- reactive({ 
    inFile2 <- input$file2
    if (is.null(inFile2)) return(invisible(NULL))
    else {read.csv2(inFile2$datapath, sep=input$sep2, dec=input$dec2, header=TRUE)}
  })
  
  ##############################################################
  #LOAD AND TRANSPOSE DATA FROM FILE1  
  dd3 <- reactive({ 
    inFile1 <- input$file1
    if (is.null(inFile1)) return(invisible(NULL))
    else {
      #if(input$transpose1) {
      #vnames1 <- colnames(dd1())
      #snames1 <- as.character(dd1[,1])
      setNames(data.frame(t(read.csv2(inFile1$datapath, sep = input$sep1, dec=input$dec1, header=TRUE)[,-1])), 
               read.csv2(inFile1$datapath, sep = input$sep1, dec=input$dec1, header=TRUE)[,1])
      #}else{
      #  read.csv2(inFile1$datapath, sep = input$sep1, dec=input$dec1, header=TRUE)
      #}
    }
  })
  
  ##############################################################
  #LOAD AND TRANSPOSE DATA FROM FILE2
  dd4 <- reactive({ 
    inFile2 <- input$file2
    if (is.null(inFile2)) return(invisible(NULL))
    else {
      #if(input$transpose2) {
      setNames(data.frame(t(read.csv2(inFile2$datapath, sep = input$sep2, dec=input$dec2, 
                                      header=TRUE, na.strings = c("","NA"))[,-1])), 
               read.csv2(inFile2$datapath, sep = input$sep2, dec=input$dec2, header=TRUE)[,1])
      #}else{
      #  read.csv2(inFile2$datapath, sep = input$sep2, dec=input$dec2, header=TRUE)
      #}
    }
  })
  
  ##############################################################
  #SHOW TABLE OF DATA
  output$table1 <- renderTable(dd1()[c(1:5), ])
  output$table2 <- renderTable(dd2()[c(1:5), ])
  output$table3 <- renderTable(cbind(var=colnames(dd1())[-1], dd3()[ ,c(1:8)]))
  output$table4 <- renderTable(cbind(var=colnames(dd2())[-1], dd4()[ ,c(1:12)]))
  
  ##############################################################
  #UPDATE SELECT INPUT AND OTHER OPTIONS
  #  sortVars <- reactive({
  #    vars <- as.list(colnames(dd1()))
  #    return(vars)
  #  })
  output$var2sort <- renderUI({
    if (input$selectSet == "clin"){
      selectInput("selVar2sort", label = "Select variable to reorder the plots", 
                  choices = as.list(colnames(dd1()[-1])), selected = "Group")
    }else{
      selectInput("selVar2sort", label = "Select variable to reorder the plots", 
                  choices = as.list(colnames(dd2()[-1])))#, selected = "Group")
      
    }
  })
  
  ##############################################################  
  ##############################################################
  #DRAW THE HEATMAP OF MIXED DATA
  
  #DEFINE REORDERING VARIABLE
  sortVarText <- reactive({
    return(input$selVar2sort)
  })
  
  #decrOrder <- reactive({
  #  if (input$orderSet=="asc") {
  #    return(FALSE)
  #  }else{
  #    return(TRUE)
  #  }
  #})
  
  #READ AND WORK WITH DATA (ONLY FOR DEBUG)  
  #df1 <- read.csv2("./www/datosClinDec.csv", sep="\t", dec=".", header=TRUE, row.names = 1)
  #df1[1:5,1:5]
  #rownames(df1)
  #sortVarText <- "Timetoprogression"
  #order(df1[ ,sortVarText])
  #df1.sorted <- df1[order(df1[ ,sortVarText]), ]
  #df1.sorted[1:5,1:5]
  #rownames(df1.sorted)
  #fx <- tf1.sorted <- t(data.matrix(df1.sorted))
  #tf1.sorted[1:5,1:5]
  #
  
  
  
  ########################
  ### MIXED PLOT
  output$plotMix <- renderPlot({  
    
    # check input data and rise an error message if none
    validate(
      need(input$file1, "Please, upload a data set with heterogeneous data")
    )
    
    if (input$selectSet == "clin"){
      # sort data by the variable selected in "var2sort"
      if (input$orderSet=="asc") {
        dd1.sorted <- dd1()[order(dd1()[ ,sortVarText()], decreasing=F), ]
        dd3.sorted <- dd3()[ , order(dd3()[sortVarText(), ], decreasing=F)]
      }else{
        dd1.sorted <- dd1()[order(dd1()[ ,sortVarText()], decreasing=T), ]
        dd3.sorted <- dd3()[ , order(dd3()[sortVarText(), ], decreasing=T)]
      }
    }else{
      # sort data by the variable selected in "num2sort"
      if (input$orderSet=="asc") {
        dd1.sorted <- dd1()[order(dd2()[ ,sortVarText()], decreasing=F), ]
        dd3.sorted <- dd3()[ , order(dd4()[sortVarText(), ], decreasing=F)]
      }else{
        dd1.sorted <- dd1()[order(dd2()[ ,sortVarText()], decreasing=T), ]
        dd3.sorted <- dd3()[ , order(dd4()[sortVarText(), ], decreasing=T)]
      }
    }
    
    # color palette settings 
    maxnum <- 2
    idx <- c()
    for (j in 1:dim(dd1.sorted)[2]){
      if (is.numeric(dd1.sorted[ ,j])){
        idx <- c(idx, j)
        maxnum <- max(maxnum, max(dd1.sorted[ ,j], na.rm = T))
      }
    }
    my_colors <- c(
      colorRampPalette(c(input$colMix1, input$colMix2))(n = 2),
      colorRampPalette(c("ghostwhite", input$colMix2))(n = maxnum+3)
    )
    
    # format data as matrix or data.frame
    dx <- as.data.frame(dd3.sorted)
    fx <- t(data.matrix(dd1.sorted[-1]))
    for (i in idx){
      fx[i-1, ] <- fx[i-1, ]+3
    }
    
    # imgflip function 
    imgflip <- function(x) {t(x[nrow(x):1,])}
  
    # margins (upper heatmap)
    par(oma=c(0,5,3,5)) #(bottom, left, top, right)
    par(mar=c(0,3,3,3)) #(bottom, left, top, right)
    
    # create plot
    image(imgflip(fx),
          #breaks = (1:(nlevels(ff2)+1))-.5,
          col = my_colors,
          xaxt = "n", yaxt = "n"
    )
    abline(v=seq(0, 1, length.out=length(names(dx)))+(0.5/length(names(dx))), col="grey3", lwd=1)
    abline(h=seq(0, 1, length.out=nrow(dx))+(0.5/nrow(dx)), col="grey3", lwd=1)
    axis(2, at=seq(0,1,length.out=nrow(dx)), labels=rev(rownames(fx)), cex.axis= 15/nrow(dx), las=2)
    axis(3, at=seq(0,1,length.out=length(names(dx))), labels=names(dx), cex.axis= 20/length(names(dx)), las=3)
    
  })  
  
  
  ##############################################################
  ##############################################################
  #DRAW THE HEATMAP OF EXPRESSION DATA
  
  #READ AND WORK WITH DATA (ONLY FOR DEBUG)
  #df2 <- read.csv2("./www/datosNum.csv", sep="\t", dec=".", header=TRUE)
  #
  #for (i in 1:dim(dm2)[1]){
  #  print(range(dm2[i,], na.rm = T))
  #  print(range(scale(dm2[i,]), na.rm = T))
  #  print("")
  #}
  #
  
  ### EXPRESS PLOT
  output$plotNum <- renderPlot({  
    
    # check input data and rise an error message if none
    validate(
      need(input$file2, "Please, upload a data set with expression data")
    )
    
    if (input$selectSet == "clin"){
      # sort data by the variable selected in "var2sort"
      if (input$orderSet=="asc") {
        dd2.sorted <- dd2()[order(dd1()[ ,sortVarText()], decreasing=F), ]
        dd4.sorted <- dd4()[ , order(dd3()[sortVarText(), ], decreasing=F)]
      }else{
        dd2.sorted <- dd2()[order(dd1()[ ,sortVarText()], decreasing=T), ]
        dd4.sorted <- dd4()[ , order(dd3()[sortVarText(), ], decreasing=T)]
      }
    }else{
      # sort data by the variable selected in "num2sort"
      if (input$orderSet=="asc") {
        dd2.sorted <- dd2()[order(dd2()[ ,sortVarText()], decreasing=F), ]
        dd4.sorted <- dd4()[ , order(dd4()[sortVarText(), ], decreasing=F)]
      }else{
        dd2.sorted <- dd2()[order(dd2()[ ,sortVarText()], decreasing=T), ]
        dd4.sorted <- dd4()[ , order(dd4()[sortVarText(), ], decreasing=T)]
      }
    }
    
    # format data as matrix or data.frame
    dx <- as.data.frame(dd4.sorted)
    if (input$scale == "auto"){
      fx <- scale(t(data.matrix(dd2.sorted[-1])))
    }else{ # acotar valors de la matriu segons els inputs lower i higher value indicats
      fx <- t(data.matrix(dd2.sorted[-1]))
      if (input$scale == "bound"){
        fx[fx < input$infVal] <- input$infVal
        fx[fx > input$supVal] <- input$supVal
      }
      }
    
    # color palette settings
        my_palette <- colorRampPalette(c(input$colNum1, "grey", input$colNum2))(n = input$colorBreaks+1)
    
    # imgflip function 
    imgflip <- function(x) {t(x[nrow(x):1,])}

    # margins (lower heatmap)
    par(oma=c(3,5,0,5)) #(bottom, left, top, right)
    par(mar=c(0,3,0,3)) #(bottom, left, top, right)
    
    # create plot
    if (input$scale == "asis"){
      my_palette <- colorRampPalette(c(input$colNum1, "grey",input$colNum2))(n = 10)
      image(imgflip(fx),
            #breaks = (1:(nlevels(ff2)+1))-.5,
            col = my_palette,
            xaxt = "n", yaxt = "n",
            breaks = c(seq(-10,0.99,length=5), # for imput$col1
                       #seq(0,86,1.15,length=1),  # for grey
                       seq(1.01,10,length=6)) # for input$col2
      )
      abline(v=seq(0, 1, length.out=length(names(dx)))+(0.5/length(names(dx))), col="grey3", lwd=1)
      abline(h=seq(0, 1, length.out=nrow(dx))+(0.5/nrow(dx)), col="grey3", lwd=1)
      axis(4, at=seq(0,1,length.out=nrow(dx)), labels=rev(rownames(fx)), cex.axis= 15/nrow(dx), las=2)
      #axis(3, at=seq(0,1,length.out=length(names(dx))), labels=names(dx), cex.axis= 20/length(names(dx)), las=3)
    }
    else{
    image(imgflip(fx),
          #breaks = (1:(nlevels(ff2)+1))-.5,
          col = my_palette,
          xaxt = "n", yaxt = "n"
          )
    abline(v=seq(0, 1, length.out=length(names(dx)))+(0.5/length(names(dx))), col="grey3", lwd=1)
    abline(h=seq(0, 1, length.out=nrow(dx))+(0.5/nrow(dx)), col="grey3", lwd=1)
    axis(4, at=seq(0,1,length.out=nrow(dx)), labels=rev(rownames(fx)), cex.axis= 15/nrow(dx), las=2)
    #axis(3, at=seq(0,1,length.out=length(names(dx))), labels=names(dx), cex.axis= 20/length(names(dx)), las=3)
    }
  })  
  
  
  ##############################################################################
  ##############################################################################
  #DOWNLOAD THE PLOT (ES PODRIA FER VIA FUNCIONS QUE FACIN EL PRINT DE CADA PLOT)
  output$down <- downloadHandler(
    filename = function() {"HeatmapPlot.pdf"}, # aquesta linia cal, pero ignora el nom
    content = function(ff) {
 
      pdf(ff)
      par(mfrow=c(2,1), #two rows and 1 col
          oma=c(3,5,3,5), #outer margins (bottom, left, top, right)
          mar=c(0,3,3,3)) #inner margins (bottom, left, top, right) for 1st plot
      
      #################################################
      ############### MIXED PLOT ######################
      # check input data and rise an error message if none
      validate(
        need(input$file1, "Please, upload a data set with heterogeneous data")
      )
      
      if (input$selectSet == "clin"){
        # sort data by the variable selected in "var2sort"
        if (input$orderSet=="asc") {
          dd1.sorted <- dd1()[order(dd1()[ ,sortVarText()], decreasing=F), ]
          dd3.sorted <- dd3()[ , order(dd3()[sortVarText(), ], decreasing=F)]
        }else{
          dd1.sorted <- dd1()[order(dd1()[ ,sortVarText()], decreasing=T), ]
          dd3.sorted <- dd3()[ , order(dd3()[sortVarText(), ], decreasing=T)]
        }
      }else{
        # sort data by the variable selected in "num2sort"
        if (input$orderSet=="asc") {
          dd1.sorted <- dd1()[order(dd2()[ ,sortVarText()], decreasing=F), ]
          dd3.sorted <- dd3()[ , order(dd4()[sortVarText(), ], decreasing=F)]
        }else{
          dd1.sorted <- dd1()[order(dd2()[ ,sortVarText()], decreasing=T), ]
          dd3.sorted <- dd3()[ , order(dd4()[sortVarText(), ], decreasing=T)]
        }
      }
      
      # color palette settings 
      maxnum <- 2
      idx <- c()
      for (j in 1:dim(dd1.sorted)[2]){
        if (is.numeric(dd1.sorted[ ,j])){
          idx <- c(idx, j)
          maxnum <- max(maxnum, max(dd1.sorted[ ,j], na.rm = T))
        }
      }
      my_colors <- c(
        colorRampPalette(c(input$colMix1, input$colMix2))(n = 2),
        colorRampPalette(c("ghostwhite", input$colMix2))(n = maxnum+3)
      )
      
      # format data as matrix or data.frame
      dx <- as.data.frame(dd3.sorted)
      fx <- t(data.matrix(dd1.sorted[-1]))
      for (i in idx){
        fx[i-1, ] <- fx[i-1, ]+3
      }
      
      # imgflip function 
      imgflip <- function(x) {t(x[nrow(x):1,])}
      
      # flip image and plot heatmap
      image(imgflip(fx),
            #breaks = (1:(nlevels(ff2)+1))-.5,
            col = my_colors,
            xaxt = "n", yaxt = "n"
      )
      abline(v=seq(0, 1, length.out=length(names(dx)))+(0.5/length(names(dx))), col="grey3", lwd=1)
      abline(h=seq(0, 1, length.out=nrow(dx))+(0.5/nrow(dx)), col="grey3", lwd=1)
      axis(2, at=seq(0,1,length.out=nrow(dx)), labels=rev(rownames(fx)), cex.axis= 15/nrow(dx), las=2)
      axis(3, at=seq(0,1,length.out=length(names(dx))), labels=names(dx), cex.axis= 20/length(names(dx)), las=3)

      
      par(mar=c(3,3,0,3)) #change inner margins (bottom, left, top, right) for 2nd plot
      ######################################################     
      ########### NUMERIC (EXPRESS) PLOT ###################
      # check input data and rise an error message if none
      validate(
        need(input$file2, "Please, upload a data set with expression data")
      )
      
      if (input$selectSet == "clin"){
        # sort data by the variable selected in "var2sort"
        if (input$orderSet=="asc") {
          dd2.sorted <- dd2()[order(dd1()[ ,sortVarText()], decreasing=F), ]
          dd4.sorted <- dd4()[ , order(dd3()[sortVarText(), ], decreasing=F)]
        }else{
          dd2.sorted <- dd2()[order(dd1()[ ,sortVarText()], decreasing=T), ]
          dd4.sorted <- dd4()[ , order(dd3()[sortVarText(), ], decreasing=T)]
        }
      }else{
        # sort data by the variable selected in "num2sort"
        if (input$orderSet=="asc") {
          dd2.sorted <- dd2()[order(dd2()[ ,sortVarText()], decreasing=F), ]
          dd4.sorted <- dd4()[ , order(dd4()[sortVarText(), ], decreasing=F)]
        }else{
          dd2.sorted <- dd2()[order(dd2()[ ,sortVarText()], decreasing=T), ]
          dd4.sorted <- dd4()[ , order(dd4()[sortVarText(), ], decreasing=T)]
        }
      }
      
      # format data as matrix or data.frame
      dx <- as.data.frame(dd4.sorted)
      if (input$scale == "auto"){
        fx <- scale(t(data.matrix(dd2.sorted[-1])))
      }else{ # acotar valors de la matriu segons els inputs lower i higher value indicats
        fx <- t(data.matrix(dd2.sorted[-1]))
        if (input$scale == "bound"){
          fx[fx < input$infVal] <- input$infVal
          fx[fx > input$supVal] <- input$supVal
        }
      }
      
      # color palette settings
      my_palette <- colorRampPalette(c(input$colNum1, "grey", input$colNum2))(n = input$colorBreaks+1)
      
      # imgflip function 
      imgflip <- function(x) {t(x[nrow(x):1,])}
      
      # create plot
      if (input$scale == "asis"){
        my_palette <- colorRampPalette(c(input$colNum1, "grey",input$colNum2))(n = 10)
        image(imgflip(fx),
              #breaks = (1:(nlevels(ff2)+1))-.5,
              col = my_palette,
              xaxt = "n", yaxt = "n",
              breaks = c(seq(-10,0.99,length=5), # for imput$col1
                         #seq(0,86,1.15,length=1),  # for grey
                         seq(1.01,10,length=6)) # for input$col2
        )
        abline(v=seq(0, 1, length.out=length(names(dx)))+(0.5/length(names(dx))), col="grey3", lwd=1)
        abline(h=seq(0, 1, length.out=nrow(dx))+(0.5/nrow(dx)), col="grey3", lwd=1)
        axis(4, at=seq(0,1,length.out=nrow(dx)), labels=rev(rownames(fx)), cex.axis= 15/nrow(dx), las=2)
        #axis(3, at=seq(0,1,length.out=length(names(dx))), labels=names(dx), cex.axis= 20/length(names(dx)), las=3)
        dev.off()
      }
      else{
        image(imgflip(fx),
              #breaks = (1:(nlevels(ff2)+1))-.5,
              col = my_palette,
              xaxt = "n", yaxt = "n"
        )
        abline(v=seq(0, 1, length.out=length(names(dx)))+(0.5/length(names(dx))), col="grey3", lwd=1)
        abline(h=seq(0, 1, length.out=nrow(dx))+(0.5/nrow(dx)), col="grey3", lwd=1)
        axis(4, at=seq(0,1,length.out=nrow(dx)), labels=rev(rownames(fx)), cex.axis= 15/nrow(dx), las=2)
        #axis(3, at=seq(0,1,length.out=length(names(dx))), labels=names(dx), cex.axis= 20/length(names(dx)), las=3)
        dev.off()
      }  
  
      
     
    })
  
}


