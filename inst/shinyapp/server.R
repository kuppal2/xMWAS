options(shiny.maxRequestSize=100*1024^2)
options(shiny.sanitize.errors=FALSE)
# Server logic
server <- function(input, output) {
  
  ##################################  Introduction Page #################################################  
  
  ##################################  Analysis Page #################################################
  vals <- reactiveValues(count = 0)
  done <- reactiveValues(count = 0)
  go<- reactiveValues(count = 0)
  observeEvent(input$exampledata,
               {output$siderbar<-renderUI({
                 if(input$exampledata=="TRUE"){
                   sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 5,  value = 0))
                 }else{
                   if(vals$count==0){
                     sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 3,  value = 0))
                   }else{
                     if(vals$count==1){
                       sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 5,  value = 0))
                     }else{
                       sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 8,  value = 0))
                     }
                   }
                 }
               })})
  observeEvent(input$add,
               {if(vals$count<2){vals$count=vals$count+1}
                 if(vals$count==1 | vals$count==2){
                   output$input_dataC<-renderUI({
                     column(12,tags$div(column(width=6,  
                                               fileInput("datasetC", "Input file for dataset C ('.csv' or '.txt', 100MB limit)",
                                                         multiple = FALSE,
                                                         width="350px",
                                                         accept = c("text/csv",
                                                                    "text/comma-separated-values,text/plain",
                                                                    ".txt"))
                     ),
                     column(width=6,  
                            textInput("Cname", "Name for dataset C:", value="",placeholder="datasetC")
                     )))
                   })
                   output$spls_dataC<-renderUI({
                     numericInput(width="380px","keepZ", "Maximum #datasetC variables to select in sPLS (change according to your dataset):", 1000, min = 1, max = 100000)
                   })
                   output$rsd_dataC<-renderUI({
                     numericInput(width="380px","max_zvar", "Maximum #datasetC variables to select based on RSD (change according to your dataset):", 10000, min = 1, max = 1000000)
                   })
                   if(vals$count==1){
                       output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 5,  value = 0))})
                   }else{
                     if(input$exampledata=="FALSE"){
                       output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 8,  value = 0))})
                     }else{
                       output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 5,  value = 0))})
                     }
                   }
                 }else{
                   output$input_dataC<-renderUI({})
                   output$spls_dataC<-renderUI({})
                   output$rsd_dataC<-renderUI({})
                   if(input$exampledata=="FALSE"){
                     output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 3,  value = 0))})
                   }else{
                     output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 5,  value = 0))})  
                   }
                 }
                 if(vals$count==2){
                   output$input_dataD<-renderUI({
                     column(12,tags$div(column(width=6,  
                                               fileInput("datasetD", "Input file for dataset D ('.csv' or '.txt', 100MB limit)",
                                                         multiple = FALSE,
                                                         width="350px",
                                                         accept = c("text/csv",
                                                                    "text/comma-separated-values,text/plain",
                                                                    ".txt"))
                     ),
                     column(width=6,  
                            textInput("Dname", "Name for dataset D:", value="",placeholder="datasetD")
                     )))
                   })
                   output$spls_dataD<-renderUI({
                     numericInput(width="380px","keepW", "Maximum #datasetD variables to select in sPLS (change according to your dataset):", 1000, min = 1, max = 100000)
                   })
                   output$rsd_dataD<-renderUI({
                     numericInput(width="380px","max_wvar", "Maximum #datasetD variables to select based on RSD (change according to your dataset):", 10000, min = 1, max = 1000000)
                   })
                   if(input$exampledata=="FALSE"){
                     output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 8,  value = 0))})
                   }else{
                     output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 5,  value = 0))})
                   }
                 }else{
                   output$input_dataD<-renderUI({})
                   output$spls_dataD<-renderUI({})
                   output$rsd_dataD<-renderUI({})
                   if(vals$count==1){
                       output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 5,  value = 0))})
                   }else{
                     if(input$exampledata=="FALSE"){
                       output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 3,  value = 0))})
                     }else{
                       output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 5,  value = 0))})
                     }
                   }
                 }
               })
  observeEvent(input$delete,
               {if(vals$count!=0){vals$count=vals$count-1};
                 if(vals$count==1 | vals$count==2){
                   output$input_dataC<-renderUI({
                     column(12,tags$div(column(width=6,  
                                               fileInput("datasetC", "Input file for dataset C ('.csv' or '.txt', 100MB limit)",
                                                         multiple = FALSE,
                                                         width="350px",
                                                         accept = c("text/csv",
                                                                    "text/comma-separated-values,text/plain",
                                                                    ".txt"))
                     ),
                     column(width=6,  
                            textInput("Cname", "Name for dataset C:", value="",placeholder="datasetC")
                     )))
                   })
                   output$spls_dataC<-renderUI({
                     numericInput(width="380px","keepZ", "Maximum #datasetC variables to select in sPLS (change according to your dataset):", 1000, min = 1, max = 100000)
                   })
                   output$rsd_dataC<-renderUI({
                     numericInput(width="380px","max_zvar", "Maximum #datasetC variables to select based on RSD (change according to your dataset):", 10000, min = 1, max = 1000000)
                   })
                   if(vals$count==1){
                       output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 5,  value = 0))})  
                  }else{
                     if(input$exampledata=="FALSE"){
                       output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 8,  value = 0))})
                     }else{
                       output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 5,  value = 0))})  
                     }
                   }
                 }else{
                   output$input_dataC<-renderUI({})
                   output$spls_dataC<-renderUI({})
                   output$rsd_dataC<-renderUI({})
                   if(input$exampledata=="FALSE"){
                     output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 3,  value = 0))})
                   }else{
                     output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 5,  value = 0))})
                   }
                 }
                 if(vals$count==2){
                   output$input_dataD<-renderUI({
                     column(12,tags$div(column(width=6,  
                                               fileInput("datasetD", "Input file for dataset D ('.csv' or '.txt', 100MB limit)",
                                                         multiple = FALSE,
                                                         width="350px",
                                                         accept = c("text/csv",
                                                                    "text/comma-separated-values,text/plain",
                                                                    ".txt"))
                     ),
                     column(width=6,  
                            textInput("Dname", "Name for dataset D:", value="",placeholder="datasetD")
                     )))
                   })
                   output$spls_dataD<-renderUI({
                     numericInput(width="380px","keepW", "Maximum #datasetD variables to select in sPLS (change according to your dataset):", 1000, min = 1, max = 100000)
                   })
                   output$rsd_dataD<-renderUI({
                     numericInput(width="380px","max_wvar", "Maximum #datasetD variables to select based on RSD (change according to your dataset):", 10000, min = 1, max = 1000000)
                   })
                   if(input$exampledata=="FALSE"){
                     output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 8,  value = 0))})
                   }else{
                     output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 5,  value = 0))})  
                   }
                 }else{
                   output$input_dataD<-renderUI({})
                   output$spls_dataD<-renderUI({})
                   output$rsd_dataD<-renderUI({})
                   if(vals$count==1){
                       output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 5,  value = 0))})  
                   }else{
                     if(input$exampledata=="FALSE"){
                       output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 3,  value = 0))})
                     }else{
                       output$siderbar<-renderUI({sidebarPanel(sliderInput("obs", "Slide to go to next figure:", min = 0, max = 5,  value = 0))}) 
                     }
                   }
                 }
               })
  
  ##############################################################
  
  observeEvent(input$go, 
               {
                 if(vals$count==0 & input$exampledata=="FALSE"){
                   output$nText2 <- renderText({validate(
                     need(input$datasetA, "No datasetA provided. Please upload dataset A in 'Choose Files'."),
                     need(input$datasetA$type, "The format of datasetA is not correct. Please upload the file with correct format."),
                     need(input$datasetB, "No datasetB provided. Please upload dataset B in 'Choose Files'."),
                     need(input$datasetB$type, "The format of datasetB is not correct. Please upload the file with correct format."),
                     need(input$class_labels_file, "No class labels file provided. Please upload class labels file in 'Choose Files'."),
                     need(input$class_labels_file$type, "The format of class labels file is not correct. Please upload the file with correct format.")
                   )})
                   validate(
                     need(input$datasetA, "No datasetA provided. Please upload dataset A in 'Choose Files'."),
                     need(input$datasetA$type, "The format of datasetA is not correct. Please upload the file with correct format."),
                     need(input$datasetB, "No datasetB provided. Please upload dataset B in 'Choose Files'."),
                     need(input$datasetB$type, "The format of datasetB is not correct. Please upload the file with correct format."),
                     need(input$class_labels_file, "No class labels file provided. Please upload class labels file in 'Choose Files'."),
                     need(input$class_labels_file$type, "The format of class labels file is not correct. Please upload the file with correct format.")
                   )
                   showNotification("Starting processing now. Your results will be available for download shortly. The processing time depends on the number of variables. Please use the data filtering options to reduce the run time.", duration=600)
                   
                 }else{
                   if(vals$count==1 & input$exampledata=="FALSE"){
                     output$nText2 <- renderText({validate(
                       need(input$datasetA, "No datasetA provided. Please upload dataset A in 'Choose Files'."),
                       need(input$datasetA$type, "The format of datasetA is not correct. Please upload the file with correct format."),
                       need(input$datasetB, "No datasetB provided. Please upload dataset B in 'Choose Files'."),
                       need(input$datasetB$type, "The format of datasetB is not correct. Please upload the file with correct format."),
                       need(input$datasetC, "No datasetC provided. Please upload dataset C in 'Choose Files'."),
                       need(input$datasetC$type, "The format of datasetC is not correct. Please upload the file with correct format."),
                       need(input$class_labels_file, "No class labels file provided. Please upload class labels file in 'Choose Files'."),
                       need(input$class_labels_file$type, "The format of class labels file is not correct. Please upload the file with correct format.")
                     )})
                     validate(
                       need(input$datasetA, "No datasetA provided. Please upload dataset A in 'Choose Files'."),
                       need(input$datasetA$type, "The format of datasetA is not correct. Please upload the file with correct format."),
                       need(input$datasetB, "No datasetB provided. Please upload dataset B in 'Choose Files'."),
                       need(input$datasetB$type, "The format of datasetB is not correct. Please upload the file with correct format."),
                       need(input$datasetC, "No datasetC provided. Please upload dataset C in 'Choose Files'."),
                       need(input$datasetC$type, "The format of datasetC is not correct. Please upload the file with correct format."),
                       need(input$class_labels_file, "No class labels file provided. Please upload class labels file in 'Choose Files'."),
                       need(input$class_labels_file$type, "The format of class labels file is not correct. Please upload the file with correct format.")
                     )
                     showNotification("Starting processing now. Your results will be available for download shortly. The processing time depends on the number of variables. Please use the data filtering options to reduce the run time.", duration=600)
                     
                   }else{
                     if(vals$count==2 & input$exampledata=="FALSE"){
                       output$nText2 <- renderText({validate(
                         need(input$datasetA, "No datasetA provided. Please upload dataset A in 'Choose Files'."),
                         need(input$datasetA$type, "The format of datasetA is not correct. Please upload the file with correct format."),
                         need(input$datasetB, "No datasetB provided. Please upload dataset B in 'Choose Files'."),
                         need(input$datasetB$type, "The format of datasetB is not correct. Please upload the file with correct format."),
                         need(input$datasetC, "No datasetC provided. Please upload dataset C in 'Choose Files'."),
                         need(input$datasetC$type, "The format of datasetC is not correct. Please upload the file with correct format."),
                         need(input$datasetD, "No datasetD provided. Please upload dataset D in 'Choose Files'."),
                         need(input$datasetD$type, "The format of datasetD is not correct. Please upload the file with correct format."),
                         need(input$class_labels_file, "No class labels file provided. Please upload class labels file in 'Choose Files'."),
                         need(input$class_labels_file$type, "The format of class labels file is not correct. Please upload the file with correct format.")
                       )})
                       validate(
                         need(input$datasetA, "No datasetA provided. Please upload dataset A in 'Choose Files'."),
                         need(input$datasetA$type, "The format of datasetA is not correct. Please upload the file with correct format."),
                         need(input$datasetB, "No datasetB provided. Please upload dataset B in 'Choose Files'."),
                         need(input$datasetB$type, "The format of datasetB is not correct. Please upload the file with correct format."),
                         need(input$datasetC, "No datasetC provided. Please upload dataset C in 'Choose Files'."),
                         need(input$datasetC$type, "The format of datasetC is not correct. Please upload the file with correct format."),
                         need(input$datasetD, "No datasetD provided. Please upload dataset D in 'Choose Files'."),
                         need(input$datasetD$type, "The format of datasetD is not correct. Please upload the file with correct format."),
                         need(input$class_labels_file, "No class labels file provided. Please upload class labels file in 'Choose Files'."),
                         need(input$class_labels_file$type, "The format of class labels file is not correct. Please upload the file with correct format.")
                       )
                       showNotification("Starting processing now. Your results will be available for download shortly. The processing time depends on the number of variables. Please use the data filtering options to reduce the run time.", duration=600)
                     }else{
                       if(input$exampledata=="TRUE" & is.null(input$datasetA$name) & is.null(input$datasetB$name) & 
                          is.null(input$datasetC$name) & is.null(input$datasetD$name) & is.null(input$class_labels_file$name)){
                         showNotification("You're using example (NCI60) data and it is processing now. Your results will be available for download shortly (approx. 10 minutes).", duration=600)
                       }else{
                         showNotification("You selected TRUE for 'use example data'. You're not allowed to process both the example data and your datasets in the same time. Please select FALSE for 'use example data' if you want to analyze your datasets.", duration=15)
                       }
                     }
                   }
                 }
                 
               })
  
  
  
  #########################################
  
  session_outloc <- reactive({
    if(( input$go!=0  & vals$count==0 & input$exampledata=="FALSE" & !is.null(input$datasetA$name) & !is.null(input$datasetB$name) & !is.null(input$class_labels_file$name)) &
       (input$datasetA$type=="text/plain" || input$datasetA$type=="text/csv") &
       (input$datasetB$type=="text/plain" || input$datasetB$type=="text/csv") &
       (input$class_labels_file$type=="text/plain" || input$class_labels_file$type=="text/csv")){
      cur_date<-Sys.time()
      cur_date<-gsub(x=cur_date,pattern="-",replacement="")
      cur_date<-gsub(x=cur_date,pattern=":",replacement="")
      cur_date<-gsub(x=cur_date,pattern=" ",replacement="")
      if(input$outloc==""){
        outloc<-paste('~/xmwasresults',cur_date,sep="")
      }else{
        outloc<-paste('~/',input$outloc,cur_date,sep="")
      }
      outloc
    }else{
      if((input$go!=0  & vals$count==1 & input$exampledata=="FALSE" & !is.null(input$datasetA$name) & !is.null(input$datasetB$name) & !is.null(input$datasetC$name) & !is.null(input$class_labels_file$name)) &
         (input$datasetA$type=="text/plain" || input$datasetA$type=="text/csv") &
         (input$datasetB$type=="text/plain" || input$datasetB$type=="text/csv") &
         (input$datasetC$type=="text/plain" || input$datasetC$type=="text/csv") &
         (input$class_labels_file$type=="text/plain" || input$class_labels_file$type=="text/csv")){
        cur_date<-Sys.time()
        cur_date<-gsub(x=cur_date,pattern="-",replacement="")
        cur_date<-gsub(x=cur_date,pattern=":",replacement="")
        cur_date<-gsub(x=cur_date,pattern=" ",replacement="")
        if(input$outloc==""){
          outloc<-paste('~/xmwasresults',cur_date,sep="")
        }else{
          outloc<-paste('~/',input$outloc,cur_date,sep="")
        }
        outloc
      }else{
        if((input$go!=0  & vals$count==2 & input$exampledata=="FALSE" & !is.null(input$datasetA$name) & !is.null(input$datasetB$name) & !is.null(input$datasetC$name) & !is.null(input$datasetD$name) & !is.null(input$class_labels_file$name)) &
           (input$datasetA$type=="text/plain" || input$datasetA$type=="text/csv") &
           (input$datasetB$type=="text/plain" || input$datasetB$type=="text/csv") &
           (input$datasetC$type=="text/plain" || input$datasetC$type=="text/csv") &
           (input$datasetD$type=="text/plain" || input$datasetD$type=="text/csv") &
           (input$class_labels_file$type=="text/plain" || input$class_labels_file$type=="text/csv")){
          cur_date<-Sys.time()
          cur_date<-gsub(x=cur_date,pattern="-",replacement="")
          cur_date<-gsub(x=cur_date,pattern=":",replacement="")
          cur_date<-gsub(x=cur_date,pattern=" ",replacement="")
          if(input$outloc==""){
            outloc<-paste('~/xmwasresults',cur_date,sep="")
          }else{
            outloc<-paste('~/',input$outloc,cur_date,sep="")
          }
          outloc
        }else{
          if(input$go!=0  & input$exampledata=="TRUE" & is.null(input$datasetA$name) & is.null(input$datasetB$name) & 
             is.null(input$datasetC$name) & is.null(input$datasetD$name) & is.null(input$class_labels_file$name)){
            cur_date<-Sys.time()
            cur_date<-gsub(x=cur_date,pattern="-",replacement="")
            cur_date<-gsub(x=cur_date,pattern=":",replacement="")
            cur_date<-gsub(x=cur_date,pattern=" ",replacement="")
            if(input$outloc==""){
              outloc<-paste('~/xmwasresults',cur_date,sep="")
            }else{
              outloc<-paste('~/',input$outloc,cur_date,sep="")
            }
            outloc
          }else{
            NULL
          }
        }
      }
    }
  })
  
  
  
  ##########################################
  
  #output$test<-renderPrint({c(vals$count,session_outloc())})
  
  observeEvent(input$go,{go$count=1})
  
  output$nText <- renderText({
    if((input$go!=0  & !is.null(go$count) & vals$count==0 & input$exampledata=="FALSE" & !is.null(input$datasetA$name) & !is.null(input$datasetB$name) & !is.null(input$class_labels_file$name)) &
       (input$datasetA$type=="text/plain" || input$datasetA$type=="text/csv") &
       (input$datasetB$type=="text/plain" || input$datasetB$type=="text/csv") &
       (input$class_labels_file$type=="text/plain" || input$class_labels_file$type=="text/csv")){
      req(input$datasetA)
      if(input$datasetA$type=="text/plain"){
        xMat <- read.table(input$datasetA$datapath,header = TRUE,sep = "\t")
        rownames(xMat)<-xMat[,1]
        xMat<-xMat[,-c(1)]
        xMat<-as.data.frame(xMat)
      }else{
        if(input$datasetA$type=="text/csv"){
          xMat <- read.table(input$datasetA$datapath,header = TRUE,sep = ",")
          rownames(xMat)<-xMat[,1]
          xMat<-xMat[,-c(1)]
          xMat<-as.data.frame(xMat)
        }else{
          # output warnings
        }
      }
      if(input$Aname==""){
        Xname="datasetA"
      }else{
        Xname=input$Aname
      }
      req(input$datasetB)
      if(input$datasetB$type=="text/plain"){
        yMat <- read.table(input$datasetB$datapath,header = TRUE,sep = "\t")
        rownames(yMat)<-yMat[,1]
        yMat<-yMat[,-c(1)]
        yMat<-as.data.frame(yMat)
      }else{
        if(input$datasetB$type=="text/csv"){
          yMat <- read.table(input$datasetB$datapath,header = TRUE,sep = ",")
          rownames(yMat)<-yMat[,1]
          yMat<-yMat[,-c(1)]
          yMat<-as.data.frame(yMat)
        }else{
          # output warnings
        }
      }
      if(input$Bname==""){
        Yname="datasetB"
      }else{
        Yname=input$Bname
      }
      req(input$class_labels_file)
      if(input$class_labels_file$type=="text/plain"){
        class_labels_file <- read.table(input$class_labels_file$datapath,header = TRUE,sep = "\t")
      }else{
        if(input$class_labels_file$type=="text/csv"){
          class_labels_file <- read.table(input$class_labels_file$datapath,header = TRUE,sep = ",")
        }else{
          # output warnings
        }
      }
      
      req(input$xmwasmethod)
      
   if(input$xmwasmethod=="PLS: Partial least squares regression"){
              xmwasmethod="pls"
              KeepX=1000
              KeepY=1000
              KeepZ=1000
	            KeepW=1000
            }else{
              if(input$xmwasmethod=="sPLS: sparse PLS"){
                xmwasmethod="spls"
                KeepX=round(input$keepX)
                KeepY=round(input$keepY)
                check_keepZ<-reactive({need(input$keepZ,"error")})
                if(is.null(check_keepZ())){
                  KeepZ=round(input$keepZ)
                }else{
                  KeepZ=1000
                }
		check_KeepW<-reactive({need(input$keepW,"error")})
              	if(is.null(check_KeepW())){
                  KeepW=round(input$keepW)
              	}else{
                  KeepW=1000
                }
              }else{
                  if(input$xmwasmethod=="OPLS: orthogonal PLS"){
                      
                      xmwasmethod="o1pls"
                  }
                  else{
                      if(input$xmwasmethod=="RCC: regularized canonical correlation"){
                          xmwasmethod="rcc"
                      }
                  }
                  KeepX=1000
                  KeepY=1000
                  KeepZ=1000
                  KeepW=1000
              }
            }
            
            max_xvar=round(input$max_xvar)
            max_yvar=round(input$max_yvar)
            check_max_zvar<-reactive({need(input$max_zvar,"error")})
            if(is.null(check_max_zvar())){
              max_zvar=round(input$max_zvar)
            }else{
              max_zvar=1000
            }
	    check_max_wvar<-reactive({need(input$max_wvar,"error")})
            if(is.null(check_max_wvar())){
              max_wvar=round(input$max_wvar)
            }else{
              max_wvar=1000
            }


      maxnodesperclass=1000000 #round(input$maxnodesperclass)
      numcomps=round(input$numcomps)
      
      max_connections=round(input$max_connections)     

      if(max_connections<0){
		max_connections<-NA
	}

	missing.val=input$missing.val

      if(missing.val=="0"){
		missing.val=0
	}else{
		missing.val=NA
	}	
      ###################
      
      library('xMWAS')
      #source("xMWAS_Rcode/xMWAS_v0.0.22.R")
      
      xmwas_res<-run_xmwas(Xome_data=xMat,Yome_data=yMat,Zome_data=NA,Wome_data=NA,outloc=session_outloc(),
                           classlabels=class_labels_file,xmwasmethod=xmwasmethod,plsmode=input$plsmode,
                           max_xvar=max_xvar,max_yvar=max_yvar,max_zvar=max_zvar,max_wvar=max_wvar,rsd.filt.thresh=input$rsd_filt_thresh,corthresh=input$corthresh,
                           keepX=KeepX,keepY=KeepY,keepZ=KeepZ,keepW=KeepW,pairedanalysis=input$pairedanalysis,optselect=input$optselect,rawPthresh=input$rawPthresh,
                           numcomps=numcomps,net_edge_colors=c("blue","red"),net_node_colors=c("orange", "green","cyan","gold"),
                           Xname=Xname,Yname=Yname,Zname=NA,Wname=NA,net_node_shape=c(input$net_node_shapeA,input$net_node_shapeB,input$net_node_shapeC,input$net_node_shapeD),
                           all.missing.thresh=input$all_missing_thresh,maxnodesperclass=maxnodesperclass,seednum=input$seednum,label.cex=input$label_cex,vertex.size=input$vertex_size,graphclustering=TRUE,
                           interactive=FALSE,max_connections=max_connections,centrality_method=input$centrality_method,use.X.reference=input$use_X_reference,removeRda=TRUE,compare.classes=input$compare.classes,missing.val=missing.val,
                           modularity.weighted=TRUE,
                           globalcomparison=TRUE,
                           plot.pairwise=TRUE,apply.sparse.class.comparison=FALSE,layout.type="fr1")
                           
                           



      done$count=1
      #file.copy(paste(getwd(),'matrix_centrality.txt',sep='/'),session_outloc())
      setwd(session_outloc())
      zip(zipfile=paste(basename(session_outloc()),'zip',sep='.'), files='.')
      print("Processing complete. Please click on Download to save the results.")
      
      
    }else{
      if((input$go!=0  & !is.null(go$count) & vals$count==1 & input$exampledata=="FALSE" & !is.null(input$datasetA$name) & !is.null(input$datasetB$name) & !is.null(input$datasetC$name) & !is.null(input$class_labels_file$name)) &
         (input$datasetA$type=="text/plain" || input$datasetA$type=="text/csv") &
         (input$datasetB$type=="text/plain" || input$datasetB$type=="text/csv") &
         (input$datasetC$type=="text/plain" || input$datasetC$type=="text/csv") &
         (input$class_labels_file$type=="text/plain" || input$class_labels_file$type=="text/csv")){
        req(input$datasetA)
        if(input$datasetA$type=="text/plain"){
          xMat <- read.table(input$datasetA$datapath,header = TRUE,sep = "\t")
          rownames(xMat)<-xMat[,1]
          xMat<-xMat[,-c(1)]
          xMat<-as.data.frame(xMat)
        }else{
          if(input$datasetA$type=="text/csv"){
            xMat <- read.table(input$datasetA$datapath,header = TRUE,sep = ",")
            rownames(xMat)<-xMat[,1]
            xMat<-xMat[,-c(1)]
            xMat<-as.data.frame(xMat)
          }else{
            # output warnings
          }
        }
        if(input$Aname==""){
          Xname="datasetA"
        }else{
          Xname=input$Aname
        }
        req(input$datasetB)
        if(input$datasetB$type=="text/plain"){
          yMat <- read.table(input$datasetB$datapath,header = TRUE,sep = "\t")
          rownames(yMat)<-yMat[,1]
          yMat<-yMat[,-c(1)]
          yMat<-as.data.frame(yMat)
        }else{
          if(input$datasetB$type=="text/csv"){
            yMat <- read.table(input$datasetB$datapath,header = TRUE,sep = ",")
            rownames(yMat)<-yMat[,1]
            yMat<-yMat[,-c(1)]
            yMat<-as.data.frame(yMat)
          }else{
            # output warnings
          }
        }
        if(input$Bname==""){
          Yname="datasetB"
        }else{
          Yname=input$Bname
        }
        req(input$datasetC)
        if(input$datasetC$type=="text/plain"){
          zMat <- read.table(input$datasetC$datapath,header = TRUE,sep = "\t")
          rownames(zMat)<-zMat[,1]
          zMat<-zMat[,-c(1)]
          zMat<-as.data.frame(zMat)
        }else{
          if(input$datasetC$type=="text/csv"){
            zMat <- read.table(input$datasetC$datapath,header = TRUE,sep = ",")
            rownames(zMat)<-zMat[,1]
            zMat<-zMat[,-c(1)]
            zMat<-as.data.frame(zMat)
          }else{
            # output warnings
          }
        }
        if(input$Cname==""){
          Zname="datasetC"
        }else{
          Zname=input$Cname
        }
        req(input$class_labels_file)
        if(input$class_labels_file$type=="text/plain"){
          class_labels_file <- read.table(input$class_labels_file$datapath,header = TRUE,sep = "\t")
        }else{
          if(input$class_labels_file$type=="text/csv"){
            class_labels_file <- read.table(input$class_labels_file$datapath,header = TRUE,sep = ",")
          }else{
            # output warnings
          }
        }
        
        req(input$xmwasmethod)
        
           if(input$xmwasmethod=="PLS: Partial least squares regression"){
              xmwasmethod="pls"
              KeepX=1000
              KeepY=1000
              KeepZ=1000
	            KeepW=1000
            }else{
              if(input$xmwasmethod=="sPLS: sparse PLS"){
                    xmwasmethod="spls"
                    KeepX=round(input$keepX)
                    KeepY=round(input$keepY)
                    check_keepZ<-reactive({need(input$keepZ,"error")})
                    if(is.null(check_keepZ())){
                      KeepZ=round(input$keepZ)
                    }else{
                      KeepZ=1000
                    }
                    check_KeepW<-reactive({need(input$keepW,"error")})
                    if(is.null(check_KeepW())){
                      KeepW=round(input$keepW)
                    }else{
                      KeepW=1000
                    }
              }else{
                
                if(input$xmwasmethod=="OPLS: orthogonal PLS"){
                
                    xmwasmethod="o1pls"
                }
                else{
                    if(input$xmwasmethod=="RCC: regularized canonical correlation"){
                        xmwasmethod="rcc"
                    }
                }
                KeepX=1000
                KeepY=1000
                KeepZ=1000
                KeepW=1000
              }
            }
            
            max_xvar=round(input$max_xvar)
            max_yvar=round(input$max_yvar)
            check_max_zvar<-reactive({need(input$max_zvar,"error")})
            if(is.null(check_max_zvar())){
              max_zvar=round(input$max_zvar)
            }else{
              max_zvar=10000
            }
	    check_max_wvar<-reactive({need(input$max_wvar,"error")})
            if(is.null(check_max_wvar())){
              max_wvar=round(input$max_wvar)
            }else{
              max_wvar=10000
            }

#maxnodesperclass=round(input$maxnodesperclass)
        maxnodesperclass=1000000
        numcomps=round(input$numcomps)
        
        # c(max_zvar,session_outloc(),Xname,Yname,Zname,dim(xMat[1:5,1:5]),dim(yMat[1:5,1:5]),dim(zMat[1:5,1:5]),dim(class_labels_file[1:2,1:2]))
        
	max_connections=round(input$max_connections)	

	if(max_connections<0){
                max_connections<-NA
        }

	 missing.val=input$missing.val

      if(missing.val=="0"){
                missing.val=0
        }else{
                missing.val=NA
        } 
        ###################
        
        
        library('xMWAS')
        
        xmwas_res<-run_xmwas(Xome_data=xMat,Yome_data=yMat,Zome_data=zMat,Wome_data=NA,outloc=session_outloc(),
                             classlabels=class_labels_file,xmwasmethod=xmwasmethod,plsmode=input$plsmode,
                             max_xvar=max_xvar,max_yvar=max_yvar,max_zvar=max_zvar,max_wvar=max_wvar,rsd.filt.thresh=input$rsd_filt_thresh,corthresh=input$corthresh,
                             keepX=KeepX,keepY=KeepY,keepZ=KeepZ,keepW=KeepW,pairedanalysis=input$pairedanalysis,optselect=input$optselect,rawPthresh=input$rawPthresh,
                             numcomps=numcomps,net_edge_colors=c("blue","red"),net_node_colors=c("orange", "green","cyan","gold"),
                             Xname=Xname,Yname=Yname,Zname=Zname,Wname=NA,net_node_shape=c(input$net_node_shapeA,input$net_node_shapeB,input$net_node_shapeC,input$net_node_shapeD),
                             all.missing.thresh=input$all_missing_thresh,maxnodesperclass=maxnodesperclass,seednum=input$seednum,label.cex=input$label_cex,vertex.size=input$vertex_size,graphclustering=TRUE,
                             interactive=FALSE,max_connections=max_connections,centrality_method=input$centrality_method,use.X.reference=input$use_X_reference,removeRda=TRUE,compare.classes=input$compare.classes,missing.val=missing.val,
                             modularity.weighted=TRUE,
                             globalcomparison=TRUE,
                             plot.pairwise=TRUE,apply.sparse.class.comparison=FALSE,layout.type="fr1")

if(FALSE){
		xmwas_res<-run_xmwas(Xome_data=xMat,Yome_data=yMat,Zome_data=zMat,Wome_data=NA,outloc=session_outloc(),
classlabels=class_labels_file,class_fname=NA,xmwasmethod="spls",plsmode="canonical",max_xvar=1000,max_yvar=1000,
max_zvar=1000,max_wvar=1000,rsd.filt.thresh=1,corthresh=0.8,keepX=100,keepY=100,keepZ=100,keepW=100,
pairedanalysis=FALSE,optselect=TRUE,rawPthresh=0.05,numcomps=10,net_edge_colors=c("blue","red"),
net_node_colors=c("orange", "green","cyan","gold"),Xname="X",Yname="Y",Zname="Z",Wname="W",
net_node_shape=c(input$net_node_shapeA,input$net_node_shapeB,input$net_node_shapeC,input$net_node_shapeD),all.missing.thresh=0.3,maxnodesperclass=100,
seednum=100,label.cex=0.2,vertex.size=6,graphclustering=TRUE,interactive=FALSE,max_connections=2000,
centrality_method="eigenvector",use.X.reference=FALSE,removeRda=TRUE,compare.classes=FALSE)
}

        done$count=1
        #file.copy(paste(getwd(),'matrix_centrality.txt',sep='/'),session_outloc())
        setwd(session_outloc())
        zip(zipfile=paste(basename(session_outloc()),'zip',sep='.'), files='.')
        print("Processing complete. Please click on Download to save the results.")
        
        
      }else{
        if((input$go!=0  & !is.null(go$count) & vals$count==2 & input$exampledata=="FALSE" & !is.null(input$datasetA$name) & !is.null(input$datasetB$name) & !is.null(input$datasetC$name) & !is.null(input$datasetD$name) & !is.null(input$class_labels_file$name)) &
           (input$datasetA$type=="text/plain" || input$datasetA$type=="text/csv") &
           (input$datasetB$type=="text/plain" || input$datasetB$type=="text/csv") &
           (input$datasetC$type=="text/plain" || input$datasetC$type=="text/csv") &
           (input$datasetD$type=="text/plain" || input$datasetD$type=="text/csv") &
           (input$class_labels_file$type=="text/plain" || input$class_labels_file$type=="text/csv")){
          req(input$datasetA)
          if(input$datasetA$type=="text/plain"){
            xMat <- read.table(input$datasetA$datapath,header = TRUE,sep = "\t")
            rownames(xMat)<-xMat[,1]
            xMat<-xMat[,-c(1)]
            xMat<-as.data.frame(xMat)
          }else{
            if(input$datasetA$type=="text/csv"){
              xMat <- read.table(input$datasetA$datapath,header = TRUE,sep = ",")
              rownames(xMat)<-xMat[,1]
              xMat<-xMat[,-c(1)]
              xMat<-as.data.frame(xMat)
            }else{
              # output warnings
            }
          }
          if(input$Aname==""){
            Xname="datasetA"
          }else{
            Xname=input$Aname
          }
          req(input$datasetB)
          if(input$datasetB$type=="text/plain"){
            yMat <- read.table(input$datasetB$datapath,header = TRUE,sep = "\t")
            rownames(yMat)<-yMat[,1]
            yMat<-yMat[,-c(1)]
            yMat<-as.data.frame(yMat)
          }else{
            if(input$datasetB$type=="text/csv"){
              yMat <- read.table(input$datasetB$datapath,header = TRUE,sep = ",")
              rownames(yMat)<-yMat[,1]
              yMat<-yMat[,-c(1)]
              yMat<-as.data.frame(yMat)
            }else{
              # output warnings
            }
          }
          if(input$Bname==""){
            Yname="datasetB"
          }else{
            Yname=input$Bname
          }
          req(input$datasetC)
          if(input$datasetC$type=="text/plain"){
            zMat <- read.table(input$datasetC$datapath,header = TRUE,sep = "\t")
            rownames(zMat)<-zMat[,1]
            zMat<-zMat[,-c(1)]
            zMat<-as.data.frame(zMat)
          }else{
            if(input$datasetC$type=="text/csv"){
              zMat <- read.table(input$datasetC$datapath,header = TRUE,sep = ",")
              rownames(zMat)<-zMat[,1]
              zMat<-zMat[,-c(1)]
              zMat<-as.data.frame(zMat)
            }else{
              # output warnings
            }
          }
          if(input$Cname==""){
            Zname="datasetC"
          }else{
            Zname=input$Cname
          }
          req(input$datasetD)
          if(input$datasetD$type=="text/plain"){
            wMat <- read.table(input$datasetD$datapath,header = TRUE,sep = "\t")
            rownames(wMat)<-wMat[,1]
            wMat<-wMat[,-c(1)]
            wMat<-as.data.frame(wMat)
          }else{
            if(input$datasetD$type=="text/csv"){
              wMat <- read.table(input$datasetD$datapath,header = TRUE,sep = ",")
              rownames(wMat)<-wMat[,1]
              wMat<-wMat[,-c(1)]
              wMat<-as.data.frame(wMat)
            }else{
              # output warnings
            }
          }
          if(input$Dname==""){
            Wname="datasetD"
          }else{
            Wname=input$Dname
          }
          req(input$class_labels_file)
          if(input$class_labels_file$type=="text/plain"){
            class_labels_file <- read.table(input$class_labels_file$datapath,header = TRUE,sep = "\t")
          }else{
            if(input$class_labels_file$type=="text/csv"){
              class_labels_file <- read.table(input$class_labels_file$datapath,header = TRUE,sep = ",")
            }else{
              # output warnings
            }
          }
          
          req(input$xmwasmethod)
          
          if(input$xmwasmethod=="PLS: Partial least squares regression"){
            xmwasmethod="pls"
            KeepX=1000
            KeepY=1000
            KeepZ=1000
            KeepW=1000
          }else{
            if(input$xmwasmethod=="sPLS: sparse PLS"){
              xmwasmethod="spls"
              KeepX=round(input$keepX)
              KeepY=round(input$keepY)
              check_keepZ<-reactive({need(input$keepZ,"error")})
              if(is.null(check_keepZ())){
                KeepZ=round(input$keepZ)
              }else{
                KeepZ=1000
              }
              check_KeepW<-reactive({need(input$keepW,"error")})
              if(is.null(check_KeepW())){
                KeepW=round(input$keepW)
              }else{
                KeepW=1000
              }
            }else{
                if(input$xmwasmethod=="OPLS: orthogonal PLS"){
                    
                    xmwasmethod="o1pls"
                }
                else{
                    if(input$xmwasmethod=="RCC: regularized canonical correlation"){
                        xmwasmethod="rcc"
                    }
                }
                KeepX=1000
                KeepY=1000
                KeepZ=1000
                KeepW=1000
            }
          }
          
          #c(max_zvar,session_outloc(),Xname,Yname,Zname,dim(xMat[1:5,1:5]),dim(yMat[1:5,1:5]),dim(zMat[1:5,1:5]),dim(class_labels_file[1:2,1:2]))
          
          max_xvar=round(input$max_xvar)
          max_yvar=round(input$max_yvar)
          check_max_zvar<-reactive({need(input$max_zvar,"error")})
          if(is.null(check_max_zvar())){
            max_zvar=round(input$max_zvar)
          }else{
            max_zvar=1000
          }
          check_max_wvar<-reactive({need(input$max_wvar,"error")})
          if(is.null(check_max_wvar())){
            max_wvar=round(input$max_wvar)
          }else{
            max_wvar=1000
          }
          #maxnodesperclass=round(input$maxnodesperclass)
          maxnodesperclass=1000000
	  numcomps=round(input$numcomps)
          
	  max_connections=round(input$max_connections)
         
          if(max_connections<0){
                max_connections<-NA
	        }
	   missing.val=input$missing.val

      if(missing.val=="0"){
                missing.val=0
        }else{
                missing.val=NA
        } 
          ###################
          
          
          library('xMWAS')
         
          xmwas_res<-run_xmwas(Xome_data=xMat,Yome_data=yMat,Zome_data=zMat,Wome_data=wMat,outloc=session_outloc(),
                               classlabels=class_labels_file,xmwasmethod=xmwasmethod,plsmode=input$plsmode,
                               max_xvar=max_xvar,max_yvar=max_yvar,max_zvar=max_zvar,max_wvar=max_wvar,rsd.filt.thresh=input$rsd_filt_thresh,corthresh=input$corthresh,
                               keepX=KeepX,keepY=KeepY,keepZ=KeepZ,keepW=KeepW,pairedanalysis=input$pairedanalysis,optselect=input$optselect,rawPthresh=input$rawPthresh,
                               numcomps=numcomps,net_edge_colors=c("blue","red"),net_node_colors=c("orange", "green","cyan","gold"),
                               Xname=Xname,Yname=Yname,Zname=Zname,Wname=Wname,net_node_shape=c(input$net_node_shapeA,input$net_node_shapeB,input$net_node_shapeC,input$net_node_shapeD),
                               all.missing.thresh=input$all_missing_thresh,maxnodesperclass=maxnodesperclass,seednum=input$seednum,label.cex=input$label_cex,vertex.size=input$vertex_size,graphclustering=TRUE,
                               interactive=FALSE,max_connections=max_connections,centrality_method=input$centrality_method,use.X.reference=input$use_X_reference,removeRda=TRUE,compare.classes=input$compare.classes,missing.val=missing.val,
                               modularity.weighted=TRUE,
                               globalcomparison=TRUE,
                               plot.pairwise=TRUE,apply.sparse.class.comparison=FALSE,layout.type="fr1")

if(FALSE){
xmwas_res<-run_xmwas(Xome_data=xMat,Yome_data=yMat,Zome_data=zMat,Wome_data=NA,outloc=session_outloc(),
classlabels=class_labels_file,class_fname=NA,xmwasmethod="spls",plsmode="canonical",max_xvar=1000,max_yvar=1000,
max_zvar=1000,max_wvar=1000,rsd.filt.thresh=1,corthresh=0.8,keepX=100,keepY=100,keepZ=100,keepW=100,
pairedanalysis=FALSE,optselect=TRUE,rawPthresh=0.05,numcomps=10,net_edge_colors=c("blue","red"),
net_node_colors=c("orange", "green","cyan","gold"),Xname="X",Yname="Y",Zname="Z",Wname="W",
net_node_shape=c(input$net_node_shapeA,input$net_node_shapeB,input$net_node_shapeC,input$net_node_shapeD),all.missing.thresh=0.3,maxnodesperclass=100,
seednum=100,label.cex=0.2,vertex.size=6,graphclustering=TRUE,interactive=FALSE,max_connections=2000,
centrality_method="eigenvector",use.X.reference=FALSE,removeRda=TRUE,compare.classes=FALSE)
}
          done$count=1
          #file.copy(paste(getwd(),'matrix_centrality.txt',sep='/'),session_outloc())
          setwd(session_outloc())
          zip(zipfile=paste(basename(session_outloc()),'zip',sep='.'), files='.')
          print("Processing complete. Please click on Download to save the results.")
          
        }else{
          if((input$go!=0  & !is.null(go$count) & input$exampledata=="TRUE" & is.null(input$datasetA$name) & is.null(input$datasetB$name) & 
              is.null(input$datasetC$name)) &  is.null(input$datasetD$name) & is.null(input$class_labels_file$name)){
            library('xMWAS')
            #source("xMWAS_Rcode/xMWAS_v0.0.22.R")
            
            data(exnci60)
            xMat<-exnci60$mrna
            yMat<-exnci60$miRNA
            zMat<-exnci60$prot
            class_labels_file<-exnci60$classlabels
            
            Xname="mrna"
            Yname="miRNA"
            Zname="prot"
            
            req(input$xmwasmethod)
            
            if(input$xmwasmethod=="PLS: Partial least squares regression"){
              xmwasmethod="pls"
              KeepX=1000
              KeepY=1000
              KeepZ=1000
	      KeepW=1000
            }else{
              if(input$xmwasmethod=="sPLS: sparse PLS"){
                xmwasmethod="spls"
                KeepX=round(input$keepX)
                KeepY=round(input$keepY)
                check_keepZ<-reactive({need(input$keepZ,"error")})
                if(is.null(check_keepZ())){
                  KeepZ=round(input$keepZ)
                }else{
                  KeepZ=1000
                }
		check_KeepW<-reactive({need(input$keepW,"error")})
              	if(is.null(check_KeepW())){
                  KeepW=round(input$keepW)
              	}else{
                  KeepW=1000
                }
              }else{
                  if(input$xmwasmethod=="OPLS: orthogonal PLS"){
                      
                      xmwasmethod="o1pls"
                  }
                  else{
                      if(input$xmwasmethod=="RCC: regularized canonical correlation"){
                          xmwasmethod="rcc"
                      }
                  }
                  KeepX=1000
                  KeepY=1000
                  KeepZ=1000
                  KeepW=1000
              }
            }
            
            max_xvar=round(input$max_xvar)
            max_yvar=round(input$max_yvar)
            check_max_zvar<-reactive({need(input$max_zvar,"error")})
            if(is.null(check_max_zvar())){
              max_zvar=round(input$max_zvar)
            }else{
              max_zvar=1000
            }
	    check_max_wvar<-reactive({need(input$max_wvar,"error")})
            if(is.null(check_max_wvar())){
              max_wvar=round(input$max_wvar)
            }else{
              max_wvar=1000
            }
            maxnodesperclass=1000000 #round(input$maxnodesperclass)
            numcomps=round(input$numcomps)
            
            # c(max_zvar,session_outloc(),Xname,Yname,Zname,dim(xMat[1:5,1:5]),dim(yMat[1:5,1:5]),dim(zMat[1:5,1:5]),dim(class_labels_file[1:2,1:2]))
            
            max_connections=round(input$max_connections)
            if(max_connections<0){
                max_connections<-NA
            }
	     missing.val=input$missing.val

      if(missing.val=="0"){
                missing.val=0
        }else{
                missing.val=NA
        } 
            ###################
            
            #xmwas_res<-run_xmwas(Xome_data=xMat,Yome_data=yMat,Zome_data=zMat,Wome_data=NA,outloc=session_outloc(),
            #                     classlabels=class_labels_file,xmwasmethod=xmwasmethod,plsmode=input$plsmode,
            #                     max_xvar=max_xvar,max_yvar=max_yvar,max_zvar=max_zvar,max_wvar=NA,rsd.filt.thresh=input$rsd_filt_thresh,corthresh=input$corthresh,
            #                     keepX=KeepX,keepY=KeepY,keepZ=KeepZ,keepW=NA,pairedanalysis=input$pairedanalysis,optselect=input$optselect,rawPthresh=input$rawPthresh,
            #                     numcomps=numcomps,net_edge_colors=c("blue","red"),net_node_colors=c("orange", "green","cyan","gold"),
            #                     Xname=Xname,Yname=Yname,Zname=Zname,Wname=NA,net_node_shape=c(input$net_node_shapeA,input$net_node_shapeB,input$net_node_shapeC,input$net_node_shapeD),
            #                     all.missing.thresh=input$all_missing_thresh,maxnodesperclass=maxnodesperclass,seednum=input$seednum,label.cex=input$label_cex,vertex.size=input$vertex_size,graphclustering=TRUE,
            #                     interactive=input$interactive,max_connections=max_connections,centrality_method=input$centrality_method,use.X.reference=input$use_X_reference,removeRda=input$removeRda)      

           xmwas_res<-run_xmwas(Xome_data=xMat,Yome_data=yMat,Zome_data=zMat,Wome_data=NA,outloc=session_outloc(),
				classlabels=class_labels_file,xmwasmethod=xmwasmethod,plsmode=input$plsmode,
				max_xvar=max_xvar,max_yvar=max_yvar,max_zvar=max_zvar,max_wvar=max_wvar,rsd.filt.thresh=input$rsd_filt_thresh,corthresh=input$corthresh,
				keepX=KeepX,keepY=KeepY,keepZ=KeepZ,keepW=KeepW,pairedanalysis=input$pairedanalysis,optselect=input$optselect,rawPthresh=input$rawPthresh,
				numcomps=numcomps,net_edge_colors=c("blue","red"), net_node_colors=c("orange","green","cyan","gold"),
				Xname=Xname,Yname=Yname,Zname=Zname,Wname=NA, net_node_shape=c(input$net_node_shapeA,input$net_node_shapeB,input$net_node_shapeC,input$net_node_shapeD),
				all.missing.thresh=input$all_missing_thresh,maxnodesperclass=maxnodesperclass,seednum=input$seednum,label.cex=input$label_cex,
				vertex.size=input$vertex_size,graphclustering=TRUE,interactive=FALSE,max_connections=max_connections,
                centrality_method=input$centrality_method,use.X.reference=input$use_X_reference,removeRda=TRUE,compare.classes=input$compare.classes,missing.val=missing.val,modularity.weighted=TRUE,
                globalcomparison=TRUE,
                plot.pairwise=TRUE,apply.sparse.class.comparison=FALSE,layout.type="fr1")
  
if(FALSE){ 
xmwas_res<-run_xmwas(Xome_data=xMat,Yome_data=yMat,Zome_data=zMat,Wome_data=NA,outloc=session_outloc(),
classlabels=class_labels_file,class_fname=NA,xmwasmethod="spls",plsmode="canonical",max_xvar=1000,max_yvar=1000,
max_zvar=1000,max_wvar=1000,rsd.filt.thresh=1,corthresh=0.8,keepX=100,keepY=100,keepZ=100,keepW=100,
pairedanalysis=FALSE,optselect=TRUE,rawPthresh=0.05,numcomps=10,net_edge_colors=c("blue","red"),
net_node_colors=c("orange", "green","cyan","gold"),Xname="X",Yname="Y",Zname="Z",Wname="W",
net_node_shape=c(input$net_node_shapeA,input$net_node_shapeB,input$net_node_shapeC,input$net_node_shapeD),all.missing.thresh=0.3,maxnodesperclass=100,
seednum=100,label.cex=0.2,vertex.size=6,graphclustering=TRUE,interactive=FALSE,max_connections=2000,
centrality_method="eigenvector",use.X.reference=FALSE,removeRda=TRUE,compare.classes=FALSE)
}
 
            done$count=1
            #file.copy(paste(getwd(),'matrix_centrality.txt',sep='/'),session_outloc())
            setwd(session_outloc())
            zip(zipfile=paste(basename(session_outloc()),'zip',sep='.'), files='.')
            print("Processing complete. Please click on Download to save the results.")
          }else{
            go$count=NULL
          }
        }
      }
    }
    
  })
  
  ##########################################
  
  observeEvent({if(done$count==1) TRUE else return()},{
    output$myImage <- renderImage({
      
      req(input$obs)
      
      l1<-list.files(session_outloc(),".png",recursive=TRUE,full.names=FALSE)
      if(length(grep("Rplot",l1,perl=TRUE))==0){
        
      }else{
        l1<-l1[-grep("Rplot",l1,perl=TRUE)]
      
      }
      l1<-l1[grep("Multidata",l1,perl=TRUE)]
      if(input$obs>0 && input$obs<=length(l1))
      {
        
        filename <- normalizePath(file.path(session_outloc(),l1[input$obs]))
        list(src = filename,width=600,height=600,
             alt = "This is an image")
        
      }else{
        filename <- normalizePath(file.path(session_outloc(),l1[length(l1)]))
        list(src = filename,width=600,height=600,
             alt = "This is an image")
        
      }
      
      
    }, deleteFile = FALSE)
  })
  
  
  output$downloadData <- downloadHandler(
    
    #if(input$go!=0 && input$featselmethod!="-" && input$feature_table_file!="" && input$class_labels_file!=""){
    
    filename <- function() {
      paste(basename(session_outloc()), "zip", sep=".")
    },
    content <- function(file) {
      fname1<-paste(session_outloc(),"/",basename(session_outloc()), ".zip", sep="")
      file.copy(fname1, file)
    },
    contentType = "application/zip"
    #}
  )
  
  ##################################  Help Page #################################################  
  
  #output$table_dataset <- renderPrint({
  
  #library(htmlTable)
  #library('xMWAS')        
  #data(exnci60)        
  
  #output<-exnci60$mrna[1:5,1:6]
  #output<-htmlTable(output,align='c',caption="<strong>Dataset File Format:</strong>",css.table="width:100%;")
  #output
  
  #})
  
} 
