if (!require("pacman")){install.packages('pacman')}

pacman::p_load(
  "shiny",
  "shinythemes",
  "tidyverse",
  "lmerTest",
  "parallel",
  "foreach",
  "doParallel",
  "waiter",
  "shinycssloaders"
)


# Define UI for easyEWAS app --------------------------------------------
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  # shinythemes::themeSelector(),
  # App title ----
  titlePanel(tags$h1("easyEWAS: Easily perform Epigenome-Wide Association Study", 
                     style = "font-weight: bold;")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      ## upload the data file ----
      tags$p("Step 1: Prepare the Data Files", 
             style = "font-size: 16px; font-weight: bold; color: #317EAC;"),
      ### Input1: sample data file ----
      fileInput("sample", "Choose Sample Data File (.csv or .xlsx)",
                multiple = TRUE,
                accept = c(".csv",".xlsx")),
      ### Input2: methylation file ----
      fileInput("methy", "Choose Methylation File (.csv or .xlsx)",
                multiple = TRUE,
                accept = c(".csv",".xlsx")),
      tags$hr(),
      
      
      ## transform data type ----
      tags$p("Step 2: Transform the Variables Type", 
             style = "font-size: 16px; font-weight: bold; color: #317EAC;"),
      textInput("facvar", "Enter the Variables to Convert to factor",
                value = ""),
      helpText("Note: If the exposure is a categorical variable, make sure it is converted to a factor type here."),
      textInput("numvar", "Enter the Variables to Convert to numeric",
                value = ""),
      helpText("Note: All variable names should be seperated by commas, e.g., var1,var2,var3"),
      
      tags$hr(),
      tags$p("Step 3: Peform the EWAS Analysis", 
             style = "font-size: 16px; font-weight: bold; color: #317EAC;"),
      
      ## peform the ewas ------
      radioButtons("model", "Choose the Model Type",
                   c("Linear Regression Model" = "lm",
                     "Linear Mixed-Effects Model" = "lmer",
                     "Cox Proportional Hazards Model" = "cox"),
                   selected = "lm"),
      # conditionalPanel(
      #   condition = "input.model == 'lmer'",
      #   textInput("expovar", "Enter the Exposure Variable", value = "")
      # ),
      conditionalPanel(
        condition = "input.model == 'lm' || input.model == 'lmer'",
        textInput("expovar", "Enter the Exposure Variable", value = ""),
      ),
      conditionalPanel(
        condition = "input.model == 'cox'",
        textInput("status", "Enter the Status Variable", value = "")
      ),
      conditionalPanel(
        condition = "input.model == 'cox'",
        textInput("time", "Enter the Time Variable", value = "")
      ),
      
      textInput("covvar", "Enter the Covariate Variable", value = ""),
      helpText("Note: Variable names should be seperated by commas, e.g., cov1,cov2,cov3"),
      
      conditionalPanel(
        condition = "input.model == 'lmer'",
        textInput("random", "Enter the Random Effects Term", value = "")
      ),
      radioButtons("adjust", "Whether to Correct P-values",
                   c("Yes" = TRUE,
                     "No" = FALSE),
                   selected = TRUE,
                   inline = TRUE),
      numericInput("core","Number of cores for parallel operations",
                   value = 1, min=1),
      helpText("Note: The value should not exceed the number of physical cores of the user's computer."),
      textInput("filename", "Enter the Output file name",
                value = ""),
      helpText("Note: Customize the name of the output result file, a combination of letters and numbers, e.g., out1"),
      tags$hr(),

      tags$p("Step 4: Submit and Download", 
             style = "font-size: 16px; font-weight: bold; color: #317EAC;"),
      actionButton("submit","Submit"),
      tags$br(),tags$br(),
      tags$p("All steps have been completed, please go to the RESULTS page and wait patiently.",
             style = "font-weight: bold;"),
      
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        
        #### Guideline----
        tabPanel("Guideline", 
                 tags$br(),
                 tags$h3("Welcome to the easyEWAS!", style = "font-size: 24px;font-weight: bold;"),
                 tags$p(
                   "easyEWAS, a Shiny app designed for individuals unfamiliar with DNA methylation analysis. 
                   With easyEWAS, users can effortlessly conduct differential methylation analysis using various 
                   models by simply providing sample data and methylation data. The app also supports parallel 
                   computing, significantly enhancing computational efficiency. Visit our website to learn more 
                   about how easyEWAS streamlines methylation analysis for researchers of all levels."
                 ),
                 
                 #### What to prepare
                 tags$h4("What to prepare before starting:",style = "font-weight: bold;"),
                 tags$span(tags$b("(1) Sample data: "), "Users need to prepare a sample file to store sample information, such as sample ID, exposure 
                   variables, covariates, etc. easyEWAS only supports .csv and .xlsx files, the sample format is as follows:"),
                 img(src = "sampledata.png", height = 200, width = 600, style = "display: block; margin: auto;"),
                 tags$p(
                   "Note: The first column must be the sample ID. And note that all column names should comply with common naming 
                   conventions. For example, the name cannot start with a number, and cannot contain spaces or special symbols.",
                   style = "color: brown;"
                 ),
                 tags$span(tags$b("(2) Methylation data: "), "Users also need to prepare a methylation file, requiring each row to represent a CpG site and each column 
                   to represent a sample. easyEWAS only supports .csv and .xlsx files, the sample format is as follows:"),
                 img(src = "methydata.png", height = 336, width = 800, style = "display: block; margin: auto;"),
                 tags$p(
                   "Note: The column name (i.e. sample ID) in the methylation file must be consistent with the sample ID of the sample data.",
                   style = "color: brown;"
                 ),
                 tags$br(),
                 
                 #### Analysis Workflow
                 tags$h4("Analysis Workflow:",style = "font-weight: bold;"),
                 tags$li("Step 1: Prepare the Data Files",style = "font-weight: bold; "),
                 tags$p(
                   "Upload your sample data and methylation files. Make sure to follow the format mentioned above. Currently only supports files under 20 MB."
                 ),
                 
                 tags$li("Step 2: Transform the Variables Type",style = "font-weight: bold; "),
                 tags$p(
                   "Convert variables to appropriate types for analysis. This step ensures that the data is formatted correctly for the analysis."
                 ),
                 tags$p(
                   "Note: If you select Linear Regression or Linear Mixed-Effects Model and your exposure variable is categorical, be sure to convert it to factor type here. 
                     Variables need to be separated by commas.",
                   style = "color: brown;"
                 ),
                 
                 
                 tags$li("Step 3: Perform the EWAS Analysis",style = "font-weight: bold; "),
                 tags$p(
                   "First, you need to choose the model you want. If you choose a linear regression model, enter the names of your exposed 
                   variables (only one) and the covariates (one or more, separated by commas); If you chose the linear mixed effects model, 
                   you will need to enter the name of the random effects term (only one). If you choose the Cox proportional risk model, 
                   you will need to provide the time and status variables. Finally, select whether to calculate the corrected P-value and 
                   specify the number of physical cores required for parallel operations.
                    "
                 ),
                 tags$p(
                   "Note: Be careful not to exceed the maximum number of cores supported by your device.",
                   style = "color: brown;"
                 ),
                 
                 tags$li("Step 4: Submit and Download",style = "font-weight: bold; "),
                 tags$span("After setting all parameters, click the ",tags$b("SUBMIT"), " button and Now you can go to the ",tags$b("RESULTS"),
                           " page and wait patiently, and when the results are displayed you can click ", tags$b("DOWNLOAD",".")),
                 
                 tags$br(),tags$br(),
                 
                 #### What Users Will Get:
                 tags$h4("What Users Will Get:",style = "font-weight: bold;"),
                 
                 tags$li("Linear Regression Model or Linear Mixed-Effects Model: For continuity variables, regression coefficient (per 1 unit, per SD, per IQR), 
                         standard error (per 1 unit, per SD, per IQR), significance P-value, and corrected P-value (FDR, Bofferoni). For categorical variables, 
                         regression coefficient, standard error, significance p-value and corrected P-value of each class relative to the reference class."),
                 tags$li("Cox proportional risk model: HR value, 95% confidence interval, significance P-value, and corrected P-value (FDR, Bofferoni) for each methylation site."),

                 tags$br(),
                 tags$p("Hope you have a good experience!",style = "font-weight: bold;"),
                 tags$hr(),
                 # tags$p("Hope you have a good experience!",style = "text-align: right; font-weight: bold;"),
                 tags$p("updated 02/24/2024", style = "text-align: right; font-weight: bold; ; color: #317EAC;"),
                 
        ),
        
        #### Results----
        tabPanel("Results", 
                 
                 # Output: Data file ----
                 shinycssloaders::withSpinner(tableOutput("ewasres")),
                 tags$br(),
                 tags$p("Please wait until the results are displayed here and click DOWNLOAD for full results.", 
                        style = "font-size: 14px; font-weight: bold; color: red;"),
                 tags$p("Note: Only the first 40 rows of results are shown here.",
                        style = "font-size: 14px; font-weight: bold; color: red;"),
                 
                 downloadButton("downloadData", "Download")
        ),
        
        #### More-----
        tabPanel("More", 
                 tags$br(),
                 tags$h4("Cite",style = "font-weight: bold;"),
                 tags$p(
                   "Please cite us as follows:"
                 ),
                 tags$br(),tags$br(),
                 
                 tags$h4("R package",style = "font-weight: bold;"),
                 tags$p(
                   "We have also developed the R package of the same name of easyEWAS, which also provides excellent result 
                   visualization functions. You can install it with the following code to learn more:"
                 ),
                 tags$code(
                   "devtools::install_github(\"ytwangZero/easyEWAS\")"
                 ),
                 tags$br(),tags$br(),
                
                 tags$h4("Contact",style = "font-weight: bold;"),
                 tags$p(
                   tags$p(
                     "If any suggestions or questions about easyEWAS, please send an email 
                     to ytwang@pku.edu.cn."
                   ),
                 ),
                 
        ),
        
        #### FAQs-----
        tabPanel("FAQs",
                 tags$br(),
                 tags$h3("runGitHub",style = "font-weight: bold;"),
                 tags$li("Q1: Error in initializePtr() : function 'cholmod_factor_ldetA' not provided by package 'Matrix'."),
                 tags$li("A1: It' because you are using 'old' Matrix package. Please updata it in your R studio."),
                 tags$code(
                   "install.packages(\"Matrix\")",
                 ),
        ),
      )
    )
    
  )
  
) 


options(shiny.maxRequestSize = 6000 * 1024^2)
server <- function(input, output, session) {
  
  datasetInput <- eventReactive(input$submit,{
    
    req(input$sample)
    req(input$methy)
    
    if(substr(input$sample$datapath,nchar(input$sample$datapath)-3,nchar(input$sample$datapath)) == ".csv"){
      sampledata = vroom::vroom(input$sample$datapath, delim = ",", show_col_types = FALSE) %>% as.data.frame()
    }else{
      sampledata = readxl::read_xlsx(input$sample$datapath) %>% as.data.frame()
    }
    
    if(substr(input$methy$datapath,nchar(input$methy$datapath)-3,nchar(input$methy$datapath)) == ".csv"){
      methydata = vroom::vroom(input$methy$datapath, delim = ",", show_col_types = FALSE) %>% as.data.frame()
    }else{
      methydata = readxl::read_xlsx(input$methy$datapath) %>% as.data.frame()
    }
    
    
    # transtype
    if(input$facvar == ""){
      facvars = input$facvar
    }
    if (!is.null(sampledata) & input$facvar != "") {
      facvars <- unlist(strsplit(input$facvar, ","))
      if(length(facvars) == 1){
        sampledata[, facvars] = factor(sampledata[, facvars])
      }else{
        sampledata[, facvars] <- lapply(sampledata[, facvars], as.factor)
      }
      
    }
    if(!is.null(sampledata) & input$numvar != ""){
      numvars <- unlist(strsplit(input$numvar, ","))
      if(length(numvars) == 1){
        sampledata[, numvars] <- as.numeric(sampledata[, numvars])
      }else{
        sampledata[, numvars] <- lapply(sampledata[, numvars], as.numeric)
      }
      
    }
    
    
    #### prepare ewas data---
    if(input$model == "lm"){
      if(input$covvar != ""){
        VarCov = unlist(strsplit(input$covvar, ","))
        covdata = sampledata[,c(input$expovar,VarCov)]
      }else{
        covdata = as.data.frame(sampledata[,c(input$expovar)])
        colnames(covdata)[1] = input$expovar
      }
      
    }else if(input$model == "lmer"){
      if(input$covvar == ""){
        covdata = sampledata[,c(input$expovar,input$random)]
      }else{
        VarCov = unlist(strsplit(input$covvar, ","))
        covdata = sampledata[,c(input$expovar,VarCov,input$random)]
      }
      random_index = which(colnames(covdata) == input$random)
      colnames(covdata)[random_index] = "random"
    }else{
      if(input$covvar == ""){
        covdata = sampledata[,c(input$time,input$status)]
      }else{
        VarCov = unlist(strsplit(input$covvar, ","))
        covdata = sampledata[,c(input$time,input$status,VarCov)]
      }
      time_index = which(colnames(covdata) == input$time)
      colnames(covdata)[time_index] = "time"
      status_index = which(colnames(covdata) == input$status)
      colnames(covdata)[status_index] = "status"
      
    }
    
    formula <- switch (input$model,
                       "lm" = as.formula( paste0("cpg ~ ",paste(colnames(covdata), collapse = " + "))),
                       "lmer" = as.formula( paste0("cpg ~ ",paste(colnames(covdata)[-random_index],
                                                                  collapse = " + "), " + (1 | random)")),
                       "cox" = as.formula( paste0("Surv(time, status) ~ cpg + ",
                                                  paste(VarCov, collapse = " + ")))
    )
    
    row = methydata[[1]]
    rownames(methydata) = row
    methydata %>%
      as.data.frame() %>%
      dplyr::select(sampledata[[1]]) -> methydata
    
    
    ####  define ewas function---
    if((input$expovar %in% facvars) & input$model %in% c("lm","lmer")){
      facnum = length(levels(sampledata[,input$expovar]))
      ewasfun <- switch (input$model,
                         "lm" = function(cg,ff,cov){
                           cov$cpg = as.vector(t(cg))
                           out <- base::summary(lm(ff, data = cov))
                           temp = c()
                           for (i in 2:facnum) {
                             temp = append(temp,as.vector(out$coefficients[i,c(1,2,4)]))
                           }
                           return(temp)
                         },
                         "lmer" = function(cg,ff,cov){
                           cov$cpg = as.vector(t(cg))
                           out <- base::summary(lmer(ff, data = cov))
                           temp = c()
                           for (i in 2:facnum) {
                             temp = append(temp,as.vector(out$coefficients[i,c(1,2,5)]))
                           }
                           return(temp)
                         }
      )
    }else{
      ewasfun <- switch (input$model,
                         "lm" = function(cg,ff,cov){
                           cov$cpg = as.vector(t(cg))
                           out <- base::summary(lm(ff, data = cov))
                           return(c(out$coefficients[2,1],
                                    out$coefficients[2,2],
                                    out$coefficients[2,4]))
                         },
                         "lmer" = function(cg,ff,cov){
                           cov$cpg = as.vector(t(cg))
                           out <- base::summary(lmer(ff, data = cov))
                           return(c(out$coefficients[2,1],
                                    out$coefficients[2,2],
                                    out$coefficients[2,5]))
                         }
      )
    }
    if(input$model == "cox"){
      ewasfun = function(cg,ff,cov){
        cov$cpg = as.vector(t(cg))
        out <- base::summary(coxph(ff, data = cov))
        return(c(as.vector(out$conf.int[1,c(1,3,4)]),
                 out$coefficients[1,5]))
      }
      
    }
    
    
    
    #### parallel calculating---
    # cores <- detectCores(logical=F)
    cl <- makeCluster(input$core)
    registerDoParallel(cl, cores=input$core)
    if(input$model == "lmer"){
      clusterEvalQ(cl, library(lme4))
      clusterEvalQ(cl, library(lmerTest))
    }
    if(input$model == "cox"){
      clusterEvalQ(cl, library(survival))
    }
    
    len = nrow(methydata)
    # split data by ourselves
    chunk.size <- len/input$core
    if(input$model %in% c("lm","lmer")){
      if(input$expovar  %in% facvars){
        system.time(
          modelres <- foreach(i=1:input$core, .combine='rbind') %dopar%
            {  # local data for results
              res <- matrix(0, nrow=ceiling(chunk.size), ncol=3*(facnum-1))
              for(x in ((i-1)*chunk.size+1):(i*chunk.size)) {
                res[x - (i-1)*chunk.size,] <- as.numeric(base::t(ewasfun(methydata[x,],formula,covdata)))
              }
              # return local results
              res
            }
        )
      }else{
        system.time(
          modelres <- foreach(i=1:input$core, .combine='rbind') %dopar%
            {  # local data for results
              res <- matrix(0, nrow=ceiling(chunk.size), ncol=3)
              for(x in ((i-1)*chunk.size+1):(i*chunk.size)) {
                res[x - (i-1)*chunk.size,] <- as.numeric(base::t(ewasfun(methydata[x,],formula,covdata)))
              }
              # return local results
              res
            }
        )
      }
      
    }else{
      system.time(
        modelres <- foreach(i=1:input$core, .combine='rbind') %dopar%
          {  # local data for results
            res <- matrix(0, nrow=ceiling(chunk.size), ncol=4)
            for(x in ((i-1)*chunk.size+1):(i*chunk.size)) {
              res[x - (i-1)*chunk.size,] <- as.numeric(base::t(ewasfun(methydata[x,],formula,covdata)))
            }
            # return local results
            res
          }
      )
      
    }
    
    stopImplicitCluster()
    stopCluster(cl)
    modelres = modelres[1:len,]
    
    
    if(input$model %in% c("lm","lmer")){
      if((input$expovar %in% facvars)){
        
        ### categorical variable-----
        modelres %>%
          as.data.frame() %>%
          purrr::set_names(paste(rep(c("BETA","SE","PVAL"),facnum-1),rep(1:(facnum-1),each = 3), sep = "_")) %>% 
          mutate(probe = rownames(methydata)) %>% 
          dplyr::select(probe, everything()) -> modelres
        
        #### FDR---
        if(input$adjust){
          FDRname = paste(rep(c("FDR","Bofferoni"),each = (facnum-1)),rep(1:(facnum-1),2), sep = "_")
          pindex = grep("PVAL",colnames(modelres))
          FDR = matrix(0,nrow = nrow(modelres),ncol = length(FDRname))
          for(i in pindex){
            FDR[,(i-1)/3] = p.adjust(modelres[[i]],method = "BH")
            FDR[,((i-1)/3)+(facnum-1)] = p.adjust(modelres[[i]],method = "bonferroni")
          }
          FDR = as.data.frame(FDR)
          FDR <- FDR %>% 
            purrr::set_names(paste(rep(c("FDR","Bofferoni"),each = (facnum-1)),rep(1:(facnum-1),2), sep = "_"))
          modelres = cbind(modelres,FDR)
          
          return(modelres)
        }else{
          return(modelres)
        }
        
      }else{
        
        ### continuous variable-----
        modelres %>%
          as.data.frame() %>%
          purrr::set_names("BETA","SE","PVAL") %>% 
          mutate(probe = rownames(methydata))-> modelres
        
        #### per SD & IQR---
        modelres$BETA_perSD = (modelres$BETA)*(sd(sampledata[[input$expovar]],na.rm = TRUE))
        modelres$BETA_perIQR = (modelres$BETA)*(IQR(sampledata[[input$expovar]],na.rm = TRUE))
        modelres$SE_perSD = (modelres$SE)*(sd(sampledata[[input$expovar]],na.rm = TRUE))
        modelres$SE_perIQR = (modelres$SE)*(IQR(sampledata[[input$expovar]],na.rm = TRUE))
        modelres %>% 
          dplyr::select(probe,BETA,BETA_perSD,BETA_perIQR,SE,SE_perSD,SE_perIQR,PVAL) -> modelres
        
        #### FDR---
        if(input$adjust){
          modelres$FDR = p.adjust(modelres$PVAL, method = "BH")
          modelres$Bofferoni = p.adjust(modelres$PVAL,method = "bonferroni")
          
          return(modelres)
        }else{
          return(modelres)
        }
        
        
      }
    }
    if(input$model == "cox"){
      modelres %>%
        as.data.frame() %>%
        purrr::set_names("HR","LOWER_95%","UPPER_95%","PVAL") %>% 
        mutate(probe = rownames(methydata)) %>% 
        dplyr::select(probe, everything())-> modelres
      
      #### FDR---
      if(input$adjust){
        modelres$FDR = p.adjust(modelres$PVAL, method = "BH")
        modelres$Bofferoni = p.adjust(modelres$PVAL,method = "bonferroni")
        
        return(modelres)
      }else{
        return(modelres)
      }
      
      
    }
    
    
  })
  
  
  output$ewasres <- renderTable({
    if(nrow(datasetInput())<41){
      return(datasetInput())
    }else{
      return(datasetInput()[1:40,])
    }
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      if(input$filename == ""){
        "result.csv"
      }else{
        paste(input$filename, ".csv", sep = "")
      }
      
    },
    content = function(file) {
      vroom::vroom_write(datasetInput(), file, ",")
    }
  )
  
}





# Create Shiny app ----------------------
shinyApp(ui, server)


