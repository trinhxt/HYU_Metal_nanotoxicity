library(caret) 
library(randomForest)
library(data.table)
library(shiny)
library(shinythemes)
library(shinydashboard)

#Load data and trained model
load("www/train1.Rdata")
load("www/test1.Rdata")
load("www/RFmodel1.Rdata")
load("www/Performance1.Rdata")

# UI of the application
#----------------------------------------------------------------------------------------------------------
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "QSAR for nanomaterials", titleWidth = 300),
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(
                        menuItem("QSAR Metal NPs", tabName = "Model1", icon = icon("desktop")),
                        menuItem("Acknowledgement", tabName = "Acknowledgement", icon = icon("far fa-file-pdf"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        # Model 1 tab content
                        #----------------------------------------------------------------------------------------------------------      
                        tabItem(tabName = "Model1",
                                
                                column(
                                  width = 12,
                                  p(strong("Predicting cytotoxicity of metal nanoparticles (i.e., Ag and Au)"),
                                    style="font-size:30px;
                          text-align:justify;
                          color:black;
                          background-color:papayawhip;
                          padding:15px;
                          border-radius:10px"),
                                  p(strong("Summary of model:"), "This model predicts death level (below or above 50%) of human cells (i.e., HeLa, A549, BEAS-2B and HepG2) exposed to gold or silver nanoparticles. 
                  When users input data by choosing values of parameters in the left panel (Model input), 
                  the model will predict death level and show in the right panel (Model output). 
                  Death level above 50% means the nanoparticles sample is toxic (red label) to cell. 
                  Otherwise, the sample is non-toxic (green label) to cell. 
                  It is based on random forest algorithm. It is built by using package \"caret\" in R.
                  The number trees in the random forest was set to 100.",
                                    style="font-size:16px;
                          text-align:justify;
                          color:black;
                          background-color:white;
                          padding:15px;
                          border-radius:10px")
                                ),
                                
                                
                                # Show Model input and output
                                fluidRow(
                                  # Show Model input
                                  box(
                                    width = 6,
                                    height = 1350,
                                    title = p(strong("Model input:"),
                                              style="font-size:24px;
                          text-align:justify;
                          color:black;
                          background-color:papayawhip;
                          padding:15px;
                          border-radius:10px"),
                                    p(strong("User input data:"),
                                      style="font-size:20px;
                          text-align:justify;
                          color:black;
                          background-color:white;
                          padding:15px;
                          border-radius:10px"),
                                    selectInput("Metal", "Metal NPs:",
                                                c("Ag" = "Ag",
                                                  "Au" = "Au")),
                                    selectInput("Shape", "Shape:",
                                                c("Sphere" = "Sphere",
                                                  "Nanorod" = "Nanorod",
                                                  "Hollow" = "Hollow")),
                                    sliderInput("CoreSize", "Core diameter (nm):", 1, 100, 10),
                                    sliderInput("HSize", "Hydrodynamic diameter (nm):", 1, 300, 50),
                                    sliderInput("Zeta", "Zeta potential (mV):", -20, 20, -5),
                                    selectInput("Cell_line", "Cell line:",
                                                c("HeLa" = "HeLa",
                                                  "HepG2" = "HepG2",
                                                  "BEAS-2B" = "BEAS-2B",
                                                  "A549" = "A549")),
                                    selectInput("Assay", "Toxicity assay method:",
                                                c("MTS" = "MTS",
                                                  "MTT" = "MTT",
                                                  "AlamarBlue" = "Alamar Blue",
                                                  "NRU" = "NRU")),
                                    sliderInput("Time", "Exposure time (h):", 1, 96, 72),
                                    sliderInput("Dose", "Concentration (ug/L):", 1, 10^3, 200),
                                    p(strong("Summary of input:"),
                                      style="font-size:16px;
                          text-align:justify;
                          color:black;
                          background-color:white;
                          padding:15px;
                          border-radius:10px"),
                                    tableOutput("SummaryInput1")
                                  ),
                                  
                                  # Show Model output
                                  box(
                                    width = 6,
                                    height = 1350,
                                    title = p(strong("Model output:"),
                                              style="font-size:24px;
                          text-align:justify;
                          color:black;
                          background-color:papayawhip;
                          padding:15px;
                          border-radius:10px"),
                                    
                                    p(strong("Predicted toxicity:"),
                                      style="font-size:20px;
                          text-align:justify;
                          color:black;
                          background-color:white;
                          padding:15px;
                          border-radius:10px"),
                                    valueBoxOutput("Prediction1", width = 12),
                                    
                                    column(
                                      width = 12,
                                      p(strong("Performance of the predictive model:"),
                                        style="font-size:16px;
                          text-align:justify;
                          color:black;
                          background-color:white;
                          padding:15px;
                          border-radius:10px"),
                                      tableOutput("Performance1")
                                    ),
                                  ),
                                  
                                  column(
                                    br(),
                                    p("Dataset for model development was published in:", 
                                      strong("Trinh et al., Curation of datasets, assessment of their quality and completeness, 
                         and nanoSAR classification model development for metallic nanoparticles. 
                         Environmental Science: Nano 5.8 (2018): 1902-1910."),
                                      style="font-size:16px;
                        text-align:justify;
                        color:black;
                        background-color:papayawhip;
                        padding:15px;border-radius:10px"),
                                    width=12),
                                  
                                ),
                                
                                
                        ),
                        
                        
                        
                        # Acknowledgement tab content
                        #----------------------------------------------------------------------------------------------------------
                        tabItem(tabName = "Acknowledgement",
                                column(
                                  width = 12,
                                  p(strong("Acknowledgement"),
                                    style="font-size:30px;
                          text-align:justify;
                          color:black;
                          background-color:papayawhip;
                          padding:15px;
                          border-radius:10px"),
                                  p(strong("The authors appreciate the NCEC members (Hanyang University, Seoul: Mr. Jinbae Kim, Mr. Jonghoon Park and Ms. Nuri Yang), the staff of KIT (Korea Institute of Toxicology, Daejeon; Ms. Soojin Kim, Dr. Junghwa Oh, and Dr. Seokjoo Yoon) and other collaborators in the S2NANO community (Safe and Sustainable Nanotechnology, www.s2nano.org) for their contributions to nanosafety data collection."),
                                    style="font-size:16px;
                          text-align:justify;
                          color:black;
                          background-color:white;
                          padding:15px;
                          border-radius:10px")
                                ),
                        )
                        
                      )
                    )
)

# Server of the application
#----------------------------------------------------------------------------------------------------------
server <- function(input, output) {
  
  ## Model 1
  output$Performance1 <- renderTable({
    Perf1 <- data.frame("Parameter" = Performance1[,1],
                        "Value (%)" = Performance1[,2]*100)
    colnames(Perf1) <- c("Parameter", "Value (%)")
    Perf1
  }, digits = 1)
  
  output$SummaryInput1 <- renderTable({
    data.frame("Parameter" = c("Metal NPs","Core size (nm)", "Hydrodynamic diameter (nm)", "Zeta potential (mV)","Cell line","Toxic Assay","Exposure time (h)","Concentration (ug/L)"),
               "Value" = c(input$Metal,input$CoreSize,input$HSize, input$Zeta,input$Cell_line,input$Assay,input$Time,input$Dose))
  }, digits = 2)
  
  output$Prediction1 <- renderValueBox({
    Cell_Tissue <- fifelse(input$Cell_line == "HepG2", "Liver", 
                           fifelse(input$Cell_line == "HeLa", "Cervix",
                                   fifelse(input$Cell_line == "BEAS-2B", "Lung","Lung"))) 
    table1 <- data.frame("Toxicity" = "UNKNOWN",
                         "Dose" = input$Dose,
                         "Assay" = input$Assay,
                         "Time" = input$Time,
                         "Species" = "Human",
                         "Cancer" = 1,
                         "Cell_Tissue" = Cell_Tissue,
                         "Cell_line" = input$Cell_line,
                         "SSA" = 20,
                         "Zeta" = input$Zeta,
                         "HSize" = input$HSize,
                         "CoreSize" = input$CoreSize,
                         "Coating" = "None",
                         "Shape" = "Sphere",
                         "Metal" = input$Metal)
    table2 <- as.data.frame(predict(RFmodel1, table1))
    boxcolor <- if(table2[1,1] == "TOXIC"){"red"} else {"green"}
    boxicon <- if(table2[1,1] == "TOXIC"){"exclamation-triangle"} else {"fas fa-envira"}
    subtitle <- if(table2[1,1] == "TOXIC"){paste("More than 50% of ",input$Cell_line, " cells might die", sep = "")} 
    else {paste("Less than 50% of ",input$Cell_line, " cells might die", sep = "")}
    toxicvalue <- if(table2[1,1] == "TOXIC"){"TOXIC"} else {"non TOXIC"}
    valueBox(toxicvalue, subtitle, icon = icon(boxicon), color = boxcolor, width = 12)
  })
  
  
  
  
  
  
}

#----------------------------------------------------------------------------------------------------------
shinyApp(server = server, ui = ui)