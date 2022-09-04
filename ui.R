library(shiny) 
library(shinyhelper) 
library(data.table) 
library(Matrix) 
library(DT) 
library(magrittr) 
sc1conf = readRDS("sc1conf.rds")
sc1def  = readRDS("sc1def.rds")



### Start server code 
shinyUI(fluidPage( 
### HTML formatting of error messages 
 
tags$head(tags$style(HTML(".shiny-output-error-validation {color: red; font-weight: bold;}"))), 
list(tags$style(HTML(".navbar-default .navbar-nav { font-weight: bold; font-size: 16px; }"))), 
 
   
### Page title 
titlePanel("Dual-color labeling and scRNA-seq revealed dental follicle niche cells directly contributed to tooth reconstitution and morphogenesis"),  
br(),
navbarPage( 
  NULL,  
 ### Tab1.a1: cellInfo vs geneExpr on dimRed 
  tabPanel( 
    HTML("CellInfo & GeneExpr"), 
    h4("E14.5 Cell information and gene expression on reduced dimensions"), 
    "In this tab, users can visualise both cell information and gene ",  
    "expression side-by-side on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("sc1a1drX", "X-axis:", choices = sc1conf[dimred == TRUE]$UI, 
                           selected = sc1def$dimred[1]), 
            selectInput("sc1a1drY", "Y-axis:", choices = sc1conf[dimred == TRUE]$UI, 
                        selected = sc1def$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("sc1a1togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.sc1a1togL % 2 == 1", 
          selectInput("sc1a1sub1", "Cell information to subset:", 
                      choices = sc1conf[grp == TRUE]$UI, 
                      selected = sc1def$grp1), 
          uiOutput("sc1a1sub1.ui"), 
          actionButton("sc1a1sub1all", "Select all groups", class = "btn btn-primary"), 
          actionButton("sc1a1sub1non", "Deselect all groups", class = "btn btn-primary") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("sc1a1tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.sc1a1tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("sc1a1siz", "Point size:", 
                             min = 0, max = 4, value = 1.25, step = 0.25), 
              radioButtons("sc1a1psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Medium", inline = TRUE), 
              radioButtons("sc1a1fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Medium", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("sc1a1asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("sc1a1txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information"), 
        fluidRow( 
          column( 
            6, selectInput("sc1a1inp1", "Cell information:", 
                           choices = sc1conf$UI, 
                           selected = sc1def$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("sc1a1tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.sc1a1tog1 % 2 == 1", 
              radioButtons("sc1a1col1", "Colour (Continuous data):", 
                           choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("sc1a1ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("sc1a1lab1", "Show cell info labels", value = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("sc1a1oup1.ui"))), 
        downloadButton("sc1a1oup1.pdf", "Download PDF"), 
        downloadButton("sc1a1oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("sc1a1oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("sc1a1oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)), br(), 
        actionButton("sc1a1tog9", "Toggle to show cell numbers / statistics"), 
        conditionalPanel( 
          condition = "input.sc1a1tog9 % 2 == 1", 
          h4("Cell numbers / statistics"), 
          radioButtons("sc1a1splt", "Split continuous cell info into:", 
                       choices = c("Quartile", "Decile"), 
                       selected = "Decile", inline = TRUE), 
          dataTableOutput("sc1a1.dt") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression"), 
        fluidRow( 
          column( 
            6, selectInput("sc1a1inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("sc1a1tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.sc1a1tog2 % 2 == 1", 
              radioButtons("sc1a1col2", "Colour:", 
                           choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "White-Red"), 
              radioButtons("sc1a1ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ) , 
        fluidRow(column(12, uiOutput("sc1a1oup2.ui"))), 
        downloadButton("sc1a1oup2.pdf", "Download PDF"), 
        downloadButton("sc1a1oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("sc1a1oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("sc1a1oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
 ### Tab1.b2: Gene coexpression plot 
 tabPanel( 
   HTML("Gene coexpression"), 
   h4("E14.5 Coexpression of two genes on reduced dimensions"), 
   "In this tab, users can visualise the coexpression of two genes ", 
   "on low-dimensional representions.", 
   br(),br(), 
   fluidRow( 
     column( 
       3, h4("Dimension Reduction"), 
       fluidRow( 
         column( 
           12, selectInput("sc1b2drX", "X-axis:", choices = sc1conf[dimred == TRUE]$UI, 
                           selected = sc1def$dimred[1]), 
           selectInput("sc1b2drY", "Y-axis:", choices = sc1conf[dimred == TRUE]$UI, 
                       selected = sc1def$dimred[2])) 
       ) 
     ), # End of column (6 space) 
     column( 
       3, actionButton("sc1b2togL", "Toggle to subset cells"), 
       conditionalPanel( 
         condition = "input.sc1b2togL % 2 == 1", 
         selectInput("sc1b2sub1", "Cell information to subset:", 
                     choices = sc1conf[grp == TRUE]$UI, 
                    selected = sc1def$grp1), 
         uiOutput("sc1b2sub1.ui"), 
         actionButton("sc1b2sub1all", "Select all groups", class = "btn btn-primary"), 
         actionButton("sc1b2sub1non", "Deselect all groups", class = "btn btn-primary") 
       ) 
     ), # End of column (6 space) 
     column( 
       6, actionButton("sc1b2tog0", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.sc1b2tog0 % 2 == 1", 
         fluidRow( 
           column( 
             6, sliderInput("sc1b2siz", "Point size:", 
                            min = 0, max = 4, value = 1.25, step = 0.25), 
             radioButtons("sc1b2psz", "Plot size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Medium", inline = TRUE), 
             radioButtons("sc1b2fsz", "Font size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Medium", inline = TRUE) 
           ), 
           column( 
             6, radioButtons("sc1b2asp", "Aspect ratio:", 
                             choices = c("Square", "Fixed", "Free"), 
                             selected = "Square", inline = TRUE), 
             checkboxInput("sc1b2txt", "Show axis text", value = FALSE) 
           ) 
         ) 
       ) 
     )  # End of column (6 space) 
   ),   # End of fluidRow (4 space) 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", h4("Gene Expression"), 
       selectInput("sc1b2inp1", "Gene 1:", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
               title = "Gene expression to colour cells by", 
               content = c("Select gene to colour cells by gene expression", 
                          paste0("- Gene expression are coloured in a ", 
                                 "White-Red colour scheme which can be ", 
                                 "changed in the plot controls"))), 
       selectInput("sc1b2inp2", "Gene 2:", choices=NULL) %>% 
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Gene expression to colour cells by", 
                content = c("Select gene to colour cells by gene expression", 
                            paste0("- Gene expression are coloured in a ", 
                                   "White-Blue colour scheme which can be ", 
                                   "changed in the plot controls"))), 
       actionButton("sc1b2tog1", "Toggle plot controls"), 
       conditionalPanel( 
         condition = "input.sc1b2tog1 % 2 == 1", 
         radioButtons("sc1b2col1", "Colour:", 
                      choices = c("Red (Gene1); Blue (Gene2)", 
                                  "Orange (Gene1); Blue (Gene2)", 
                                  "Red (Gene1); Green (Gene2)", 
                                  "Green (Gene1); Blue (Gene2)"), 
                      selected = "Red (Gene1); Blue (Gene2)"), 
         radioButtons("sc1b2ord1", "Plot order:", 
                      choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                      selected = "Max-1st", inline = TRUE) 
       ) 
     ), # End of column (6 space) 
     column( 
       6, style="border-right: 2px solid black", 
       uiOutput("sc1b2oup1.ui"), 
       downloadButton("sc1b2oup1.pdf", "Download PDF"), 
       downloadButton("sc1b2oup1.png", "Download PNG"), br(), 
       div(style="display:inline-block", 
           numericInput("sc1b2oup1.h", "PDF / PNG height:", width = "138px", 
                        min = 4, max = 20, value = 8, step = 0.5)), 
       div(style="display:inline-block", 
           numericInput("sc1b2oup1.w", "PDF / PNG width:", width = "138px", 
                        min = 4, max = 20, value = 10, step = 0.5)) 
     ), # End of column (6 space) 
     column( 
       3, uiOutput("sc1b2oup2.ui"), 
       downloadButton("sc1b2oup2.pdf", "Download PDF"), 
       downloadButton("sc1b2oup2.png", "Download PNG"), 
       br(), h4("Cell numbers"), 
       dataTableOutput("sc1b2.dt") 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
 ### Tab1.b2: vISH
 tabPanel( 
   HTML("vISH"), 
   h4("E14.5 Specific spatial mapping of single cell transcriptome by DistMap"), 
   "In this tab, users can visualise the genes ", 
   "on virtual in situ hybridization (vISH).", 
   fluidRow(
     column( 
       12, selectInput(inputId="vish", "Gene name:", choices=NULL),
     ),
     column(12,
           textInput(inputId="thres", label="Thres", value = 0.75),),
     ),
     div(inputId="sc1a4oup1" ),
   br(),br(), 
 ),     # End of tab (2 space) 
 
 
br(), 
p(("© 2022 Li Laboratory"),style = "font-size: 125%;  font-weight: bold"), 
br(),br(),br(),br(),br() 
))) 
 
 
 
 
