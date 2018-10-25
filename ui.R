require(ggplot2)
require(knitr);require(rmarkdown)
require(shiny)

#require(shinythemes)

# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
#library(datasets)

# Define the overall UI
shinyUI(
  
  navbarPage(title=div( "METODOLOGIA DE ANALISIS DE REDES DE TRABAJO"),
                   theme = shinytheme("cerulean"),
      #             img(src="univ.png", style="float:right; padding-right:25px"), 
                   tabPanel("METODOLOGIA",
                            img(src="univ.png", style="float:right; padding-right:25px"),  
                      h2("Titulo"),  
                        
                    hr('Texto Introduccion'),
                    h1(),
                    img(src="univ.png", style="float:center; padding-right:25px"),
                    hr(),
                    hr("otro texto")
                            
                            ),
                   tabPanel("Matriz de Adyacencia",
                            img(src="univ.png", style="float:right; padding-right:25px"),  
                            # Use a fluid Bootstrap layout
                            # fluidPage(theme = "bootstrap.css",    
                            
                            sidebarLayout(      
                              
                              # Define the sidebar with one input
                              sidebarPanel(

                                fileInput('file1', 'Cargue la matriz de Adyacencia en formato csv:',
                                          accept=c('text/csv', 'text/comma-separated-values,text/plain'))                                
                                #actionButton("save2", "Guardar")#,
                                

                              ),
                              
                              # Create a spot for the barplot
                              mainPanel(
                                #plotOutput("grap"),
                                h3("Matriz de Adyaciencia"),
                                
                                div(DT::dataTableOutput("matriz1"), style = "font-size: 100%; width: 75%")
                                
#                                plotOutput('plot1')
                                #dataTableOutput(outputId="tecpro")
                              ))),
                   
                   tabPanel("Matriz de Caracteristicas",
                            img(src="univ.png", style="float:right; padding-right:25px"),
                            sidebarLayout(
                              sidebarPanel(

                                fileInput('file2', 'Cargue la matriz de caracteristicas en formato csv:',
                                          accept=c('text/csv', 'text/comma-separated-values,text/plain'))
                                
                              ),
                              mainPanel(

                                h3('Matriz de caracteristicas'),
                                div(DT::dataTableOutput("matriz2"), style = "font-size: 100%; width: 75%")
                                
                              ))),
                   tabPanel("Resultado Red",
                            img(src="univ.png", style="float:right; padding-right:25px"),
                            sidebarLayout(position = "right",
                              sidebarPanel(width = 3,
                                
                              sliderInput('tamano', 'Tamano de los nodos', min = 1, max = 100,
                                                       value = 10, step = 10, round = 0),
                              
                              numericInput("elecpos","Numero de elecciones Posibles",3),
                              
                              #verbatimTextOutput,
                              
                              helpText("Indicadores"),
                              div(DT::dataTableOutput('ind_tot'), style = "font-size: 100%; width: 50%")
                                
                                ),
                              mainPanel(
                              h3('Grafo de elecciones positivas'),
                              
                              visNetworkOutput("network_hello",height = "1000px"),
                              
                              h3('Grafo de elecciones negativas'),
                              
                              visNetworkOutput("network_hello4",height = "1000px")
                              ))),
                   tabPanel("Resultados Redes por Filtro",
                            img(src="univ.png", style="float:right; padding-right:25px"),
                            sidebarLayout(position = "right",
                              sidebarPanel( width = 2,

                                hr("Indicadores de resultados por grupo"),
                                
                                uiOutput("filt"),
                                
                      hr("Indicadores de resultados por persona"),
                      
                      uiOutput("iden2"),
                      
                      verbatimTextOutput('values')
                              ),
                              mainPanel(
                                
                          hr('Resultados Relaciones de atraccion'),
                          
                          visNetworkOutput("network_hello2",height = "1000px"),

                          hr('Resultados Relaciones de rechazo'),
                          visNetworkOutput("network_hello3",height = "1000px")      
                          )
                              )),
                  tabPanel("Indicadores",
                           div(DT::dataTableOutput("indicadores"), style = "font-size: 100%; width: 75%")
                  )

)
                   
) 



