library("shiny")
library("shinydashboard")
library("smooth")
library("rAmCharts")
library("ggplot2")
library("plotly")
library("data.table")
library("googleVis")
library("readxl")
library("dplyr")
library("scales")
library("RColorBrewer")
library("lubridate")
library("ggrepel")
library("GGally")
library("ggpmisc")
library("DT")
library("foreign")
library("tidyr")
library("shinyjs")

#Path to find the data set in Kaggle
# https://www.kaggle.com/datasets/datascientistanna/customers-dataset
base <- read.csv('Customers.csv')
base<-
 base<-base%>%mutate_if(is.character,as.factor)
  datos_numericos<-base%>%select_if(is.numeric)
  datos_cat<-base%>%select_if(is.factor)
  
  #Estructura de un shiny dashboard 
  ui <- dashboardPage(skin="black", 
                      dashboardHeader(title="Data exploration"),
                      dashboardSidebar(
                        
                        sidebarMenu(
                          menuItem("Exploration", tabName="dashboard1", icon=icon("table")),
                        #  fileInput("file1", "Documento CSV", accept = ".csv"),
                        #  fileInput("file2", "Documento Excel", accept = ".xlsx"),
                          checkboxInput("resumen", label="Statistical summary", value = FALSE),
                          actionButton("go", label="Update", icon("refresh"))
                          
                        )
                        
                      ),
                      dashboardBody(
                        
                        useShinyjs(),  
                        
                        tabItems(
                          
                          tabItem(tabName = "dashboard1",
                                  fluidRow(
                                    column(12,
                                           h2("Data preview"),
                                           dataTableOutput(outputId = "tabla"),
                                           
                                           shinyjs::hidden(
                                             div(
                                               id = "hiddenbox", 
                                               
                                               h2("Statistical summary"),
                                               #box(htmlOutput(outputId = "resumen"),width = "100%"),
                                               box(verbatimTextOutput(outputId = "resumen",placeholder = F),width = 'auto',height = 'auto')
                                               
                                             )
                                           ),
                                           
                                           box(plotlyOutput(outputId = "grafico1"), solidHeader = FALSE, height="auto", width = "8"),
                                           box(selectInput(inputId = "var1",label="Choose a variable",choices = names(base[,-1])),
                                               width = "4"),
                                           
                                           
                                           box(plotlyOutput(outputId = "grafico2"), solidHeader = FALSE, width = "8"),
                                           box(selectInput(inputId = "var2",label="Choose a numeric variable",choices = names(datos_numericos)),
                                               width = "4"),
                                           box( sliderInput(inputId = "bins",
                                                            label = "Bins for the histogram:",
                                                            min = 1,
                                                            max = 100,
                                                            value = 30
                                           ),  width = "4")
                                    ),
                                    column(12,
                                           box(plotlyOutput(outputId = "grafico3"), solidHeader = FALSE, height="auto", width = "8"),
                                           box(selectInput(inputId = "var3",label="Choose a categorical variable",choices = names(datos_cat)),
                                               width = "4")
                                           
                                    ),
                                    column(12,
                                           box(plotlyOutput(outputId = "grafico4"), solidHeader = FALSE, height="auto", width = "8"),
                                           box(selectInput(inputId = "var4num",label="Choose a numeric variable",choices = names(datos_numericos)),
                                               width = "4"),
                                           box(selectInput(inputId = "var4cat",label="Choose a categorical variable",choices = names(datos_cat)),
                                               width = "4"),
                                           
                                           box(plotlyOutput(outputId = "grafico5"), solidHeader = FALSE, height="auto", width = "8"),
                                           box(selectInput(inputId = "var5cat1",label="Choose a categorical variable",choices = names(datos_cat)),
                                               width = "4"),
                                           box(selectInput(inputId = "var5cat2",label="Choose a categorical variable",choices = names(datos_cat)),
                                               width = "4"),
                                           
                                           box(plotlyOutput(outputId = "grafico6"), solidHeader = FALSE, height="auto", width = "8"),
                                           box(selectInput(inputId = "var6",label="Choose a date variable",choices = names(base)),
                                               width = "4"),
                                           
                                           box(plotlyOutput(outputId = 'grafico7'),solidHeader = F,height='auto',width='8'),
                                           box(selectInput(inputId = "var7num1",label="Choose a numerical variable",choices = names(datos_numericos)),
                                               width = "2"),
                                           box(selectInput(inputId = "var7num2",label="Choose a categorical variable",choices = names(datos_numericos)),
                                               width = "2"),
                                           box(selectInput(inputId = "var7num3",label="Choose a numerical variable",choices = names(datos_numericos)),
                                               width = "2"),
                                           box(selectInput(inputId = "var7num4",label="Choose a categorical variable",choices = names(datos_numericos)),
                                               width = "2"),
                                           
                                           box(plotlyOutput(outputId = "grafico8"), solidHeader = FALSE, height="auto", width = "8"),
                                           box(selectInput(inputId = "var8num1",label="Choose a numerical variable",choices = names(datos_numericos)),
                                               width = "4"),
                                           box(selectInput(inputId = "var8num2",label="Choose a categorical variable",choices = names(datos_numericos)),
                                               width = "4"),
                                           checkboxInput("tendencia", "Add tendency line", value = FALSE)
                                    )
                                    
                                  )
                          )
                          
                        )
                        
                      )
  )
  
  
  server <- function(input, output) {
    ####BASE A ANALIZAR#### 
    
    output$tabla<- renderDataTable({
      return(base)
    })
    
    #Habilita y deshabilita el resumen estadistico  
    observeEvent(input$go, {
      shinyjs::toggle(id = "hiddenbox")
    })
    output$resumen<-renderPrint({
      
      lista<-list(summary(base))
      return(lista)
      
    })
    
    
    output$grafico1<-renderPlotly({
      
      variable<-input$var1
      
      tabla<-base%>%
        group_by(base[[variable]])%>%
        summarise(n=n())
      
      names(tabla)<-c("X","Y")
      
      fig<- plot_ly(tabla,
                    x = ~X,
                    y = ~Y,
                    type = "bar",
                    color=~X,
                    marker = list(line = list(color = 'black',
                                              width = 1.5))
      )
      
      barras<-fig%>%layout(title=paste0('Count per ',input$var1),
                           showlegend=F,
                           yaxis=list(title='Frequency'),
                           xaxis=list(title='',tickangle=45))
      
      #barras<-plot(base$CONSULTORIO,base[[input$var1]])
      
      return(barras)
      
    })
    
    output$grafico2<-renderPlotly({
      
      bins <- seq(min(base[[input$var2]]),max(base[[input$var2]]) , length.out = input$bins + 1)
      
      p <- ggplot(base,aes(x=.data[[input$var2]] ))+
        geom_histogram(data=base,fill = "red", alpha = 0.2, breaks=bins) +
        labs(x="",y='Frequency',title=paste0('Data distribution according to ',input$var2))+
        scale_x_continuous(n.breaks = 6, labels = comma)
      
      histograma<-ggplotly(p)
      
      return(histograma)
      
    })
    
    output$grafico3<-renderPlotly({
      
      fig <- plot_ly(base%>%group_by(X=base[[input$var3]])%>%summarise(n=n()), 
                     labels=~X, values = ~n, type = 'pie')
      fig <- fig %>% layout(title = paste0('Percentage of units per ',input$var3),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      return(fig)
      
    })
    
    output$grafico4<-renderPlotly({
      
      
      fig<-plot_ly(data=base, y=~base[[input$var4num]], color= ~base[[input$var4cat]], type='box', boxpoints='all',jitter=0.2,pointpos=-1.8)
      
      fig<-fig%>%layout(showlegend=F,
                        title=paste0('Distribution of ',input$var4num,' per ', input$var4cat),
                        yaxis=list(title=""))
      
      return(fig)
    })
    
    output$grafico5<-renderPlotly({
      
      fig<-ggplot(data = base,aes(x=.data[[input$var5cat1]],fill=.data[[input$var5cat2]]))+geom_bar()+
        labs(y='Count of values')
      fig<-ggplotly(fig)
      return(fig)
    })
    
    output$grafico6<-renderPlotly({
      base[[input$var6]]<-as.character(base[[input$var6]])
      base[[input$var6]]<-as.POSIXct(base[[input$var6]], format="%d/%m/%Y")
      tabla<-base%>%group_by(VAR=base[[input$var6]])%>%summarise(n=n())

      fig<-    fig <- plot_ly(data = tabla,x =~VAR ,type = "scatter")
      fig<-fig%>%layout(showlegend=F,
                        title='Date',
                        xaxis=list(title=''))
      return(fig)
    })
    
    output$grafico7<-renderPlotly({
      nombres<-c(input$var7num1,input$var7num2,input$var7num3,input$var7num4)
      cor_mat<-cor(na.omit(base[,nombres]))
      
      fig<-ggcorrplot::ggcorrplot(cor_mat, hc.order = TRUE, type = "lower", outline.col = "white",lab=T,
                                  title='Pearson Correlation')
      return(fig)
    })  
    
    output$grafico8<-renderPlotly({
      
      if( input$tendencia==F){
        
        fig<- ggplot(base,aes(x=.data[[input$var8num1]],y=.data[[input$var8num2]]))+geom_point()+
          labs(x=paste0(input$var8num1),
               y=paste0(input$var8num2),
               title = paste0(input$var8num1," vs. ",input$var8num2)) +
          theme(plot.title=element_text(color="black",hjust=0.5))+
          scale_y_continuous(n.breaks = 10, labels = comma)+
          scale_x_continuous(n.breaks = 10,labels = comma)
        
      }  else{
        
        fig<- ggplot(base,aes(x=.data[[input$var8num1]],y=.data[[input$var8num2]]))+geom_point()+
          geom_smooth(method = 'lm',formula=y~x+0,se=T)+
          labs(x=paste0(input$var8num1),
               y=paste0(input$var8num2),
               title = paste0(input$var8num1," vs. ",input$var8num2)) +
          theme(plot.title=element_text(color="black",hjust=0.5))+
          scale_y_continuous(n.breaks = 10, labels = comma)+
          scale_x_continuous(n.breaks = 10,labels = comma)  
      }
      
      
      return(fig)
      
    })
    
  }
  
  shinyApp(ui = ui, server = server)


