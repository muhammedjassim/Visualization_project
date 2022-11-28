
# Link to hosted web app:
#   

# Importing Libraries required
library(ggplot2)
library(dplyr)
library(shiny)
library(shinydashboard)

# Import and set up the data

mydata = read.csv("IHMStefanini_industrial_safety_and_health_database_with_accidents_description.csv")

# Removing irrelevent columns.

mydata$Description = NULL
mydata$X = NULL

# Renaming columns

colnames(mydata)[1] = 'Date'
colnames(mydata)[4] = 'Sector'
colnames(mydata)[5] = 'Level'
colnames(mydata)[6] = 'Potential_level'
colnames(mydata)[7] = 'Gender'
colnames(mydata)[8] = 'Victim'
colnames(mydata)[9] = 'Critical_risk'

# Preparing the Date column.

mydata$Date = format(as.Date(mydata$Date), '%Y-%m')

head(mydata)

ui <- dashboardPage(
  dashboardHeader(title="Industrial Accidents"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Data", tabName = "data"),
      menuItem("Univariate Analysis", tabName = "univar"),
      menuItem("Multivariate Analysis", tabName = "multivar"),
      menuItem("Time Series Anlaysis", tabName = "timeseries")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("about",
              tabPanel("About",
                       h2("Industrial Labour Accidents Data"),
                       h3("This dashboard aims at addressing the different factors associated with industrial accidents and analysing them. We utilize the data obtained to visually analyse them by allowing the user to interact with the plots and retrieve the insights required. I have included plots which provide information into the number of accidents occured corresponding to different factors associated. We extract relevant information from the data using different interactive visualization techniques."),
                       align = "center"
                )),
      tabItem("univar",
              tabsetPanel(
                tabPanel("Frequency",
                         h3("Analysis the frequency of values of a variable"),
                         selectInput("cat_var", "Categorical Variable", colnames(mydata)[-c(9)]),
                         plotOutput("hist_plot"),
                         h4("Frequency Table"),
                         tableOutput("freq_table"),
                         align = "center"
                  ),
                tabPanel("Proportion",
                         h3("Analysis the proportion of values of a variable"),
                         selectInput("cat_var_1", "Categorical Variable", colnames(mydata)[-c(1,3,9)]),
                         plotOutput("pie_plot"),
                         align = "center"
                  )
                )),
      tabItem("multivar",
                tabPanel("Categorical-Categorical Analysis",
                         h3("Analysis of two Variables against each other"),
                         selectInput("cat_var_2",  "Variable 1", colnames(mydata)[-c(3,9)]),
                         selectInput("cat_var_3", "Variable 2", colnames(mydata)[-c(1,3,9)]),
                         plotOutput("multi_plot"),
                         align = "center"
                )),
      tabItem("timeseries",
              tabPanel("Time Series",
                       h3("Analysis of trend of values of a variable over time"),
                       selectInput("cat_var_4", "Select Variable", colnames(mydata)[-c(1,3,9)]),
                       plotOutput("time_plot"),
                       align = "center"
              )),
      tabItem("data",
              tabsetPanel(
                tabPanel("Data Head",
                         h3("First 10 rows of the Industrial Labours Accidents Data"),
                         tableOutput("table_head"),
                         align = "center"
                ),
                tabPanel("Data Select",
                         h3("Viewing the selected portion of the data"),
                         checkboxGroupInput("selvars", "Add/Remove Data Variables", colnames(mydata)),
                         textInput("selrows", "Select Rows (vector expression)", "1:15"),
                         tableOutput("table_select"),
                         align = "center"
                )
              ))
    )
  )
)

server <- function(input,output){
  
  rv = reactiveValues()
# Vector for the colors used
  
  colours = c("#22A39F","#A4BE7B","#905E96","#92B4EC",'#F57328','#C21010')
    
# Plots for univariate
  
  ## Plot for Histogram
  
  output$hist_plot = renderPlot({
    var = input$cat_var
    ggplot(data=mydata) + 
      geom_histogram(mapping=aes_string(x=var,fill=var), stat="count",
                     color="black", position="identity", fill = "#6D8B74") +
      ggtitle(paste0("Frequency of Accidents with respect to ", var)) +
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            axis.title = element_text(size = 17),
            axis.text = element_text(size = 15))
  })
  
  ## Plot of Frequency table
  
  output$freq_table = renderTable({
    var = input$cat_var
    t = table(mydata[,var])
  }, colnames=FALSE)
  
  ## Plot of Pie chart
  
  output$pie_plot = renderPlot({
    var1 = input$cat_var_1
    gp_df = mydata%>%group_by(across(var1))%>%summarise(total_count=n(),groups='drop')
    ggplot(gp_df, aes_string(y='total_count',x="''",fill=var1)) +
      geom_col(color = "black") +
      theme_void() + scale_fill_manual(values = colours) +
      geom_bar(stat='identity', color = "black") + coord_polar('y') +
      ggtitle(paste('Proportion of accidents accross various',var1)) +
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            legend.key.size = unit(1, 'cm'), legend.title = element_text(size = 15),
            legend.text = element_text(size = 13))
  }, height = 650)
  
# Plots for multivariate

  output$multi_plot = renderPlot({
    var2 = input$cat_var_2
    var3 = input$cat_var_3
    ggplot(data=mydata) +
      geom_histogram(mapping=aes_string(x=var2, fill=var3), stat="count",position = "stack", color="black") +
      ggtitle(paste0("Histogram of ", var2, " stacked by ", var3)) +
      scale_fill_manual(values = colours) +
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            axis.title = element_text(size = 17),
            axis.text = element_text(size = 15),
            legend.key.size = unit(0.6, 'cm'),
            legend.title = element_text(size = 17),
            legend.text = element_text(size = 13))
  }, height = 600)
  
# Plots for time series
  
  output$time_plot = renderPlot({
    var4 = input$cat_var_4
    ggplot(data=mydata, aes(x=Date)) +
      geom_line(mapping=aes_string(group=var4, col=var4),stat = 'count', size = 1.2) +
      geom_point(mapping=aes_string(group=var4, col=var4),stat = 'count', size = 2) +
      labs(x= 'Month', y = 'Number of Accidents') +
      ggtitle(paste0('Trend in number of accidents in different ', var4)) +
      scale_colour_manual(values = colours) +
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            axis.text.x = element_text(angle = 90, vjust = 0.5, size = 13),
            legend.key.size = unit(1.5, 'cm'), legend.title = element_text(size = 17),
            legend.text = element_text(size = 15), axis.title = element_text(size = 17),
            axis.text.y = element_text(size = 13))
  }, height = 600)
  
# Data Viewing
  
  ## Data head
  
  output$table_head = renderTable({
    mydata[1:10,]
  })

  ## Selective Data view
  
  output$table_select = renderTable({
    vars = input$selvars
    dat = cbind(data.frame(S.no=1:nrow(mydata)), mydata[,vars])
    rows = eval(parse(text=input$selrows))
    dat[rows,]
  })
}


## main()
shinyApp(ui,server)

