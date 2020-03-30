#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Forecasting Covid-19 Infections"),
   
   # Sidebar with a slider input for number of bins 
   selectInput(inputId = 'country',
               selected = 'US',
               label = 'Select a Country',
               choices = c("Afghanistan","Albania","Algeria","Andorra","Angola","Antigua and Barbuda","Argentina",
                           "Armenia","Australia","Austria","Azerbaijan","Bahamas","Bahrain","Bangladesh","Barbados",
                           "Belarus","Belgium","Belize","Benin","Bhutan","Bolivia","Bosnia and Herzegovina","Brazil",
                           "Brunei","Bulgaria","Burkina Faso","Burma","Cabo Verde","Cambodia","Cameroon","Canada",
                           "Central African Republic","Chad","Chile","China","Colombia","Congo (Brazzaville)","Congo (Kinshasa)",
                           "Costa Rica","Cote d'Ivoire","Croatia","Cuba","Cyprus","Czechia","Denmark","Diamond Princess",
                           "Djibouti","Dominica","Dominican Republic","Ecuador","Egypt","El Salvador","Equatorial Guinea",
                           "Eritrea","Estonia","Eswatini","Ethiopia","Fiji","Finland","France","Gabon","Gambia","Georgia",
                           "Germany","Ghana","Greece","Grenada","Guatemala","Guinea","Guinea-Bissau","Guyana","Haiti",
                           "Holy See","Honduras","Hungary","Iceland","India","Indonesia","Iran","Iraq","Ireland","Israel",
                           "Italy","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Korea, South","Kosovo","Kuwait","Kyrgyzstan",
                           "Laos","Latvia","Lebanon","Liberia","Libya","Liechtenstein","Lithuania","Luxembourg","Madagascar","Malaysia",
                           "Maldives","Mali","Malta","Mauritania","Mauritius","Mexico","Moldova","Monaco","Mongolia","Montenegro",
                           "Morocco","Mozambique","MS Zaandam","Namibia","Nepal","Netherlands","New Zealand","Nicaragua","Niger",
                           "Nigeria","North Macedonia","Norway","Oman","Pakistan","Panama","Papua New Guinea","Paraguay","Peru",
                           "Philippines","Poland","Portugal","Qatar","Romania","Russia","Rwanda","Saint Kitts and Nevis",
                           "Saint Lucia","Saint Vincent and the Grenadines","San Marino","Saudi Arabia","Senegal","Serbia",
                           "Seychelles","Singapore","Slovakia","Slovenia","Somalia","South Africa","Spain","Sri Lanka","Sudan",
                           "Suriname","Sweden","Switzerland","Syria","Taiwan*","Tanzania","Thailand","Timor-Leste","Togo",
                           "Trinidad and Tobago","Tunisia","Turkey","Uganda","Ukraine","United Arab Emirates","United Kingdom",
                           "Uruguay","US","Uzbekistan","Venezuela","Vietnam","West Bank and Gaza","Zambia","Zimbabwe")),
   
   sliderInput(inputId = 'forecastPeriod',
               label = "Forecast Period (in days)",
               min = 2,
               max = 7,
               value = 1),
   
   # Show a plot of the generated distribution
      mainPanel(
         plotOutput("forecastPlot")
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$forecastPlot <- renderPlot({
     
      library(forecast); library(dplyr); library(tidyr); library(ggplot2); library(stringr); library(lubridate); library(ggthemes)
      dat = read.csv('https://raw.githubusercontent.com/pseudorational/covid/master/time_series_covid19_confirmed_global.csv')
      data = 
         dat %>%
         gather(key = date,value=infections,5:ncol(dat))%>%
         mutate(date = str_replace(string = date,pattern = "X",replacement = ""))%>%
         mutate(date = mdy(date))%>%
         group_by(date, Country.Region)%>%
         summarize(infections=sum(infections))%>%
         ungroup()%>%
         filter(Country.Region==input$country)%>%
         select(date, infections)
      
      data_ts = ts(data = data$infections,start = c(3,1),frequency = 7)
      arima_auto = auto.arima(data_ts,stepwise = F,approximation = F)
      fcast = forecast(object = arima_auto,h = input$forecastPeriod)
      df =
         rbind(cbind(data,type = rep('Past',nrow(data))),
               data.frame(
                  date = seq.Date(from = last(data$date)+1,to = last(data$date)+input$forecastPeriod,by = 'day'),
                  infections = fcast$mean,
                  type = rep('Future',length(fcast$mean))))
      
      ggplot(data = df, aes(x=date,y=infections, color=type))+
         geom_line(size=1.1)+
         geom_text(data = df[df$type=='Future',],aes(label=formatC(round(infections,0),big.mark=',',format='fg')),size=3,nudge_x = -5)+
         ggtitle(label = paste('Prediction of Covid Infections for ', input$country))+
         scale_color_manual(name='',values = c('#619CFF','#F8766D'))+
         theme_fivethirtyeight()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

