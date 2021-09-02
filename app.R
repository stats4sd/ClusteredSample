library(tidyverse)
library(shiny)
library(Hmisc)

set.seed(1349934)
x<-runif(50,0,1)
y<-runif(50,0,1)

sourcepoint<-c(rnorm(1,mean=0.5,sd=0.25),rnorm(1,mean=0.5,sd=0.25))

locations<-data.frame(x,y)

locations$dist<-NA
for(i in 1:50)  {
  locations$dist[i]<-dist(rbind(locations[i,1:2],sourcepoint))
}
locations$Yield<-500+sqrt(rnorm(50,1000*(0.5+1/locations$dist),800))



p1<-ggplot(data=locations,aes(x=x,y=y,fill=Yield))+
  geom_point(size=4,pch=21)+
   theme_minimal()+
  xlab("")+ylab("")+
  scale_x_continuous(breaks=NULL)+
  scale_y_continuous(breaks=NULL)+
  ggtitle("Villages In My Region")+
  scale_fill_fermenter(palette="RdYlGn",direction=1)+
  theme(panel.border = element_rect(fill=NA))



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Clustered Sample"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of clusters to sample",
                        min = 1,
                        max = 25,
                        value = 5),
            sliderInput("size","Total sample size",min=50,max=500,value=250),
            actionButton("rep","Sample!")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("ciPlot")
        )
    )
)

rows<-NA
# Define server logic required to draw a histogram
server <- function(input, output) {

  counter <- reactiveValues(countervalue = 0) # Defining & initializing the reactiveValues object

  observeEvent(input$rep, {
    counter$countervalue <- counter$countervalue + 1     # if the add button is clicked, increment the value by 1 and update it
  })


  POPinput <- eventReactive(input$rep, {

    smp<-sample(1:50,as.numeric(input$bins))
    mypop<-locations[smp,]

    return(mypop)
  })


  Stats <- eventReactive(input$rep, {

    nums<-NULL
    for(i in 1:as.numeric(input$bins)){
     tmp<-data.frame(clust=POPinput()[i,1],
                y=rnorm(mean=POPinput()[i,4],sd=20,n=round(as.numeric(input$size)/as.numeric(input$bins))))
     nums<-rbind(nums,tmp)
    }

    deff<-Hmisc::deff(nums$y,factor(nums$clust))

    sumstats<-data.frame(mean=mean(nums$y),se=(sd(nums$y)/sqrt(nrow(nums)))*deff[4],n=nrow(nums),
                         n0=round(as.numeric(input$size)/as.numeric(input$bins)))

    return(sumstats)
  })


    output$distPlot <- renderPlot({
     if(counter$countervalue==0){
       print(p1)
     }
      else{
   p2<-p1+geom_point(data=POPinput(),aes(x=x,y=y),shape=15,col="grey70",size=8)+
     geom_point(size=4,pch=21)+
     ggtitle("Villages In My Region",subtitle=paste("Total sample =",Stats()$n,"Sample per cluster =",Stats()$n0))
   print(p2)
      }
      })

    output$ciPlot <- renderPlot({
      print(Stats())


      ggplot(data=Stats(),aes(x=1,y=mean,ymax=mean+1.96*se,ymin=mean-1.96*se))+
        geom_hline(yintercept=mean(locations$Yield),col="red",lty=2)+
        geom_errorbar(size=2,width=0.2)+
        geom_point(col="red",size=4)+
        ylim(500,650)+
        xlim(0.8,1.2)+
        theme_minimal()+
        ggtitle("Estimated yield + margin of error",subtitle=paste("Mean Yield =",round(Stats()$mean),"Margin of error +-",round(1.96*Stats()$se),"kg/ha"))

    })


}

# Run the application
shinyApp(ui = ui, server = server)
