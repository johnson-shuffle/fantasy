rm(list=ls())
source("~/GoogleDrive/Research/Fantasy/scripts/fantasy.Rprofile")

library(shiny)
library(DT)

shinyUI(fluidPage(
  titlePanel(
    div(
      img(src = "nyg.png", height = 72, width = 72),
      img(src = "sea.png", height = 72, width = 72),
      "new york seahawks / seattle giants",
      img(src = "sea.png", height = 72, width = 72),
      img(src = "nyg.png", height = 72, width = 72)
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      
      fluidRow(
        column(6,selectInput("year","year",c(2006:2015),"2015")),
        column(6,selectInput("week","week",c(1:17),"1"))
      ),
      
      uiOutput("qb"),
      
      uiOutput("rb"),
      
      uiOutput("wr"),
      
      uiOutput("te"),
      
      uiOutput("pk"),
      
      uiOutput("df")
          
    ),
    
    mainPanel(      
      
      selectInput("position","postion",c("all","qb","rb","wr","te","pk"),"all"),
      
      plotOutput("history"),
      
      tableOutput("pregame")
      
      #uiOutput("image")
      
    )
    
  )
  
))