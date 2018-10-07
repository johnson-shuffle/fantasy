fantasy <- readRDS(paste(dshiny,"nfl_stats.Rds",sep=""))
schedule <- readRDS(paste(dshiny,"nfl_schedules.Rds",sep=""))
players <- c("matt ryan","justin forsett","mark ingram","alfred morris",
             "c.j. spiller","danny woodhead","devonta freeman","antonio brown",
             "a.j. green","nelson agholor","charles johnson","kevin white",
             "travis kelce","connor barth")
positions <- c("QB","RB")
week <- 1
year <- 2015

get.players <- function(data,positions,year) {
  if (year==2015) {
    foo <- subset(data,POSITION %in% positions & !is.na(CURRENT.TEAM)) %>%
      use_series("PLAYER.FULL") %>%
      unique %>%
      sort
    foo
  } else {
    foo <- subset(data,POSITION %in% positions & YEAR==year ) %>%
      use_series("PLAYER.FULL") %>%
      unique %>%
      sort
    foo
  }
}

get.pregame <- function(players,positions,year,week) {
  
  career <- fantasy %>%
    subset(YEAR<=year) %>%
    subset(PLAYER.FULL %in% players) %>%
    subset(POSITION %in% positions) %>%
    mutate(
      player = PLAYER.FULL,
      team = CURRENT.TEAM
    ) %>%
    group_by(player) %>% 
    mutate(
      games = length(player),
      avg = round(mean(PTS),digits=2),
      min = min(PTS),
      max = max(PTS)
    ) %>%
    extract(c("player","team","games","avg","min","max")) %>%
    unique %>%
    merge(subset(schedule,YEAR==year & WEEK==week),by.x="team",by.y="TEAM") %>%
    mutate(
      team = tolower(team),
      opp = tolower(OPPONENT)
    ) %>%
    extract(c("player","team","games","avg","min","max","opp"))
  
  career$avg.v.opp <- NA
  for (i in 1:length(players)) {
    versus <- fantasy %>%
      subset(PLAYER.FULL %in% career$player[i]) %>%
      subset(tolower(OPPONENT)==gsub("@","",career$opp[i])) %>%
      use_series(PTS) %>%
      mean
    if (!is.na(versus)) {career$avg.v.opp[i] <- versus}
     
  }
  career
}

shinyServer(function(input, output) {

  output$qb <- renderUI({
    selectInput("qb",
                "quarterbacks",
                get.players(fantasy,"QB",input$year),
                selected=c("matt ryan"),
                multiple=T)
  })
  
  output$rb <- renderUI({
    selectInput("rb",
                "running backs",
                get.players(fantasy,"RB",input$year),
                selected=c("justin forsett","mark ingram",
                           "alfred morris","c.j. spiller",
                           "danny woodhead","devonta freeman"),
                multiple=T)
  })
  
  output$wr <- renderUI({
    selectInput("wr",
                "wide receivers",
                get.players(fantasy,"WR",input$year),
                selected=c("antonio brown","a.j. green",
                           "nelson agholor","charles johnson",
                           "kevin white"),
                multiple=T)
  })
  
  output$te <- renderUI({
    selectInput("te",
                "tight ends",
                get.players(fantasy,"TE",input$year),
                selected=c("travis kelce"),
                multiple=T)
  })
  
  output$pk <- renderUI({
    selectInput("pk",
                "kickers",
                get.players(fantasy,c("PK"),input$year),
                selected=c("connor barth"),
                multiple=T)
  })
  
  output$df <- renderUI({
    selectInput("df",
                "defense",
                sort(unique(fantasy$TEAM)),
                selected="BAL")
  })
  
  output$image <- renderUI({
    headshot <- fantasy %>%
      subset(PLAYER.FULL %in% "marshawn lynch") %>%
      use_series("PLAYER.ID") %>%
      unique %>%
      paste0("http://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/",.) %>%
      paste0(.,".png") %>%
      img(src = ., height = 100, width = 100*1.4)
  })
  
  output$history <- renderPlot({
    players <- c(input$qb,input$rb,input$wr,input$te,input$pk)
    if (input$position == "all") {
      positions <- c("QB","RB","WR","TE","PK")
    } else {
      positions <- toupper(input$position)
    }
    p <- fantasy %>%
      subset(POSITION %in% positions) %>%
      group_by(YEAR) %>%
      mutate(
        AVG.PTS = mean(PTS[PTS!=0])
      ) %>%
      subset(PLAYER.FULL %in% players) %>%
      subset(YEAR==input$year) %>%
      ggplot(data = .) +
      geom_bar(aes(x=factor(WEEK,levels=c(1:17)),y=PTS,fill=PLAYER.FULL),
               stat="identity",position="dodge",origin=0) +
      geom_line(aes(x=WEEK,y=AVG.PTS)) +
      labs(x = "", y = "") +
      scale_x_discrete(drop=F) +
      scale_fill_discrete("player",drop=F) +
      theme_classic()
    print(p)
  })
  
  output$pregame <- renderTable({
    players <- c(input$qb,input$rb,input$wr,input$te,input$pk)
    if (input$position == "all") {
      positions <- c("QB","RB","WR","TE","PK")
    } else {
      positions <- toupper(input$position)
    }
    tb <- get.pregame(players,positions,input$year,input$week)
    tb <- tb[order(desc(tb$avg)),]},
    include.rownames=F
  )
    
})