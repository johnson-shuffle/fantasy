rm(list=ls())
source("~/GoogleDrive/Research/Fantasy/scripts/fantasy.Rprofile")

#---------------------------------------------------------------------------------
# Weekly schedules
#---------------------------------------------------------------------------------
url<-"http://espn.go.com/nfl/schedule/_/"
weeks<-c(1:17)
years<-c(2006:2015)

dat<-NULL
for (y in years) {
  for (w in weeks) {
    page<-paste(url,"/year/",y,"/week/",w,sep="")
    ses<-html_session(page)
    tbls<-html_table(ses)
    dates<-html_nodes(ses,"caption")
    if ("BYE" %in% names(tbls[[length(tbls)]])) {
      tbls<-tbls[1:(length(tbls)-1)]
    }
    sch<-NULL
    for (i in 1:length(tbls)) {
      df<-tbls[[i]]
      df<-df[c(1,2)]
      names(df)<-c("TEAM","OPPONENT")
      df$TEAM<-sapply(df$TEAM,function(x) strsplit(x," ")[[1]][length(strsplit(x," ")[[1]])])
      df$OPPONENT<-sapply(df$OPPONENT,function(x) strsplit(x," ")[[1]][length(strsplit(x," ")[[1]])])
      df$OPPONENT<-paste("@",df$OPPONENT,sep="")
      df$DAY<-trim(xml_text(dates[[i]]))
      sch<-rbind(sch,df)
    }
    sch$WEEK<-w
    sch$YEAR<-y
    dat<-rbind(dat,sch)
    cat("*")
  }
  print(y)
}

# One game was postponed: NYJ @BUF Sunday November 23, 2014 moved to Monday
row.names(dat[dat$YEAR==2014 & dat$TEAM=="NYJ" & dat$DAY=="Sunday, November 23",])
dat<-dat[row.names(dat)!=1193,]

# Flip schedule
dat.flip<-dat
dat.flip$TEAM<-dat$OPPONENT
dat.flip$OPPONENT<-dat$TEAM
dat.flip$TEAM<-gsub("@","",dat.flip$TEAM)

# Combine, melt, cast, melt
sch<-rbind(dat,dat.flip)
sch.melt<-unique(sch[c("TEAM","WEEK","YEAR","OPPONENT")])
sch.melt<-melt(sch.melt,id=c(c("TEAM","WEEK","YEAR")))
sch.cast<-dcast(sch.melt,TEAM+YEAR~WEEK,fill="bye")
sch.melt<-melt(sch.cast,id=c(c("TEAM","YEAR")))
names(sch.melt)[3:4]<-c("WEEK","OPPONENT")

# Merge the day back in
schedule<-merge(sch[c("TEAM","WEEK","YEAR","DAY")],
                sch.melt,
                by=c("TEAM","WEEK","YEAR"),
                all.x=T,all.y=T)
data.frame(table(schedule$TEAM))

# Save
saveRDS(schedule,file=paste(dshiny,"nfl_schedules.Rds",sep=""))
