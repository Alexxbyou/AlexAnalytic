##########################################################################
##########################################################################

# Customize ggplot2

##########################################################################
##########################################################################

require(ggplot2)
require(dplyr)
require(gridExtra)

##########################
# Column functions for data preparation
##########################

# multiple columns, to solve the frequency or prevalence
col.aggr<-function(name,df){
  as.data.frame(table(df[,getcolumn(name,df)]),stringsAsFactors=F)
}

# retrieve the column name according to the given search
getcolumn<-function(name,df,column.name=T){
  grep(name,names(df),ignore.case = T,value=column.name)
}


# multiple columns, to solve the frequency or prevalence
cols.aggr<-function(df,prev=T){
  func<-ifelse(prev,"mean","sum")
  prev.c<-apply(df,2,get(func))
  prev.df<-data.frame(
    Disease=names(prev.c),
    Prevalence=prev.c,
    stringsAsFactors = F
  )
  return(prev.df)
}

##########################
# Age Visualization
##########################

cutoff2label<-function(cutoff){
  np<-length(cutoff)
  result<-c(
    paste(cutoff[1:(np-2)],"-",cutoff[2:(np-1)]-1,sep=""),
    paste(cutoff[np-1],"+",sep="")
  )
  return(result)
}

elderly.group<-function(cutoff,elderly){
  ngrp<-length(cutoff)-1
  first.elderly<-ceiling(mean(which(sort(c(cutoff,elderly))==elderly)))-1
  result<-c(
    rep("No",first.elderly-1),
    rep("Yes",ngrp-first.elderly+1)
  )
  result<-factor(result,levels=c("Yes","No"))
  return(result)
}

age.vis<-function(
  age,
  cutoff=c(0,15+0:7*10),
  elderly=65
){
  if(is.vector(age)){
    age.vis.sing(age,cutoff,elderly)
  }else{
    names(age)<-c("age","group")
    age.vis.comp(age,cutoff,elderly)
  }
}


age.vis.sing<-function(
  age,
  cutoff=c(0,15+0:7*10),
  elderly=65
){
  cutoff<-c(0,cutoff,999)%>%
    unique()%>%
    sort()
  age.label<-cutoff2label(cutoff)
  
  age.group<-age%>%
    cut(breaks=cutoff,labels=age.label)%>%
    table()%>%
    as.data.frame()
  
  names(age.group)<-c("Age","Count")
  age.group$Elderly<-elderly.group(cutoff,elderly)
  
  g<-ggplot()+
    geom_bar(data=age.group,mapping=aes(x=Age,y=Count,fill=Elderly),stat="identity")+
    ggtitle("Age")+
    theme(legend.position = "bottom"
          ,plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  return(g)
}


age.vis.comp<-function(
  age.n.grp,   # data.frame(age,group)
  cutoff=c(0,15+0:7*10),
  elderly=65
){
  cutoff<-c(0,cutoff,999)%>%
    unique()%>%
    sort()
  age.label<-cutoff2label(cutoff)
  
  age.group.list<-lapply(unique(age.n.grp$group),function(grp){
    age<-age.n.grp$age[age.n.grp$group==grp]
    age.group<-age%>%
      cut(breaks=cutoff,labels=age.label)%>%
      table()%>%
      as.data.frame()
    names(age.group)<-c("Age","Count")
    age.group$Elderly<-elderly.group(cutoff,elderly)
    age.group$Group<-grp
    return(age.group)
  })
  age.group.sum<-do.call(rbind.data.frame,age.group.list)
  age.group.sum$Group<-factor(age.group.sum$Group)
  
  g<-ggplot()+
    geom_bar(data=age.group.sum,mapping=aes(x=Age,y=Count,group=Group,fill=Group,color=Elderly),size=1.1,stat="identity",position="dodge")+
    ggtitle("Age")+
    theme(legend.position = "bottom"
          ,plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  return(g)
}


##########################
# Gender/Race/oth. Categorical Variables Visualization
##########################
cat.vis.sing<-function(data,Title){
  if(length(unique(data[,1]))>3){
    cat.vis.bar.sing(data,Title)
  }else{
    cat.vis.donut.sing(data,Title)
  }
}

cat.vis.donut.sing<-function(
  data, #data.frame(Category,Count)
  Title
){
  names(data)<-c("Category","Count")
  data$Perc<-data$Count/sum(data$Count)
  data$Category<-factor(data$Category,levels=data$Category)
  data$label.perc<-paste(sprintf("%.1f",data$Perc*100),"%",sep="")
  lgd.txt<-paste(data$Category,"(",data$label.perc,")",sep="")[order(data$Category)]
  
  g<-ggplot() +
    geom_bar(data=data, mapping=aes(x=3.5,y=Perc,fill=Category),stat="identity",colour="grey30") +
    #geom_text(data=data,mapping=aes(x=3.5,y=Perc,label=label.perc,vjust=.1))+
    coord_polar(theta="y") +
    xlab("")+ylab("")+ggtitle(Title)+
  xlim(c(0, 4)) +
    scale_fill_discrete(name="",labels=lgd.txt)+
    theme(panel.grid=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          legend.position = "bottom",
          #legend.text = element_text(lgd.txt),
          plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  return(g)
}

cat.vis.donut.comp<-function(
  data, #data.frame(Category,Count,group)
  Title
){
  names(data)<-c("Category","Count","Group")
  groupsum<-data%>%
    group_by(Group)%>%
    summarize(grpsum=sum(Count))
  data<-left_join(data,groupsum)
  data$Perc<-data$Count/data$grpsum*100
  data$Group<-factor(data$Group,levels=c(unique(data$Group)," ","  ","   "))
  data$Category<-as.factor(data$Category)
  data$label.perc<-paste(sprintf("%.1f",data$Perc),"%",sep="")
  
  g<-ggplot() +
    geom_bar(data=data, mapping=aes(x=Group,y=Perc,fill=Category),stat="identity") +
    geom_text(mapping=aes(x=levels(data$Group),y=0,label=levels(data$Group),hjust=1))+
    coord_polar(theta="y") +
    xlab("")+ylab("")+ggtitle(Title)+
    scale_fill_discrete(name="")+
    theme(panel.grid=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          legend.position = "bottom",
          #legend.text = element_text(lgd.txt),
          plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  return(g)
}




cat.vis.bar.sing<-function(
  data, #data.frame(Category,Count)
  Title
){
  names(data)<-c("Category","Count")
  data$Perc<-data$Count/sum(data$Count)*100
  data$Category<-factor(data$Category,levels=data$Category)
  data$label.cnt<-formatC(data$Count,big.mark=",")
  
  g<-ggplot() +
    geom_bar(data=data, mapping=aes(x=Category,y=Perc,fill=Category),width=.5,stat="identity") +
    xlab("")+ylab("Proportion(%)")+ggtitle(Title)+
    scale_fill_discrete(name="")+
    theme(legend.position = "bottom",
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title = element_text(size = 12, face = "bold"),
          #legend.text = element_text(angle=30),
          plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  return(g)
}

cat.vis.bar.comp<-function(
  data, #data.frame(Category,Count,group)
  Title
){
  names(data)<-c("Category","Count","Group")
  groupsum<-data%>%
    group_by(Group)%>%
    summarize(grpsum=sum(Count))
  data<-left_join(data,groupsum)
  data$Perc<-data$Count/data$grpsum*100
  data$Group<-as.factor(data$Group)
  data$Category<-as.factor(data$Category)
  data$label.cnt<-formatC(data$Count,big.mark=",")
  
  g<-ggplot() +
    geom_bar(data=data, mapping=aes(x=Category,y=Perc,fill=Group),width=.5,stat="identity",position="dodge") +
    xlab("")+ylab("Proportion(%)")+ggtitle(Title)+
    scale_fill_discrete(name="")+
    theme(legend.position = "bottom",
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          axis.title = element_text(size = 12, face = "bold"),
          #legend.text = element_text(angle=30),
          plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  return(g)
}

data%>%
  group_by(Group)%>%
  summarize(grpsum=sum(Count))

####################################################
# Chronic Condition Prevalence Visualization
####################################################

Prev.vis<-function(
  prev.df,  # data.frame(Disease, Prevalence, Std)
  order=c("default","alphabet","desc","asc"),
  Title="Disease Prevalence",
  confidence=.95
){
  order<-order[1]
  if(order=="default")
    prev.df$Disease<-factor(prev.df$Disease,levels=rev(prev.df$Disease))
  if(order=="alphabet")
    prev.df$Disease<-factor(prev.df$Disease,levels=rev(sort(as.character(prev.df$Disease))))
  if(order=="desc"){
    lvl<-prev.df$Disease[order(prev.df$Prevalence)]
    prev.df$Disease<-factor(prev.df$Disease,levels=lvl)
  }
  if(order=="asc"){
    lvl<-prev.df$Disease[order(-prev.df$Prevalence)]
    prev.df$Disease<-factor(prev.df$Disease,levels=lvl)
  }
  if(ncol(prev.df)<=2)prev.df$Std<-0
  q.conf<-1-(1-confidence)/2
  prev.df$prev.min<-prev.df$Prevalence-prev.df$Std*qnorm(q.conf)
  prev.df$prev.max<-prev.df$Prevalence+prev.df$Std*qnorm(q.conf)
  
  g<-ggplot()+
    geom_bar(data=prev.df,mapping=aes(x=Disease,y=Prevalence),stat="identity",fill="steelblue")+
    geom_errorbar(data=prev.df,mapping=aes(x=Disease,ymin=prev.min, ymax=prev.max),color="#F08080",width=.2)+
    coord_flip()+
    ggtitle(Title)+
    theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  return(g)
}



####################################################
# Multiple plot function
####################################################
# if no share legend, use grid.arrange()

grid_arrange_shared_legend <-
  function(...,
           ncol = length(list(...)),
           nrow = 1,
           position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
    
  }







