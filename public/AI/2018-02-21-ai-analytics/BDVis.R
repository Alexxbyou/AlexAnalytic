##########################################################################
##########################################################################

# Customize ggplot2

##########################################################################
##########################################################################

require(ggplot2)
require(dplyr)
require(gridExtra)

##########################
# Data frame, sort, factorize
##########################
df.sort<-function(data,desc=T){
  data[,1]<-factor(data[,1],levels=lvl.sort(data,desc))
  return(data)
}

lvl.sort<-function(data,desc=T){
  grp.col<-which(grepl("group|grp",names(data),ignore.case = T))
  if(length(grp.col)>0){
    first.grp<-data[1,grp.col]
    data<-data[data[,grp.col]==first.grp,]
  }
  if(!desc)data[,2]<--data[,2]
  lvl<-as.character(data[order(data[,2]),1])
  return(lvl)
}




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
  age.group$Perc<-age.group$Count/sum(age.group$Count)*100
  age.group$Elderly<-elderly.group(cutoff,elderly)
  age.group$cnt.lab<-formatC(age.group$Count,big.mark=",")
  g<-ggplot(data=age.group)+
    geom_bar(mapping=aes(x=Age,y=Perc,fill=Elderly),stat="identity")+
    geom_text(mapping=aes(x=Age,y=Perc,label=cnt.lab,col=Elderly),vjust=-.5)+
    ggtitle("Age")+ylab("Proportion(%)")+ylim(0,max(age.group$Perc*1.2))+
    theme(legend.position = "bottom",
          axis.title.x = element_text(size=12,face="bold"),
          plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  return(g)
}


age.vis.comp<-function(
  age.n.grp,   # data.frame(age,group)
  cutoff=c(0,15+0:7*10),
  elderly=65
){
  names(age.n.grp)<-c("age","group")
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
    age.group$Perc<-age.group$Count/sum(age.group$Count)*100
    age.group$Elderly<-elderly.group(cutoff,elderly)
    age.group$cnt.lab<-formatC(age.group$Count,big.mark=",")
    age.group$Group<-grp
    return(age.group)
  })
  age.group.sum<-do.call(rbind.data.frame,age.group.list)
  age.group.sum$Group<-factor(age.group.sum$Group)
  
  g<-ggplot()+
    geom_bar(data=age.group.sum,mapping=aes(x=Age,y=Perc,group=Group,fill=Group,color=Elderly),size=1.1,stat="identity",position="dodge")+
    geom_text(data=age.group.sum,mapping=aes(x=Age,y=Perc,label=cnt.lab,col=Elderly,group=Group),vjust=-.5,position=position_dodge(width=1))+
    scale_colour_manual(values=c("#8B7355", "#66CDAA"))+
    ggtitle("Age")+ylab("Proportion(%)")+ylim(0,max(age.group.sum$Perc*1.2))+
    theme(legend.position = "bottom",
          axis.title.x = element_text(size=12,face="bold"),
          plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  return(g)
}


##########################
# Gender/Race/oth. Categorical Variables Visualization
##########################
cat.vis<-function(data,Title){
  grp.col<-grepl("grp|group",names(data),ignore.case = T)
  if(any(grp.col)){
    data<-cbind(data[,!grp.col],group=data[,grp.col])
    cat.vis.comp(data,Title)
  }else{
    cat.vis.sing(data,Title)
  }
}


cat.vis.sing<-function(data,Title){
  if(length(unique(data[,1]))>3){
    cat.vis.bar.sing(data,Title)
  }else{
    cat.vis.donut.sing(data,Title)
  }
}

cat.vis.comp<-function(data,Title){
  if(length(unique(data[,1]))>3){
    cat.vis.bar.comp(data,Title)
  }else{
    cat.vis.donut.comp(data,Title)
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
  data, #data.frame(Category,Count,Group)
  Title
){
  names(data)<-c("Category","Count","Group")
  groupsum<-data%>%
    group_by(Group)%>%
    summarize(grpsum=sum(Count))
  data<-left_join(data,groupsum)
  data$Perc<-data$Count/data$grpsum*100
  data$Group<-factor(data$Group,levels=c(unique(as.character(data$Group))," ","  ","   "))
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
  data, #data.frame(Category,Count,Group)
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

####################################################
# Chronic Condition Prevalence Visualization
####################################################
Prev.vis<-function(
  prev.df,  # data.frame(Disease, Prevalence, Std)
  Title="Disease Prevalence"
){
  if(any(grepl("grp|group",names(prev.df),ignore.case = T))){
    Prev.vis.comp(prev.df,Title)
  }else{
    Prev.vis.sing(prev.df,Title)
  }
}


Prev.vis.sing<-function(
  prev.df,  # data.frame(Disease, Prevalence, Std)
  Title="Disease Prevalence"
){
  g<-ggplot()+
    geom_bar(data=prev.df,mapping=aes(x=Disease,y=Prevalence),stat="identity",fill="steelblue")+
    coord_flip()+
    ggtitle(Title)+
    theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  return(g)
}


Prev.vis.comp<-function(
  prev.df,  # data.frame(Disease, Prevalence, Group)
  Title="Disease Prevalence"
){
  
  names(prev.df)<-c("Disease","Prevalence","Group")
  prev.df$Group<-as.factor(prev.df$Group)
  g<-ggplot()+
    geom_bar(data=prev.df,mapping=aes(x=Disease,y=Prevalence,fill=Group),position="dodge",stat="identity")+
    #geom_errorbar(data=prev.df,mapping=aes(x=Disease,ymin=prev.min, ymax=prev.max),color="#F08080",width=.2)+
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







