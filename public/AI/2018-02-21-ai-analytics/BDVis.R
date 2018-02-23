##########################################################################
##########################################################################

# Title: AI Analysis - Visualization
# Description: Univariate visualization & Comparison
## Functions ===
##	col.aggr	45
##	getcolumn	50
##	cols.aggr	56
##	cols.aggr.sing	74
##	df.sort	88
##	lvl.sort	93
##	vec.cumu	108
##	find.cumu.mid	112
##	cutoff2label	120
##	elderly.group	129
##	age.vis	140
##	age.vis.sing	154
##	age.vis.comp	184
##	cat.vis	226
##	cat.vis.sing	237
##	cat.vis.comp	245
##	cat.vis.donut.sing	253
##	cat.vis.donut.comp	282
##	cat.vis.bar.sing	320
##	cat.vis.bar.comp	342
##	Prev.vis	374
##	Prev.vis.sing	386
##	Prev.vis.comp	404
## ===
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
cols.aggr<-function(data,prev=T){
  grp.col<-which(grepl("group|grp",names(data),ignore.case = T))
  if(length(grp.col)>0){
    grp.list<-unique(data[,grp.col])
    prev.list<-lapply(grp.list,function(grp){
      df<-data[data[,grp.col]==grp,-grp.col]
      rslt<-cols.aggr.sing(df,prev)
      rslt$group<-grp
      return(rslt)
    })
    prev.df<-do.call(rbind.data.frame,prev.list)
    return(prev.df)
  }else{
    prev.df<-cols.aggr.sing(data,prev)
    return(prev.df)
  }
}

cols.aggr.sing<-function(df,prev=T){
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
# find cumu mid for donut
##########################

vec.cumu<-function(x){
  sapply(1:length(x),function(a)sum(x[1:a]))
}

find.cumu.mid<-function(x){
  vec.cumu(x)-x/2
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
  data$Perc<-data$Count/sum(data$Count)*100
  data$cumu.mid<-find.cumu.mid(data$Perc)
  data$Category<-factor(data$Category,levels=data$Category)
  data$cnt.lab<-formatC(data$Count,big.mark = ",")
  data$label.perc<-paste(data$cnt.lab,"(",sprintf("%.1f",data$Perc),"%)",sep="")
  
  g<-ggplot() +
    geom_bar(data=data, mapping=aes(x=3.5,y=Perc,fill=Category),stat="identity") +
    geom_text(data=data,mapping=aes(x=3.5,y=100-cumu.mid,label=label.perc,angle=cumu.mid/100*360),col="Gray30",fontface = "bold")+
    coord_polar(theta="y") +
    xlab("")+ylab("")+ggtitle(Title)+
    xlim(c(0, 4)) +
    scale_fill_discrete(name="")+
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
  grplist<-unique(data$Group)
  grpsum.list<-lapply(grplist,function(grp){
    data.sub<-data[data$Group==grp,]
    data.sub$Perc<-data.sub$Count/sum(data.sub$Count)*100
    data.sub$cumu.mid<-find.cumu.mid(data.sub$Perc)
    data.sub$Category<-as.factor(data.sub$Category)
    data.sub$cnt.lab<-formatC(data.sub$Count,big.mark=",")
    data.sub$label.perc<-paste(data.sub$cnt.lab,"(",sprintf("%.1f",data.sub$Perc),"%)",sep="")
    return(data.sub)
  })
  data<-do.call(rbind.data.frame,grpsum.list)
  
  data$Group<-factor(data$Group,levels=c(unique(as.character(data$Group))," ","  ","   "))
  
  g<-ggplot() +
    geom_bar(data=data, mapping=aes(x=Group,y=Perc,fill=Category),stat="identity") +
    geom_text(mapping=aes(x=levels(data$Group),y=0,label=levels(data$Group)),col="Gray30",fontface = "bold")+
    geom_text(data=data,mapping=aes(x=Group,y=100-cumu.mid,label=label.perc,angle=cumu.mid/100*360),col="Gray30",fontface = "bold")+
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
  data$cnt.lab<-formatC(data$Count,big.mark=",")
  
  g<-ggplot(data=data) +
    geom_bar(mapping=aes(x=Category,y=Perc,fill=Category),width=.5,stat="identity") +
    geom_text(mapping=aes(x=Category,y=Perc,label=cnt.lab,col=Category),vjust=-.5)+
    xlab("")+ylab("Proportion(%)")+ggtitle(Title)+
    scale_fill_discrete(name="")+scale_colour_discrete(guide=F)+
    theme(legend.position = "bottom",
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  return(g)
}

cat.vis.bar.comp<-function(
  data, #data.frame(Category,Count,Group)
  Title
){
  names(data)<-c("Category","Count","Group")
  grplist<-unique(data$Group)
  grpsum.list<-lapply(grplist,function(grp){
    data.sub<-data[data$Group==grp,]
    data.sub$Perc<-data.sub$Count/sum(data.sub$Count)*100
    data.sub$Category<-as.factor(data.sub$Category)
    data.sub$cnt.lab<-formatC(data.sub$Count,big.mark=",")
    return(data.sub)
  })
  data<-do.call(rbind.data.frame,grpsum.list)
  data$Group<-as.factor(data$Group)
  data$Category<-as.factor(data$Category)
  
  g<-ggplot(data=data) +
    geom_bar(mapping=aes(x=Category,y=Perc,fill=Group),width=.5,stat="identity",position="dodge") +
    geom_text(mapping=aes(x=Category,y=Perc,label=cnt.lab,col=Group),vjust=-.5,position=position_dodge(width=.5))+
    xlab("")+ylab("Proportion(%)")+ggtitle(Title)+
    scale_fill_discrete(name="")+
    scale_colour_discrete(guide = FALSE)+
    theme(legend.position = "bottom",
          axis.title = element_text(size = 12, face = "bold"),
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
  prev.df,  # data.frame(Disease, Prevalence)
  Title="Disease Prevalence"
){
  prev.df$Prevalence<-prev.df$Prevalence*100
  prev.df$Perc.lab<-paste(sprintf("%.1f",prev.df$Prevalence),"%",sep="")
  g<-ggplot(data=prev.df)+
    geom_bar(mapping=aes(x=Disease,y=Prevalence),stat="identity",fill="steelblue")+
    geom_text(mapping=aes(x=Disease,y=Prevalence,label=Perc.lab),col="steelblue",hjust=-.2,stat="identity")+
    coord_flip()+ylim(0,105)+ylab("Prevalence(%)")+
    ggtitle(Title)+
    theme(legend.position = "bottom",
          axis.title = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  return(g)
}


Prev.vis.comp<-function(
  prev.df,  # data.frame(Disease, Prevalence, Group)
  Title="Disease Prevalence"
){
  
  names(prev.df)<-c("Disease","Prevalence","Group")
  prev.df$Group<-as.factor(prev.df$Group)
  prev.df$Prevalence<-prev.df$Prevalence*100
  prev.df$Perc.lab<-paste(sprintf("%.1f",prev.df$Prevalence),"%",sep="")
  g<-ggplot(data=prev.df)+
    geom_bar(mapping=aes(x=Disease,y=Prevalence,fill=Group),position="dodge",stat="identity")+
    geom_text(mapping=aes(x=Disease,y=Prevalence,label=Perc.lab,color=Group),hjust=-.2,position=position_dodge(1),stat="identity")+
    scale_color_discrete(guide=F)+
    coord_flip()+ylim(0,105)+ylab("Prevalence(%)")+
    ggtitle(Title)+
    theme(legend.position = "bottom",
          axis.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
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







