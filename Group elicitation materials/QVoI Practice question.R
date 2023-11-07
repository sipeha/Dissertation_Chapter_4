#plot by individual response

n.hyp <- 2

# install.packages('gsheet')
library(gsheet);library(mc2d);library(reshape2);library(dplyr);library(stringi);library(ggplot2);library(tidyverse)
# Take the qVOI estimates from participants (form results go into a google spreadsheet)
kk.v <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1_2eNWn7Q5gIc3lyuz_WbRfzEfL-93aiA13bMv1ATnVg/edit?resourcekey#gid=50717004')
# The same data are also in the example spreadsheet I attached to the email ;)

# Rename columns
colnames(kk.v) <- c("Time","Name",
                    paste0("Magnitude",1:n.hyp),
                    paste0("Relevance a",1:n.hyp),
                    paste0("Relevance b",1:n.hyp),
                    paste0("Reducibility",1:n.hyp))
# Reshape from wide to long
kk.l <- kk.v %>% pivot_longer(cols=3:ncol(kk.v),
                              names_to='Variable',
                              values_to='Score')
kk.l$Hypothesis <- as.factor(stri_sub(kk.l$Variable,-1,-1))
kk.l$Variable <- as.factor(stri_sub(kk.l$Variable,1,-2))
kk.l$Score <- as.numeric(stri_sub(kk.l$Score,1,1))

# Go back to wide format
qv.df <- kk.l %>%  pivot_wider(names_from = Variable, values_from = Score)
qv.df$relab<-qv.df$`Relevance a` *qv.df$`Relevance b`
qv.df$Relevance<-(qv.df$relab-0)/(4-0)
qv.df$qVOI <- qv.df$Magnitude*qv.df$Relevance
kk <- ifelse(qv.df$qVOI>median(qv.df$qVOI),1,0) # Priority along qVOI
ww <- ifelse(qv.df$Reducibility>median(qv.df$Reducibility),1,0) # Priority along reducibility
qv.df$Priority <- ifelse(kk==0&ww==0,1,ifelse(kk==1&ww==0,2,ifelse(kk==0&ww==1,3,4)))                            
qv.df$Priority <- factor(qv.df$Priority,levels=as.character(1:4))
# Order by name and convert to anonymous ID
qv.df <- qv.df[order(qv.df$Name),-1]
qv.df$ID <- as.numeric(as.factor(qv.df$Name))
qv.df$label<-paste0("Ex",qv.df$ID)

# Summary data frame
qv.summary <- data.frame(Hypothesis=as.factor(1:nlevels(qv.df$Hypothesis)),
                         Label=paste0("H",1:nlevels(qv.df$Hypothesis)),
                         qVOI.mean=tapply(qv.df$qVOI,qv.df$Hypothesis,mean),
                         qVOI.lci=tapply(qv.df$qVOI,qv.df$Hypothesis,quantile,probs=0.025),
                         qVOI.uci=tapply(qv.df$qVOI,qv.df$Hypothesis,quantile,probs=0.975),
                         Red.mean=tapply(qv.df$Reducibility,qv.df$Hypothesis,mean),
                         Red.lci=tapply(qv.df$Reducibility,qv.df$Hypothesis,quantile,probs=0.025),
                         Red.uci=tapply(qv.df$Reducibility,qv.df$Hypothesis,quantile,probs=0.975))

# qVOI plot
library(ggplot2);library(RColorBrewer);library(grid);library(Cairo);library(ggrepel)
my.cols <- brewer.pal(nrow(qv.summary),"Set1")
# Create plot

HypoDF1<-qv.df[which(qv.df$Hypothesis==1),]
qVoiplotbyexpert1<-ggplot(HypoDF1, aes(y=qVOI, x=Reducibility, color=label))+
  geom_point(size=3.5,stroke=0,shape=16)+
  geom_label_repel(aes(label=label))+
  theme(legend.position="none")+
  theme_bw()
qVoiplotbyexpert1
HypoDF2<-qv.df[which(qv.df$Hypothesis==2),]
qVoiplotbyexpert2<-ggplot(HypoDF2, aes(y=qVOI, x=Reducibility, color=label))+
  geom_point(size=3.5,stroke=0,shape=16)+
  geom_label_repel(aes(label=label))+
  theme(legend.position="none")+
  theme_bw()
qVoiplotbyexpert2

HypoDF1<-qv.df[which(qv.df$Hypothesis==1),]
qVoiplotbyexpert1<-ggplot(HypoDF1, aes(y=qVOI, x=Reducibility, color=ID))+
  geom_point(size=3.5,stroke=0,shape=16)+
  geom_label_repel(aes(label=ID))+
  theme(legend.position="none")+
  theme_bw()
qVoiplotbyexpert1


HypoDF2<-qv.df[which(qv.df$Hypothesis==2),]
qVoiplotbyexpert2<-ggplot(HypoDF2, aes(y=qVOI, x=Reducibility, color=ID))+
  geom_point(size=3.5,stroke=0,shape=16)+
  geom_label_repel(aes(label=ID))+
  theme(legend.position="none")+
  theme_bw()
qVoiplotbyexpert2




qVOI.plot <- ggplot(qv.summary, aes(y=qVOI.mean,x=Red.mean,color=Hypothesis)) +
  geom_vline(aes(xintercept=median(Red.mean)),linetype='dashed',color='blue',size=0.3)+
  geom_hline(aes(yintercept=median(qVOI.mean)),linetype='dashed',color='blue',size=0.3)+
  geom_errorbarh(aes(xmin=Red.lci, xmax=Red.uci),color="grey", position="identity", height = 0, size=0.5)+
  geom_errorbar(aes(ymin=qVOI.lci, ymax=qVOI.uci),color="grey", position="identity", width = 0, size=0.5)+
  geom_point(size=3.5,stroke=0,shape=16)+
  geom_label_repel(aes(label=Label))+
  scale_colour_manual(name="",values=my.cols)+
  theme_minimal()+
  theme(legend.position='none',
        plot.background = element_blank(),
        panel.grid.major = element_line(size=0.5),
        panel.grid.minor = element_line(size=0.1),
        axis.title.y=element_text(size=12, color="black"),
        axis.title.x=element_text(vjust=-1.2, size=12, color="black"),
        axis.ticks.length=unit(0.5,"mm"),
        axis.text = element_text(size=9),
        plot.margin=unit(c(5,5,5,5), "mm"),
        panel.spacing = unit(2, "lines")
  )+
  scale_x_continuous(name="Reducibility",breaks=seq(0,4,1), limits=c(-1,5))+
  scale_y_continuous(name="qVOI",breaks=seq(0,12,4), limits=c(-2,14))+
  geom_text(x=-0.5, y=-1, label="Low")+
  geom_text(x=-0.5, y=13, label="Medium")+
  geom_text(x=4, y=-1, label="High")+
  geom_text(x=4, y=13, label="Highest")

qVOI.plot

ggsave(qVOI.plot, filename = "qVOI_plot.png",  bg = "white",
       width = 12, height = 6, dpi = 300, units = "in", device='png')


