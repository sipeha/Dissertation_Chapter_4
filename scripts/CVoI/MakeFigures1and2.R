#Script for processing Round 2 elicited CVOI values
#TODO: check this: makes Figure 1 and figure 2

library(here);library(ggplot2);library(ggrepel);library(ggstance);
library(scales); library(cowplot)
#read in csv of responses
Round2<-read.csv(here("data/CVoI/CVOI Round 2_Anon.csv"))
#add in expert number
n_ex<-length(unique(Round2$Expert))
Round2$expertnum<-paste0("Ex", rep(1:n_ex, each=22))
Round2$Relevanceboth<-((Round2$Relevance.A*Round2$Relevance.B)-0)/(4-0)
Round2$cvoi<-Round2$Magnitude*Round2$Relevanceboth

kk <- ifelse(Round2$cvoi>median(Round2$cvoi),1,0) # Priority along qVOI
ww <- ifelse(Round2$Reducibility>median(Round2$Reducibility),1,0) # Priority along reducibility
Round2$Priority <- ifelse(kk==0&ww==0,1,ifelse(kk==1&ww==0,2,ifelse(kk==0&ww==1,3,4)))                            
Round2$Priority <- factor(Round2$Priority,levels=as.character(1:4))

Hypothesis<-unique(Round2$Hypothesis)
Round2wMeans<-Round2[1:22,]
Round2wMeans$Expert<-"All"
Round2wMeans$expertnum<-"mean"
cvoi.lci<-cvoi.uci<-red.lci<-red.uci<-reda.l<-reda.u<-redb.l<-redb.u<-numeric(length(Hypothesis))

for(i in 1:length(unique(Round2$Hypothesis))){
  Round2wMeans$Magnitude[i]<-sum(Round2$Magnitude[which(Round2$Hypothesis==Hypothesis[i])])/n_ex
  Round2wMeans$Relevance.A[i]<-sum(Round2$Relevance.A[which(Round2$Hypothesis==Hypothesis[i])])/n_ex
  Round2wMeans$Relevance.B[i]<-sum(Round2$Relevance.B[which(Round2$Hypothesis==Hypothesis[i])])/n_ex
  Round2wMeans$Reducibility[i]<-sum(Round2$Reducibility[which(Round2$Hypothesis==Hypothesis[i])])/n_ex
  Round2wMeans$Relevanceboth[i]<-sum(Round2$Relevanceboth[which(Round2$Hypothesis==Hypothesis[i])])/n_ex
  Round2wMeans$cvoi[i]<-sum(Round2$cvoi[which(Round2$Hypothesis==Hypothesis[i])])/n_ex
  cvoi.lci[i]<-as.numeric(quantile((Round2$cvoi[which(Round2$Hypothesis==Hypothesis[i])]), 0.05))
  cvoi.uci[i]<-as.numeric(quantile((Round2$cvoi[which(Round2$Hypothesis==Hypothesis[i])]), 0.95))
  red.lci[i]<-as.numeric(quantile((Round2$Reducibility[which(Round2$Hypothesis==Hypothesis[i])]), 0.05))
  red.uci[i]<-as.numeric(quantile((Round2$Reducibility[which(Round2$Hypothesis==Hypothesis[i])]), 0.95))
}


R2SummaryDF <- data.frame(Hypothesis=unique(Round2$Hypothesis),
                          Label=paste0("H",unique(Round2$Hypothesis)),
                          CVOI.mean=Round2wMeans$cvoi,
                          CVOI.lci=cvoi.lci,
                          CVOI.uci=cvoi.uci,
                          Red.mean=Round2wMeans$Reducibility,
                          Red.lci=red.lci,
                          Red.uci=red.uci,
                          RelA=Round2wMeans$Relevance.A,
                          RelB=Round2wMeans$Relevance.B,
                          Mag=Round2wMeans$Magnitude,
                          RelAB=Round2wMeans$Relevanceboth)




hynum<-c(
  hcl.colors(n=7, palette = "Zissou 1"),
  hcl.colors(n=7, palette="PuRd")[1:5],
  hcl.colors(n=7, palette = "Batlow")[1:4],
  hcl.colors(n=7, palette="Viridis")[1:6])

show_col(hynum)

#make Figure 1
CVOI.plot_Figure1 <- ggplot(R2SummaryDF, aes(y=Red.mean,x=CVOI.mean,color=Hypothesis)) +
  geom_vline(aes(xintercept=median(CVOI.mean)),linetype='dashed',color='darkgrey',size=0.75)+
  geom_hline(aes(yintercept=median(Red.mean)),linetype='dashed',color='darkgrey',size=0.75)+
  geom_linerange(aes(ymin=Red.lci, ymax=Red.uci), alpha=1, linetype="solid")+
  geom_linerangeh(aes(xmin=CVOI.lci, xmax=CVOI.uci), alpha=1, linetype="solid")+
  geom_point(size=4,stroke=0,shape=16)+
  geom_label_repel(aes(label=Label))+
  scale_colour_manual(name="", values=hynum)+ 
  theme_bw()+
  theme(legend.position='none')+
  scale_y_continuous(name="Reducibility",breaks=seq(0,4,1), limits=c(-0.25,4.25))+
  scale_x_continuous(name="CVOI",breaks=seq(0,7,1), limits=c(-0.25,7))+
  geom_text(x=0, y=-0.3, label="Low priority",color="black")+
  geom_text(x=0, y=4.2, label="Medium priority",color="black")+
  geom_text(x=3.5, y=-0.3, label="High priority",color="black")+
  geom_text(x=3.5, y=4.2, label="Highest priority",color="black")
CVOI.plot_Figure1


ggsave(CVOI.plot_Figure1, filename = here("Tables and Figures/Figure 1.png"),  bg = "white",
       width = 9, height = 7, dpi = 300, units = "in", device='png')

#Figure 2
#Reducibility vs. Magnitude
CVOI.plot2 <- ggplot(R2SummaryDF, aes(y=Mag,x=Red.mean,color=Hypothesis)) +
  geom_point(size=3.5,stroke=0,shape=16)+
  geom_label_repel(aes(label=Label))+
  scale_colour_manual(name="", values=hynum)+ 
  labs(
    y="Magnitude of Uncertainty",x="Reducibility")+
  theme_bw()+
  theme(legend.position='none')+
  scale_y_continuous(name="Magnitude",breaks=seq(0,4,1), limits=c(-0.25,3.5))+
  scale_x_continuous(name="Reducibility",breaks=seq(0,3,1), limits=c(-0.25,3))


CVOI.plot2

ggsave(CVOI.plot_Figure1, filename = here("Tables and Figures/Figure 2.png"),  bg = "white",
       width = 9, height = 7, dpi = 300, units = "in", device='png')

