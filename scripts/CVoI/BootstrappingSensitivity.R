#Script for conducting bootstrapping sensitivity analysis for 
#hypotheses

library(here);library(ggplot2);library(ggrepel);library(ggstance);
library(dplyr); library(scales)

#Bootstrapping results and sensitivity analysis
#load in Round 2
Round2All<-read.csv(here("data/CVoI/Round2All_Anon.csv"))
#add in expert number
Round2<-Round2All[-which(Round2All$expertnum=="mean"),]

#Bootstrapping:
#For each component over many iterations
# s is the number of samples
# X is the sampled vector, mu is the mean, lower and higher are the 95% conf intervals
S<-1000
n_ex<-length(unique(Round2$Expert))
Hypos<-unique(Round2$Hypothesis)
X_mag<-X_RelA<-X_RelB<-X_Red<-array(dim=c(S,n_ex, length(Hypos)))
mu_mag<-mu_RelA<-mu_RelB<-mu_Red<-matrix(nrow=S, ncol=length(Hypos))

RelABbs<-CVoIbs<-matrix(nrow=S, ncol=length(Hypos))
Red.s.l<-Red.s.u<-CVoI.l<-CVoI.u<-length(Hypos)
mag.l<-mag.u<-rela.l<-rela.u<-relb.l<-relb.u<-relab.l<-relab.u<-length(Hypos)
se.mag<-se.rela<-se.relb<-se.rel<-se.cvoi<-se.red<-length(Hypos)
mag_bar<-Rela_bar<-Relb_bar<-Relab_bar<-CVOI_bar<-Red_bar<-length(Hypos)
var_mag<-var_rela<-var_relb<-var_rel<-var_cvoi<-var_red<-length(Hypos)

for(h in 1:length(Hypos)){
  for(s in 1:S){
    X_mag[s,,h]<-(sample(Round2$Magnitude[which(Round2$Hypothesis==Hypos[h])][1:n_ex],n_ex, replace=T))
    X_RelA[s,,h]<-(sample(Round2$Relevance.A[which(Round2$Hypothesis==Hypos[h])][1:n_ex],n_ex, replace=T))
    X_RelB[s,,h]<-(sample(Round2$Relevance.B[which(Round2$Hypothesis==Hypos[h])][1:n_ex],n_ex, replace=T))
    X_Red[s,,h]<-(sample(Round2$Reducibility[which(Round2$Hypothesis==Hypos[h])][1:n_ex],n_ex, replace=T))
    mu_mag[s,h]<-mean(X_mag[s,,h])
    mu_RelA[s,h]<-mean(X_RelA[s,,h])
    mu_RelB[s,h]<-mean(X_RelB[s,,h])
    mu_Red[s,h]<-mean(X_Red[s,,h])
    RelABbs[s,h]<-(mu_RelA[s,h]*mu_RelB[s,h])/4
    CVoIbs[s,h]<-RelABbs[s,h]*mu_mag[s,h]
  }
  mag_bar[h]<-sum(mu_mag[,h])/S
  Rela_bar[h]<-sum(mu_RelA[,h])/S
  Relb_bar[h]<-sum(mu_RelB[,h])/S
  Relab_bar[h]<-sum(RelABbs[,h])/S
  CVOI_bar[h]<-sum(CVoIbs[,h])/S
  Red_bar[h]<-sum(mu_Red[,h])/S
  
  var_mag[h]<-(1/(S-1))*sum((mu_mag[,h]-mag_bar[h])^2)
  var_rela[h]<-(1/(S-1))*sum((mu_RelA[,h]-Rela_bar[h])^2)
  var_relb[h]<-(1/(S-1))*sum((mu_RelB[,h]-Relb_bar[h])^2)
  var_rel[h]<-(1/(S-1))*sum((RelABbs[,h]-Relab_bar[h])^2)
  var_cvoi[h]<-(1/(S-1))*sum((CVoIbs[,h]-CVOI_bar[h])^2)
  var_red[h]<-(1/(S-1))*sum((mu_Red[,h]-Red_bar[h])^2)
}

Round2means<-Round2All[which(Round2All$expertnum=="mean"),]


#Make Table 5
Table5<-data.frame(Hypothesis=Round2means$Hypothesis,
                          Mag=paste0(formatC(Round2means$Magnitude, digits=2, format="f"),
                                     " (", formatC(sqrt(var_mag), digits=2, format="f"),")"),
                          RelA= paste0(formatC(Round2means$Relevance.A, digits=2, format="f"),
                                       " (", formatC(sqrt(var_rela), digits=2, format="f"),")"),
                          RelB=paste0(formatC(Round2means$Relevance.B, digits=2, format="f"),
                                      " (", formatC(sqrt(var_relb), digits=2, format="f"),")"), 
                          RelBoth=paste0(formatC(Round2means$Relevanceboth, digits=2, format="f"),
                                         " (", formatC(sqrt(var_rel), digits=2, format="f"),")"), 
                          CVoI=paste0(formatC(Round2means$cvoi, digits=2, format="f"),
                                      " (", formatC(sqrt(var_cvoi), digits=2, format="f"),")"), 
                          Red=paste0(formatC(Round2means$Reducibility, digits=2, format="f"),
                                     " (", formatC(sqrt(var_red), digits=2, format="f"),")"))
write.csv(Table5, file=here("Tables and Figures/Table5.csv"))


#Make Table 6
#probability/proportion that fell in highest priority quadrant
e2e_sensitivity<-matrix(nrow=(length(Hypos)), ncol=4)
for(i in 1:length(Hypos)){
  e2e_sensitivity[i,1]<-length(which(CVoIbs[,i]>=median(Round2means$cvoi) & mu_Red[,i]>=median(Round2means$Reducibility)))/1000
  e2e_sensitivity[i,2]<-length(which(CVoIbs[,i]>=median(Round2means$cvoi) & mu_Red[,i]<median(Round2means$Reducibility)))/1000
  e2e_sensitivity[i,3]<-length(which(CVoIbs[,i]<median(Round2means$cvoi) & mu_Red[,i]>=median(Round2means$Reducibility)))/1000
  e2e_sensitivity[i,4]<-length(which(CVoIbs[,i]<median(Round2means$cvoi) & mu_Red[,i]<median(Round2means$Reducibility)))/1000
  
}
rowSums(e2e_sensitivity)

#Write Table 6
Table6<-data.frame(Hypothesis=Round2means$Hypothesis,
                               Highest=paste0(formatC(e2e_sensitivity[,1]*100, digits=1, format="f"),
                                              " %"),
                               High=paste0(formatC(e2e_sensitivity[,2]*100, digits=1, format="f"),
                                           " %"),
                               Medium=paste0(formatC(e2e_sensitivity[,3]*100, digits=1, format="f"),
                                             " %"),
                               Low=paste0(formatC(e2e_sensitivity[,4]*100, digits=1, format="f"),
                                          " %"))
write.csv(Table6, file=here("Tables and Figures/Table6.csv"))

