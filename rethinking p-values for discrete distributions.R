#written by Uri Simonsohn (urisohn@gmail.com)
#2023 07 08

#This script is used to argue that Richard McElreath plot of 
#a p-value distribution for a discrete test is being misintepreted

#OUTLINE
#1 preliminaries
#2 Simulation 1 - All samples same size, as Rethinking author did
#3 Simulation 2 - Samples size differs
#4 Summarize results for Simulation 1
#5 Summarize results for Simulation 2
#6 make the plot    

#==============================================================

#1 preliminaries

#Clear eveyrthing
  rm(list = ls())

#Load pkgs for paralle processing
  library('groundhog')
  pkgs <- c('foreach','doParallel')
  groundhog.library(pkgs,'2023-04-01')

#Use all but 1 core
  stopImplicitCluster()
  registerDoParallel(cores=detectCores()-1)
  #==============================================================

  
#Simulation 1 - All samples same size, as Rethinking author did
  N=100 #Sample size used by 
  
  #Number of simulations
    simtot=50000
  
   #50% are control, 50% treatment
  
    x=rep(c(0,1),N/2) 
  
 #Start the loop
  p.all = foreach(simk=1:simtot,.combine='c') %dopar% {
    #Simulated data lead to a p-value saved onto p.all() vector
    
    #Set seed in each loop
      set.seed(1000+simk)
    
    #Generate y
      y=rbinom(100,1,.85)
      
    #Run logit
     g = glm( y ~ x , family=binomial ) 
     
    #Save p-value
      p =summary(g)$coefficients[2,4]
      
    #Counter in text file to see progress, optional
      #if (simk==1) write(paste0(simk," ",Sys.time()), "c:/temp/COUNTER.txt",append=FALSE)
      #if (simk%%1000==0) write( paste0(simk," ",Sys.time()), "c:/temp/COUNTER.txt",append=TRUE)

    #To ouput the result and save it, we just do this:
      p 
  }

#----------------------------------------------------------
  
#Simulation 2 - Samples size differs
  simtot=50000
  p.all2 = foreach(simk=1:simtot,.combine='c') %dopar% {
    
    #We have two samples, each between 45 and 55 people
    #this produces random variation across simulations that lead to a smooother discrete distribution
      n1=sample(45:55,size=1)
      n2=sample(45:55,size=1)
      
    #Draw the xs
      x=rep(c(1,0),c(n1,n2))
      N=n1+n2
      
    #Draw the ys
      prop=runif(1,.8,.9) #I also make the pop rate to vary, but this probably does not matter
      set.seed(1000+simk)
      y=rbinom(N,1,prop)
  
       g = glm( y ~ x , family=binomial ) 
      p =summary(g)$coefficients[2,4]
    #Counter is optional
      #if (simk==1) write(paste0(simk," ",Sys.time()), "c:/temp/COUNTER.txt",append=FALSE)
      #if (simk%%1000==0) write( paste0(simk," ",Sys.time()), "c:/temp/COUNTER.txt",append=TRUE)
      p 
  }
  
  
#-----------------------------------------------------
#4 Summarize results for Simulation 1
  
    cdf=c()
    p.all=round(p.all,3)   #round to speed things up, R struggle plotting too many values
    p.unique=unique(p.all)
    k=0
    for (pk in p.unique)
    {
      k=k+1
      cdf[k]=mean(p.all <= pk)  - mean(p.all == pk)/2  
      
    }
#5 Summarize results for Simulation 2

    cdf2=c()
    p.all2=round(p.all2,3)   #round to speed things up
    p.unique2=unique(p.all2)
    k=0
    for (pk in p.unique2)
    {
      k=k+1
      cdf2[k]=mean(p.all2 <= pk)  - mean(p.all2 == pk)/2  
      
    }

#--------------------------------------------------------------
#6 make the plot    
    
  svg("c:/temp/discrete p.svg",width=20,height=6)
    par(mfrow=c(1,3))
  par(mar=c(6.1,6.1,5.1,2.1))

  
#Plot A
  hist(p.all,main='',xlab='',ylab='',col='dodgerblue')
  mtext(side=3,line=2.5,font=2,'Somewhat misleading histogram shown by McElreath',cex=1.5)
  mtext(side=3,line=.75,font=3,'(p-values seem crazily distributed)',cex=1.5,col='red4')
  mtext(side=1,line=3,font=2,cex=1.5,"p-value")
  mtext(side=2,line=3,font=2,cex=1.5,"Frequency")
  
#Plot B
  plot(p.unique,cdf,ylim=c(0,1),pch=16,cex=2,col=adjustcolor('blue4',.25),xaxt='n',yaxt='n',xlab='',ylab='')
  points(p.unique,p.unique,type='l',col='red4')
   mtext(side=1,line=3,font=2,cex=1.5,"p-value")
  mtext(side=2,line=3,font=2,cex=1.5,"CDF - Cumulative Relative Frequency")
  yt=c(.05,.25,.5,.75,.9)
  axis(side=2,at= yt, paste0(yt*100,"%"),las=1)
  legend(cex=2,'topleft',inset=.02,pch=c(16,NA),lty=c(NA,1),col=c('blue4','red4'),c('Observed frequency',"Expected if test is valid"))
  mtext(side=3,line=2.5,font=2,'Looking at CDF it looks much better',cex=1.5)
  mtext(side=3,line=0.3,font=3,'(histograms of discrete values are misleading)',cex=1.5,col='red4')

  
#Plot C
  plot(p.unique2,cdf2,ylim=c(0,1),pch=16,cex=2,col=adjustcolor('blue4',.25),xaxt='n',yaxt='n',xlab='',ylab='')
  points(p.unique,p.unique,type='l',col='red4')
   mtext(side=1,line=3,font=2,cex=1.5,"p-value")
  mtext(side=2,line=3,font=2,cex=1.5,"CDF - Cumulative Relative Frequency")
  yt=c(.05,.25,.5,.75,.9)
  axis(side=2,at= yt, paste0(yt*100,"%"),las=1)
  legend(cex=2,'topleft',inset=.02,pch=c(16,NA),lty=c(NA,1),col=c('blue4','red4'),c('Observed frequency',"Expected if test is valid"))
  mtext(side=3,line=2.5,font=2,'Relaxing assumption that every study is N=100',cex=1.5)
  mtext(side=3,line=0.3,font=3,"(instead 90<N<110--> p's Looks Perfect)",cex=1.5,col='red4')
  
  
  
  
  dev.off()
  

