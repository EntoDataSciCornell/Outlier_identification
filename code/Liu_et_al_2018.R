#	investigate some outlier detection methods used in cleaning species distribution data
#	including: outlier & presence-only modelling with randomForest;
#	dd.plot with mvoutlier;
#	OutlierMahdist & OutlierPCOut & OutlierSign2 with rrcovHD;
#	ksvm (both one-class and two-classes) with kernlab;
#	Isolation forest with R package IsolationForest;
#	local outlier factor with R package Rlof.

#	For method 5, we output the results corresponding to maxTSS;
#	For methods 1 - 5, we also output (tss+1)/2 as auc;
#	for methods 6-10, we output the results corresponding to maxTSS as well as AUC.
#	source("\\\\ari-vm-00\\SpatialStorage\\SAN_Projects\\liu\\U_copy\\accudx\\outlier_detection-8-3.r")

nseed = 1009
set.seed(nseed)


##################################################################################################

# make confusion table using a threshold

cftb<-function(prd0,prd1,c) {
  n0<-length(prd0)
  n1<-length(prd1)
  tp<-0
  fp<-0 
  tn<-0
  fn<-0
  
  for (i in 1:n0) {
    if (prd0[i] >= c) fp<-fp+1 else tn<-tn+1
  }
  for (i in 1:n1) {
    if (prd1[i] >= c) tp<-tp+1 else fn<-fn+1
  }
  c(tp,fp,fn,tn)
}

##########################################################################################################


#	calculate maxTSS and related metrics (npo and nro) using true presence and true absence data.


dxmx<-function(prd0,prd1) {
  
  prd<-c(prd0,prd1)
  n<-length(prd)
  p<-sort(prd)
  n1 <- n-1
  
  mxss<- -9999
  
  for (i in 1:n1) {
    c<-(p[i]+p[i+1])/2
    px<-cftb(prd0,prd1,c)
    
    tp<-px[1]
    fp<-px[2]
    fn<-px[3]
    tn<-px[4]
    n0<-fp+tn
    p0<-tp+fn
    
    se<-tp/p0				
    sp<-tn/n0
    
    ss<-se+sp				
    
    if (ss > mxss) {
      mxss<-ss
      nro<-tp
      npo<-tp+fp
    }
    
    
  }
  
  
  return(c(mxss-1,npo,nro))	
  
}

##########################################################################################################

# 	nonparametric estimate of AUC

roc <- function(prd0,prd1) {
  
  n0<-length(prd0)
  n1<-length(prd1)
  au<-0
  for (i in 1:n1) {
    for (j in 1:n0) {
      if (prd1[i] > prd0[j]) {
        au<-au+1
      } else {
        if (prd1[i] == prd0[j]) {
          au<-au+0.5
        }
      }
    }
  }
  au<-au/(n0*n1)
  #	q1<-au/(2-au)
  #	q2<-2*au*au/(1+au)
  #	std<-sqrt((au*(1-au)+(n1-1)*(q1-au*au)+(n0-1)*(q2-au*au))/(n1*n0))	# std error of auc
  
  return(au)
  
}

################################################################################################################


#	modelling with Random Forest for prediction

#	trn: training data without the column for occurrence with first n1trn rows of presences and last n0trn rows of absences
#	ap:  application without the column for occurrence.

rfst<-function(trn,n1trn,n0trn,ap,ntree) {
  
  #	library(randomForest)	### it was moved to the main program before this function
  
  species<-as.factor(c(rep(1,n1trn),rep(0,n0trn)))
  
  f <- randomForest(species ~ ., data=trn,ntree=ntree)
  
  prd<-predict(f,ap,type="prob")[,2]
  
  prd
  
}

###########################################################################################################################################

#	main program

library(kernlab)
library(rrcovHD)
library(mvoutlier)
library(randomForest)
#install.packages("IsolationForest", repos="http://R-Forge.R-project.org")
library(IsolationForest)
library(Rlof)


dir <- "\\\\ari-vm-00\\SpatialStorage\\SAN_Projects\\liu\\U_copy\\accudx\\pres_only\\outlier_detect\\"

fln_dx <- paste(dir,"outlier-dx-8-6.txt",sep="")


x0 <- read.table(file="\\\\ari-vm-00\\SpatialStorage\\SAN_Projects\\liu\\U_copy\\accudx\\pres_only\\predictors.txt",header=T,as.is=T)
sv1 <- read.table(file="\\\\ari-vm-00\\SpatialStorage\\SAN_Projects\\liu\\U_copy\\accudx\\pres_only\\survey.txt",header=T,as.is=T)

cat(file=fln_dx,"prv","nv","prop_outlier","nrecd","sm","nptm","ntree","bias_id","sv_id","rltn","method","nnm","nab","npo","nro","Se","Sp","tss","auc","\n")

rptm <- 2
ntree <- 2000
nsm <- 300
prop_out <- c(0.01,0.05,0.1)	# proportion of outliers

#nv1 <- 18	# nv1 = nv0, all environmental variables (except coordinates) used in the analysis
nv2 <- 5	# only a subset of 5 environmental variables (except coordinates) used in the analysis

x0 <- x0[,-c(1:2)]
nt <- dim(x0)[1]
nv0 <- dim(x0)[2]
idt <- 1:nt

x0<-as.matrix(x0)
x0<-scale(x0)

idsv<-idt[sv1>0]		# survey locations

rm(sv0)

nv1 <- nv0



for (sm in 1:nsm) {
  
  cat("sm",sm,"\n")
  
  
  
  for (nv in c(nv2,nv1)) {
    
    cat("nv",nv,"\n")
    
    if (nv == nv1) {
      vid <- 1:nv1
    } else {
      vid <- sample(1:nv0,nv2,replace=FALSE)
    }
    
    x <- x0[,vid]
    
    
    for (prv in c(0.05,0.25,0.75)) {
      
      cat("prv",prv,"\n")
      
      for (rltn in c(1,2)) {
        
        p<-numeric(0)
        a0<-runif(1)
        a<-matrix(runif(nv),nr=1,nc=nv)
        b<-matrix(runif(nv*nv),nr=nv,nc=nv)
        
        
        if (rltn == 1) {
          
          for (ii in 1:nt) {
            
            xi<-x[ii,]
            
            fi<-a0+a%*%xi
            
            p[ii]<-1/(1+exp(-fi))
            
          }
          
        } else {
          
          for (ii in 1:nt) {
            
            xi<-x[ii,]
            
            fi<-a0+a%*%xi+xi%*%b%*%xi		
            
            p[ii]<-1/(1+exp(-fi))
            
          }
          
        }	# end of "if (rltn == 1)"
        
        
        od1 <- order(p)
        od2 <- p[od1]
        n1 <- round(prv*nt)
        n0 <- nt-n1
        ct<-od2[n0]
        
        cat("	n1=",nt-n0,"  n0=",n0,"\n")
        
        idt1<-idt[p>ct]
        idt0<-idt[!p>ct]
        
        id1tmp <- idt1
        
        
        for (nrecd in c(500,1000)) {
          
          cat("nrecd",nrecd,"\n")
          
          
          for (po in prop_out) {
            
            cat("po",po,"\n")
            
            nab <- round(nrecd*po)	# number of abnormal records
            nnm <- nrecd - nab	# number of normal records
            
            
            for (bias_id in c(0,1)) {
              
              # select presences
              
              
              if (bias_id != 0) {
                
                ids00 <- intersect(id1tmp,idsv)
                len <- length(ids00)
                ids0 <- numeric()
                
                if (len >= 1) {
                  mxsv <- max(sv1[ids00])
                  for (k in ids00) {
                    if (sample(1:mxsv,1) <= sv1[k]) ids0 <- c(ids0,k)
                  }
                  
                } 
                
                len <- length(ids0)
                if (len < nrecd) ids0 <- c(ids0,sample(setdiff(id1tmp,ids0),nrecd-len,replace=FALSE))
                
                ns0 <- length(ids0)	# number of presence points sampled
                
              } else {	# end of "if (bias_id != 0)"
                
                ids0 <- id1tmp
                
              }
              
              id1 <- sample(ids0,nnm,replace=FALSE)
              id2 <- sample(idt0,nab,replace=FALSE) 
              
              dt <- x[c(id1,id2),]
              
              
              
              #----------------------------------------------------------------------------------------------------------
              
              #	mvoutlier
              
              f <- tryCatch(
                
                {
                  
                  dd <- dd.plot(dt)
                  
                  ot <- c(1:dim(dt)[1])[dd$outliers]
                  
                  #print(ot)
                  
                  npo <- length(ot)	# how many potential outliers are identified ?
                  
                  nro <- sum(ot %in% (nnm+1):(nnm+nab))	# among the identified outliers, how many are real outliers ?
                  
                  tss <- nro/nab - (npo-nro)/nnm
                  
                  cat(file=fln_dx,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,rltn,1,nnm,nab,npo,nro,nro/nab,1-(npo-nro)/nnm,tss,(tss+1)/2,append=TRUE,"\n")	# 1: method 1
                  
                },
                
                error = function(cond) {
                  message(cond)
                  return(1)
                  
                }
                
              )
              
              
              
              #----------------------------------------------------------------------------------------------------------
              
              #	rrcovHD
              
              f <- tryCatch(
                
                {
                  
                  obj <- OutlierMahdist(dt)
                  
                  ot <- getOutliers(obj)
                  
                  #print(ot2)
                  
                  npo <- length(ot)	# how many potential outliers are identified ?
                  
                  nro <- sum(ot %in% (nnm+1):(nnm+nab))	# among the identified outliers, how many are real outliers ?
                  
                  tss <- nro/nab - (npo-nro)/nnm
                  
                  cat(file=fln_dx,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,rltn,2,nnm,nab,npo,nro,nro/nab,1-(npo-nro)/nnm,tss,(tss+1)/2,append=TRUE,"\n")	
                  
                },
                
                error = function(cond) {
                  message(cond)
                  return(1)
                  
                }
                
              )
              
              
              f <- tryCatch(
                
                {
                  
                  obj <- OutlierPCOut(dt)
                  
                  ot <- getOutliers(obj)
                  
                  #print(ot)
                  
                  npo <- length(ot)	# how many potential outliers are identified ?
                  
                  nro <- sum(ot %in% (nnm+1):(nnm+nab))	# among the identified outliers, how many are real outliers ?
                  
                  tss <- nro/nab - (npo-nro)/nnm
                  
                  cat(file=fln_dx,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,rltn,3,nnm,nab,npo,nro,nro/nab,1-(npo-nro)/nnm,tss,(tss+1)/2,append=TRUE,"\n")	
                  
                },
                
                error = function(cond) {
                  message(cond)
                  return(1)
                  
                }
                
              )
              
              
              f <- tryCatch(
                
                {
                  
                  obj <- OutlierSign2(dt)
                  
                  ot <- getOutliers(obj)
                  
                  #print(ot)
                  
                  npo <- length(ot)	# how many potential outliers are identified ?
                  
                  nro <- sum(ot %in% (nnm+1):(nnm+nab))	# among the identified outliers, how many are real outliers ?
                  
                  tss <- nro/nab - (npo-nro)/nnm
                  
                  cat(file=fln_dx,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,rltn,4,nnm,nab,npo,nro,nro/nab,1-(npo-nro)/nnm,tss,(tss+1)/2,append=TRUE,"\n")	
                  
                },
                
                error = function(cond) {
                  message(cond)
                  return(1)
                  
                }
                
              )
              
              
              #----------------------------------------------------------------------------------------------------------
              
              #	one-class SVM
              
              npo <- numeric()
              nro <- numeric()
              
              k <- 0
              
              for (nu in 1:9/10) {
                
                k <- k+1
                
                
                f <- tryCatch(
                  
                  {
                    
                    sv <- ksvm(dt,type = "one-svc", kernel = "rbfdot",kpar = list(sigma = 0.1), C = 10,nu=nu,prob.model = T)
                    
                    psv1 <- predict(sv,dt)
                    
                    ot <- c(1:(nnm+nab))[!psv1]
                    
                    #print(ot)
                    
                    npo[k] <- length(ot)	# how many potential outliers are identified ?
                    
                    nro[k] <- sum(ot %in% (nnm+1):(nnm+nab))	# among the identified outliers, how many are real outliers ?
                    
                  },
                  
                  error = function(cond) {
                    message(cond)
                    return(1)
                    
                  }
                  
                )	# end of "tryCatch"
                
                
              }	# end of "for ( nu in )"
              
              tss <- nro/nab - (npo-nro)/nnm
              
              idmx <- order(tss,decreasing=TRUE)[1]
              
              cat(file=fln_dx,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,rltn,5,nnm,nab,npo[idmx],nro[idmx],nro[idmx]/nab,1-(npo[idmx]-nro[idmx])/nnm,tss[idmx],(tss[idmx]+1)/2,append=TRUE,"\n")	
              
              
              #----------------------------------------------------------------------------------------------------------
              
              #	Rlof with parameter k = 15
              
              ot <- lof(dt, k=15)
              
              prd0 <- ot[1:nnm]
              prd1 <- ot[(nnm+1):nrecd]
              
              dx <- dxmx(prd0,prd1)
              tss <- dx[1]
              npo <- dx[2]
              nro <- dx[3]
              
              auc <- roc(prd0,prd1)
              
              
              cat(file=fln_dx,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,rltn,6,nnm,nab,npo,nro,nro/nab,1-(npo-nro)/nnm,tss,auc,append=TRUE,"\n")	
              
              
              #----------------------------------------------------------------------------------------------------------
              
              #	Isolation Forest
              
              tr <- IsolationTrees(as.data.frame(dt),rFactor=1,rowSamp=TRUE,nRowSamp=100,colSamp=FALSE)
              as <- AnomalyScore(as.data.frame(dt),tr)
              ot <- as$outF	# show anomaly score
              
              prd0 <- ot[1:nnm]
              prd1 <- ot[(nnm+1):nrecd]
              
              dx <- dxmx(prd0,prd1)
              tss <- dx[1]
              npo <- dx[2]
              nro <- dx[3]
              
              auc <- roc(prd0,prd1)
              
              
              cat(file=fln_dx,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,rltn,7,nnm,nab,npo,nro,nro/nab,1-(npo-nro)/nnm,tss,auc,append=TRUE,"\n")	
              
              #----------------------------------------------------------------------------------------------------------
              
              #	random forest - outlier
              
              rf <- randomForest(dt, ntree=ntree, proximity=TRUE)
              
              ot <- outlier(rf)
              
              prd0 <- ot[1:nnm]
              prd1 <- ot[(nnm+1):nrecd]
              
              dx <- dxmx(prd0,prd1)
              tss <- dx[1]
              npo <- dx[2]
              nro <- dx[3]
              
              auc <- roc(prd0,prd1)
              
              
              cat(file=fln_dx,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,rltn,8,nnm,nab,npo,nro,nro/nab,1-(npo-nro)/nnm,tss,auc,append=TRUE,"\n")	
              
              
              #----------------------------------------------------------------------------------------------------------
              
              #	random forest - presence-only modelling
              
              
              id1trn <- c(id1,id2)
              n1trn <- nnm+nab
              x1trn <- x[id1trn,]
              rownames(x1trn) <- 1:n1trn
              x1trn <- na.omit(x1trn)
              
              n0trn <- rptm*n1trn 
              id0trn <- sample(1:nt,n0trn,replace=F)	
              x0trn <- x[id0trn,]
              rownames(x0trn)<-(n1trn+1):(n1trn+n0trn)
              x0trn<-na.omit(x0trn)
              prf <- rfst(rbind(x1trn,x0trn),n1trn,n0trn,x1trn,ntree)
              
              prd0 <- 1-prf[1:nnm]
              prd1 <- 1-prf[(nnm+1):nrecd]
              
              dx <- dxmx(prd0,prd1)
              tss <- dx[1]
              npo <- dx[2]
              nro <- dx[3]
              
              auc <- roc(prd0,prd1)
              
              
              cat(file=fln_dx,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,rltn,9,nnm,nab,npo,nro,nro/nab,1-(npo-nro)/nnm,tss,auc,append=TRUE,"\n")	
              
              
              #----------------------------------------------------------------------------------------------------------
              
              #	two-class SVM
              
              f <- tryCatch(
                
                {
                  
                  species<-as.factor(c(rep(1,n1trn),rep(0,n0trn)))
                  sv <- ksvm(species ~ ., data=rbind(x1trn,x0trn),type = "C-bsvc", kernel = "rbfdot",kpar = list(sigma = 0.1), C = 10,prob.model = T)
                  
                  psv2<-predict(sv,rbind(x1trn),type="probabilities")[,2]
                  
                  prd0 <- 1-psv2[1:nnm]
                  prd1 <- 1-psv2[(nnm+1):nrecd]
                  
                  dx <- dxmx(prd0,prd1)
                  tss <- dx[1]
                  npo <- dx[2]
                  nro <- dx[3]
                  
                  auc <- roc(prd0,prd1)
                  
                  
                  cat(file=fln_dx,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,rltn,10,nnm,nab,npo,nro,nro/nab,1-(npo-nro)/nnm,tss,auc,append=TRUE,"\n")	
                  
                  
                },
                
                error = function(cond) {
                  message(cond)
                  return(1)
                  
                }
                
              )
              
              
            }	# end of "for (bias_id in c(1,0))"
            
          }	# end of "for (po in prop_out)"
          
        }	# end of "for (nrecd in c())"
        
      }	# end of "for (nv in c(nv1,nv2))"
      
    }	# end of "for (rltn in c(1,2))"
    
  }	# end of "for (prv in c())"
  
}	# end of "for (sm in 1:nsm)"


#################################################################################
#################################################################################
#################################################################################


Part 2. R code for the second set of analysis producing the proportion of records to be checked if a specific proportion of true outliers are to be identified

#	investigate some outlier detection methods used in cleaning species distribution data

#	including: outlier & presence-only modelling with randomForest;
#	dd.plot & arw with mvoutlier;
#	OutlierMahdist & OutlierPCDist & OutlierPCOut & OutlierSign2 with rrcovHD;
#	ksvm (both one-class and two-classes) with kernlab;
#	Isolation forest with R package IsolationForest;
#	local outlier factor with R package Rlof;

#	source("\\\\ari-vm-00\\SpatialStorage\\SAN_Projects\\liu\\U_copy\\accudx\\outlier_detection-4.r")

nseed = 1033
set.seed(nseed)


################################################################################################################


#	modelling with Random Forest for prediction

#	trn: training data without the column for occurrence with first n1trn rows of presences and last n0trn rows of absences
#	ap:  application without the column for occurrence.

rfst<-function(trn,n1trn,n0trn,ap,ntree) {
  
  #	library(randomForest)	### it was moved to the main program before this function
  
  species<-as.factor(c(rep(1,n1trn),rep(0,n0trn)))
  
  f <- randomForest(species ~ ., data=trn,ntree=ntree)
  
  prd<-predict(f,ap,type="prob")[,2]
  
  prd
  
}

###########################################################################################################################################

#	main program

library(kernlab)
library(rrcovHD)
library(mvoutlier)
library(randomForest)
library(IsolationForest)
library(Rlof)

dir <- "\\\\ari-vm-00\\SpatialStorage\\SAN_Projects\\liu\\U_copy\\accudx\\pres_only\\outlier_detect\\"

fln_out <- paste(dir,"outlier-33.txt",sep="")

x0 <- read.table(file="\\\\ari-vm-
00\\SpatialStorage\\SAN_Projects\\liu\\U_copy\\accudx\\pres_only\\predictors.txt",header=T,as.is=T)
sv1 <- read.table(file="\\\\ari-vm-
00\\SpatialStorage\\SAN_Projects\\liu\\U_copy\\accudx\\pres_only\\survey.txt",header=T,as.is=T)

cat(file=fln_out,"prv","nv","prop_outlier","nrecd","sm","nptm","ntree","bias_id","rltn","method","nnm","nab","npo","nro","\n")

rptm <- 2
ntree <- 2000
nsm <- 300
prop_out <- c(0.01,0.05,0.1) # proportion of outliers

#nv1 <- 18 # nv1 = nv0, all environmental variables (except coordinates) used in the analysis
nv2 <- 5 # only a subset of 5 environmental variables (except coordinates) used in the analysis

x0 <- x0[,-c(1:2)]
nt <- dim(x0)[1]
nv0 <- dim(x0)[2]
idt <- 1:nt

x0<-as.matrix(x0)
x0<-scale(x0)

idsv<-idt[sv1>0] # survey locations

rm(sv0)

nv1 <- nv0

for (sm in 1:nsm) {
  
  cat("sm",sm,"\n")
  
  
  
  for (nv in c(nv1,nv2)) {
    
    cat("nv",nv,"\n")
    
    if (nv == nv1) {
      vid <- 1:nv1
    } else {
      vid <- sample(1:nv0,nv2,replace=FALSE)
    }
    
    x <- x0[,vid]
    
    
    for (prv in c(0.05,0.25,0.75)) {
      
      cat("prv",prv,"\n")
      
      p<-numeric(0)
      a0<-runif(1)
      a<-matrix(runif(nv),nr=1,nc=nv)
      b<-matrix(runif(nv*nv),nr=nv,nc=nv)
      
      for (ii in 1:nt) {
        
        xi<-x[ii,]
        
        #		fi<-a0+a%*%xi+xi%*%b%*%xi		
        fi<-a0+a%*%xi
        
        p[ii]<-1/(1+exp(-fi))
        
      }
      
      od1 <- order(p)
      od2 <- p[od1]
      n1 <- round(prv*nt)
      n0 <- nt-n1
      ct<-od2[n0]
      
      cat("	n1=",nt-n0,"  n0=",n0,"\n")
      
      idt1<-idt[p>ct]
      idt0<-idt[!p>ct]
      
      id1tmp <- idt1
      
      
      for (nrecd in c(100,500,1000)) {
        
        cat("nrecd",nrecd,"\n")
        
        
        for (po in prop_out) {
          
          cat("po",po,"\n")
          
          nab <- round(nrecd*po)	# number of abnormal records
          nnm <- nrecd - nab	# number of normal records
          
          
          for (bias_id in c(1,0)) {
            
            # select presences
            
            
            if (bias_id != 0) {
              
              ids00 <- intersect(id1tmp,idsv)
              len <- length(ids00)
              ids0 <- numeric()
              
              if (len >= 1) {
                mxsv <- max(sv1[ids00])
                for (k in ids00) {
                  if (sample(1:mxsv,1) <= sv1[k]) ids0 <- c(ids0,k)
                }
                
              }
              
              len <- length(ids0)
              if (len < nrecd) ids0 <- c(ids0,sample(setdiff(id1tmp,ids0),nrecd-len,replace=FALSE))
              
              ns0 <- length(ids0)	# number of presence points sampled
              
            } else {	# end of "if (bias_id != 0)"
              
              ids0 <- id1tmp
              
            }
            
            id1 <- sample(ids0,nnm,replace=FALSE)
            id2 <- sample(idt0,nab,replace=FALSE) 
            
            dt <- x[c(id1,id2),]
            
            
            #----------------------------------------------------------------------------------------------------------
            
            #	mvoutlier
            
            f <- tryCatch(
              
              {
                
                dd <- dd.plot(dt)
                
                ot <- c(1:dim(dt)[1])[dd$outliers]
                
                #print(ot)
                
                npo <- length(ot)	# how many potential outliers are identified ?
                
                nro <- sum(ot %in% (nnm+1):(nnm+nab))	# among the identified outliers, how many are real outliers ?
                
                cat(file=fln_out,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,1,nnm,nab,npo,nro,append=TRUE,"\n")	# 1: method 1
                
              },
              
              error = function(cond) {
                message(cond)
                return(1)
                
              }
              
            )
            
            
            #----------------------------------------------------------------------------------------------------------
            
            #	rrcovHD
            
            f <- tryCatch(
              
              {
                
                obj <- OutlierMahdist(dt)
                
                ot <- getOutliers(obj)
                
                #print(ot)
                
                npo <- length(ot)	# how many potential outliers are identified ?
                
                nro <- sum(ot %in% (nnm+1):(nnm+nab))	# among the identified outliers, how many are real outliers ?
                
                cat(file=fln_out,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,2,nnm,nab,npo,nro,append=TRUE,"\n")	
                
              },
              
              error = function(cond) {
                message(cond)
                return(1)
                
              }
              
            )
            
            
            #---------------------------------------------------------------------------------------------------------------------
            
            #	rrcovHD
            
            f <- tryCatch(
              
              {
                
                obj <- OutlierPCOut(dt)
                
                ot <- getOutliers(obj)
                
                #print(ot)
                
                npo <- length(ot)	# how many potential outliers are identified ?
                
                nro <- sum(ot %in% (nnm+1):(nnm+nab))	# among the identified outliers, how many are real outliers ?
                
                cat(file=fln_out,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,3,nnm,nab,npo,nro,append=TRUE,"\n")	
                
              },
              
              error = function(cond) {
                message(cond)
                return(1)
                
              }
              
            )
            
            
            #---------------------------------------------------------------------------------------------------------------------
            
            #	rrcovHD
            
            f <- tryCatch(
              
              {
                
                obj <- OutlierSign2(dt)
                
                ot <- getOutliers(obj)
                
                #print(ot)
                
                npo <- length(ot)	# how many potential outliers are identified ?
                
                nro <- sum(ot %in% (nnm+1):(nnm+nab))	# among the identified outliers, how many are real outliers ?
                
                cat(file=fln_out,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,4,nnm,nab,npo,nro,append=TRUE,"\n")	
                
              },
              
              error = function(cond) {
                message(cond)
                return(1)
                
              }
              
            )
            
            #----------------------------------------------------------------------------------------------------------
            
            #	one-class SVM (SVM_novelty)
            
            k <- 0
            
            for (nu in c(0.8,0.6,0.4,0.2,0.1)) {
              
              k <- k+1
              
              
              f <- tryCatch(
                
                {
                  
                  sv <- ksvm(dt,type = "one-svc", kernel = "rbfdot",kpar = list(sigma = 0.1), C = 10,nu=nu,prob.model = T)
                  
                  psv1 <- predict(sv,dt)
                  
                  ot <- c(1:(nnm+nab))[!psv1]
                  
                  #print(ot)
                  
                  npo <- length(ot)	# how many potential outliers are identified ?
                  
                  nro <- sum(ot %in% (nnm+1):(nnm+nab))	# among the identified outliers, how many are real outliers ?
                  
                  m <- as.numeric(paste("5",k,sep=""))
                  
                  cat(file=fln_out,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,m,nnm,nab,npo,nro,append=TRUE,"\n")	
                  
                },
                
                error = function(cond) {
                  message(cond)
                  return(1)
                  
                }
                
              )	# end of "tryCatch"
              
              
            }	# end of "for ( nu in c())"
            
            
            #----------------------------------------------------------------------------------------------------------
            
            #	Rlof with parameter k = 15
            
            ot <- lof(dt, k=15)
            
            npo <- sum(ot >= min(ot[(nnm+1):(nnm+nab)])) # in order to identify all the outliers, how many
            records need to be checked?
              nro <- nab
            
            cat(file=fln_out,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,rltn,61,nnm,nab,npo,nro,append=TRUE,"\n")
            
            k0 <- 1
            
            for (prop in c(0.8,0.6,0.4,0.2)) {
              
              k0 <- k0+1
              
              nro <- max(1,round(nab*prop))
              npo <- sum(ot >= quantile(ot[(nnm+1):(nnm+nab)],1-prop)) # in order to identify prop of the
              outliers, how many records need to be checked?
                
                m <- as.numeric(paste("6",k0,sep=""))
              
              cat(file=fln_out,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,rltn,m,nnm,nab,npo,nro,append=TRUE,"\n")
              
            }
            
            #----------------------------------------------------------------------------------------------------------
            
            #	Isolation Forest
            
            tr <- IsolationTrees(as.data.frame(dt),rFactor=0)
            as <- AnomalyScore(as.data.frame(dt),tr)
            ot <- as$outF	# show anomaly score
            
            npo <- sum(ot >= min(ot10[(nnm+1):(nnm+nab)]))	# in order to identify all the outliers, how many records need to be checked?
            nro <- nab
            
            cat(file=fln_out,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,71,nnm,nab,npo,nro,append=TRUE,"\n")	
            
            k <- 1
            
            for (prop in c(0.8,0.6,0.4,0.2)) {
              
              k <- k+1
              
              nro <- max(1,round(nab*prop))
              npo <- sum(ot >= quantile(ot[(nnm+1):(nnm+nab)],1-prop))	# in order to identify prop of the outliers, how many records need to be checked?
              
              m <- as.numeric(paste("7",k,sep=""))
              
              cat(file=fln_out,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,m,nnm,nab,npo,nro,append=TRUE,"\n")	
            }
            
            #----------------------------------------------------------------------------------------------------------
            
            #	random forest - outlier
            
            rf <- randomForest(dt, ntree=ntree, proximity=TRUE)
            
            ot <- outlier(rf)
            
            od <- order(ot,decreasing=T)
            #print(od)
            
            npo <- sum(ot >= min(ot[(nnm+1):(nnm+nab)]))	# in order to identify all the outliers, how many records need to be checked?
            nro <- nab
            
            cat(file=fln_out,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,81,nnm,nab,npo,nro,append=TRUE,"\n")	
            
            k <- 1
            
            for (prop in c(0.8,0.6,0.4,0.2)) {
              
              k <- k+1
              
              nro <- max(1,round(nab*prop))
              npo <- sum(ot >= quantile(ot[(nnm+1):(nnm+nab)],1-prop))	# in order to identify prop of the outliers, how many records need to be checked?
              
              m <- as.numeric(paste("8",k,sep=""))
              
              cat(file=fln_out,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,m,nnm,nab,npo,nro,append=TRUE,"\n")	
              
            }
            
            
            #----------------------------------------------------------------------------------------------------------
            
            #	random forest - presence-only modelling
            
            
            id1trn <- c(id1,id2)
            n1trn <- nnm+nab
            x1trn <- x[id1trn,]
            rownames(x1trn) <- 1:n1trn
            x1trn <- na.omit(x1trn)
            
            n0trn <- rptm*n1trn 
            id0trn <- sample(1:nt,n0trn,replace=F)	
            x0trn <- x[id0trn,]
            rownames(x0trn)<-(n1trn+1):(n1trn+n0trn)
            x0trn<-na.omit(x0trn)
            prf <- rfst(rbind(x1trn,x0trn),n1trn,n0trn,rbind(x1trn,x0trn),ntree)
            od <- order(prf[1:(nnm+nab)])
            
            npo <- sum(prf[1:(nnm+nab)] <= max(prf[(nnm+1):(nnm+nab)]))			# in order to identify all the outliers, how many records need to be checked?
            nro <- nab
            
            cat(file=fln_out,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,91,nnm,nab,npo,nro,append=TRUE,"\n")	
            
            k <- 1
            
            for (prop in c(0.8,0.6,0.4,0.2)) {
              
              k <- k+1
              
              nro <- max(1,round(nab*prop))
              npo <- sum(prf[1:(nnm+nab)] <= quantile(prf[(nnm+1):(nnm+nab)],prop))		# in order to identify prop of the outliers, how many records need to be checked?
              
              m <- as.numeric(paste("9",k,sep=""))
              
              cat(file=fln_out,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,m,nnm,nab,npo,nro,append=TRUE,"\n")	
              
            }
            
            
            #----------------------------------------------------------------------------------------------------------
            
            #	two-class SVM
            
            f <- tryCatch(
              
              {
                
                species<-as.factor(c(rep(1,n1trn),rep(0,n0trn)))
                sv <- ksvm(species ~ ., data=rbind(x1trn,x0trn),type = "C-bsvc", kernel = "rbfdot",kpar = list(sigma = 0.1), C = 10,prob.model = T)
                
                psv2<-predict(sv,rbind(x1trn),type="probabilities")[,2]
                
                #print(order(psv2[1:(nnm+nab)]))
                
                od <- order(psv2[1:(nnm+nab)])
                
                npo <- sum(psv2[1:(nnm+nab)] <= max(psv2[(nnm+1):(nnm+nab)]))		# in order to identify all the outliers, how many records need to be checked?
                nro <- nab
                
                cat(file=fln_out,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,101,nnm,nab,npo,nro,append=TRUE,"\n")	
                
                k <- 1
                
                for (prop in c(0.8,0.6,0.4,0.2)) {
                  
                  k <- k+1
                  
                  nro <- max(1,round(nab*prop))
                  npo <- sum(psv2[1:(nnm+nab)] <= quantile(psv2[(nnm+1):(nnm+nab)],prop))	# in order to identify prop of the outliers, how many records need to be checked?
                  
                  m <- as.numeric(paste("10",k,sep=""))
                  
                  cat(file=fln_out,prv,nv,po,nrecd,sm,rptm,ntree,bias_id,sv_id,m,nnm,nab,npo,nro,append=TRUE,"\n")	
                  
                }	# end of "for (prop in c())"
                
              },
              
              error = function(cond) {
                message(cond)
                return(1)
                
              }
              
            )
            
            #----------------------------------------------------------------------------------------------------------
            
            
          }	# end of "for (bias_id in c(1,0))"
          
        }	# end of "for (po in prop_out)"
        
      }	# end of "for (nrecd in c())"
      
    }	# end of "for (rltn in c())"
    
  }	# end of " for (prv in c())"
  
}	# end of "for (nv in c(nv1,nv2))"

}	# end of "for (sm in 1:nsm)"