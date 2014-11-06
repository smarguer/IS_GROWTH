format_IS=function(toDo,n1=20,n2=10.5,check=T)
{
features=loadWorkbook("features_list_201014.xlsx")
feature1=features[1]
rownames(feature1)=feature1[,1]
rm(features)
features=feature1[,2:ncol(feature1)]

## requires split workbook with first column containing batch numbers and intermediate
## headers removed.
print("loading files") 

if(check==T)
{
 D=read.delim(paste(toDo,"_D_201014.txt",sep=''),check.names=F,stringsAsFactors=F)
 gc()
 return(colnames(D)[grep("Intensity_Spot\\(",colnames(D))])
}

if(toDo %in% c("NH"))
{
 D=read.delim(paste(toDo,"_D_201014.txt",sep=''),check.names=F,stringsAsFactors=F)
 gc()
 M=read.delim(paste(toDo,"_M_201014.txt",sep=''),check.names=F,stringsAsFactors=F)
 gc()
 G1=read.delim(paste(toDo,"_G1_201014.txt",sep=''),check.names=F,stringsAsFactors=F)
 gc()
 G2=read.delim(paste(toDo,"_G2_201014.txt",sep=''),check.names=F,stringsAsFactors=F)
 gc()
 S=read.delim(paste(toDo,"_S_201014.txt",sep=''),check.names=F,stringsAsFactors=F)
 gc()
}
else if(toDo %in% c("TRP","PHE"))
{
 D=read.delim(paste(toDo,"_D_201014.txt",sep=''),check.names=F,stringsAsFactors=F)
 gc()
 M=read.delim(paste(toDo,"_M_201014.txt",sep=''),check.names=F,stringsAsFactors=F)
 gc()
 G1=read.delim(paste(toDo,"_G1_201014.txt",sep=''),check.names=F,stringsAsFactors=F)
 gc()
 SG2=read.delim(paste(toDo,"_SG2_201014.txt",sep=''),check.names=F,stringsAsFactors=F)
 gc()
}
else if (toDo %in% c("PRO"))
{
 D=read.delim(paste(toDo,"_D_201014.txt",sep=''),check.names=F,stringsAsFactors=F)
 gc()
 ND=read.delim(paste(toDo,"_ND_201014.txt",sep=''),check.names=F,stringsAsFactors=F)
 gc()
 SG2=read.delim(paste(toDo,"_SG2_201014.txt",sep=''),check.names=F,stringsAsFactors=F)
 gc()
}
else
{
 D=read.delim(paste(toDo,"_D_201014.txt",sep=''),check.names=F,stringsAsFactors=F)
 gc()
 ND=read.delim(paste(toDo,"_ND_201014.txt",sep=''),check.names=F,stringsAsFactors=F)
 gc()
}

print("formating")
rm(feature1)
if(toDo %in% c("CYS","ISO"))
{
 m=4
}
else
{
 m=5
}
features[1,2]=paste("Length_Erode(M0",m,", 3)",sep="")
features[2,2]=paste("Width_Erode(M0",m,", 3)",sep="")
features[3,2]=paste("Area_Erode(M0",m,", 3)",sep="")
features[4,2]=paste("Intensity_Spot(M01, Ch01, Bright, ",n2,", 3)_Ch01",sep="")
features[6,2]=paste("Intensity_Spot(M02, Ch02, Bright, ",n1,", 3) And Not Spot(M01, Ch01, Bright, ",n2,", 3)_Ch02",sep="")
features[7,2]=paste("Spot Count_Spot(M02, Ch02, Bright, ",n1,", 3) And Not Spot(M01, Ch01, Bright, ",n2,", 3)",sep="")
features[8,2]=paste("Area_Spot(M02, Ch02, Bright, ",n1,", 3) And Not Spot(M01, Ch01, Bright, ",n2,", 3)",sep="")
features[9,2]=paste("Spot Distance Min_Spot(M02, Ch02, Bright, ",n1,", 3) And Not Spot(M01, Ch01, Bright, ",n2,", 3)",sep="")

toUse=vector()

for(i in 1:9)
{
 toUse[i]=which(colnames(D) == features[i,2])
}
 if(toDo=="PHE")
 {
  toUse[10]=which(colnames(D) == "Length_Fill(daughter 1)")
  toUse[11]=which(colnames(D) == "Width_Fill(daughter 1)")
  toUse[12]=which(colnames(D) == "Area_Fill(daughter 1)")
  toUse[13]=which(colnames(D) == "Length_Fill(daughter 2)")
  toUse[14]=which(colnames(D) == "Width_Fill(daughter 2)")
  toUse[15]=which(colnames(D) == "Area_Fill(daughter 2)")
 }
 else
 {
  toUse[10]=which(colnames(D) == "Length_daughter 1")
  toUse[11]=which(colnames(D) == "Width_daughter 1")
  toUse[12]=which(colnames(D) == "Area_daughter 1")
  toUse[13]=which(colnames(D) == "Length_daughter 2")
  toUse[14]=which(colnames(D) == "Width_daughter 2")
  toUse[15]=which(colnames(D) == "Area_daughter 2")
 }
 toUse[16]=1
 toUse[17]=2

print(toUse)

if(toDo %in% c("NH"))
{ 
 D=D[,toUse]
 D[,-16]=round(D[,-16],3)
 D[,18]=rep("D",nrow(D))
 colnames(D)=c(row.names(features),"Length_D1","Width_D1","Area_D1","Length_D2","Width_D2","Area_D2","batch","cell","phase")
 
 M=M[,toUse]
 M[-16]=round(M[-16],3)
 M[,18]=rep("M",nrow(M))
 colnames(M)=c(row.names(features),"Length_D1","Width_D1","Area_D1","Length_D2","Width_D2","Area_D2","batch","cell","phase")
 
 G1=G1[,toUse]
 G1[-16]=round(G1[-16],3)
 G1[,18]=rep("G1",nrow(G1))
 colnames(G1)=c(row.names(features),"Length_D1","Width_D1","Area_D1","Length_D2","Width_D2","Area_D2","batch","cell","phase")
 
 S=S[,toUse]
 S[-16]=round(S[-16],3)
 S[,18]=rep("S",nrow(S))
 colnames(S)=c(row.names(features),"Length_D1","Width_D1","Area_D1","Length_D2","Width_D2","Area_D2","batch","cell","phase")
 
 G2=G2[,toUse]
 G2[-16]=round(G2[-16],3)
 G2[,18]=rep("G2",nrow(G2))
 colnames(G2)=c(row.names(features),"Length_D1","Width_D1","Area_D1","Length_D2","Width_D2","Area_D2","batch","cell","phase")
 
 assign(paste(toDo,"_data",sep=""),value=rbind(D,M,G1,S,G2))
}
else if (toDo %in% c("PRO"))
{
 D=D[,toUse]
 D[,-16]=round(D[,-16],3)
 D[,18]=rep("D",nrow(D))
 colnames(D)=c(row.names(features),"Length_D1","Width_D1","Area_D1","Length_D2","Width_D2","Area_D2","batch","cell","phase")
 
 ND=ND[,toUse]
 ND[-16]=round(ND[-16],3)
 ND[,18]=rep("ND",nrow(ND))
 colnames(ND)=c(row.names(features),"Length_D1","Width_D1","Area_D1","Length_D2","Width_D2","Area_D2","batch","cell","phase")
 
 SG2=SG2[,toUse]
 SG2[-16]=round(SG2[-16],3)
 SG2[,18]=rep("SG2",nrow(SG2))
 colnames(SG2)=c(row.names(features),"Length_D1","Width_D1","Area_D1","Length_D2","Width_D2","Area_D2","batch","cell","phase")
 
 assign(paste(toDo,"_data",sep=""),value=rbind(D,ND,SG2))
}
else if(toDo %in% c("TRP","PHE"))
{ 
 
 D=D[,toUse]
 D[,-16]=round(D[,-16],3)
 D[,18]=rep("D",nrow(D))
 colnames(D)=c(row.names(features),"Length_D1","Width_D1","Area_D1","Length_D2","Width_D2","Area_D2","batch","cell","phase")
 print("done D")

 M=M[,toUse]
 M[-16]=round(M[-16],3)
 M[,18]=rep("M",nrow(M))
 colnames(M)=c(row.names(features),"Length_D1","Width_D1","Area_D1","Length_D2","Width_D2","Area_D2","batch","cell","phase")
 print("done M")
 
 G1=G1[,toUse]
 G1[-16]=round(G1[-16],3)
 G1[,18]=rep("G1",nrow(G1))
 colnames(G1)=c(row.names(features),"Length_D1","Width_D1","Area_D1","Length_D2","Width_D2","Area_D2","batch","cell","phase")
 print("done G1")

 SG2=SG2[,toUse]
 SG2[-16]=round(SG2[-16],3)
 SG2[,18]=rep("SG2",nrow(SG2))
 colnames(SG2)=c(row.names(features),"Length_D1","Width_D1","Area_D1","Length_D2","Width_D2","Area_D2","batch","cell","phase")
 print("done SG2")
 
 assign(paste(toDo,"_data",sep=""),value=rbind(D,M,G1,SG2))
}
else
{
 D=D[,toUse]
 D[,-16]=round(D[,-16],3)
 D[,18]=rep("D",nrow(D))
 colnames(D)=c(row.names(features),"Length_D1","Width_D1","Area_D1","Length_D2","Width_D2","Area_D2","batch","cell","phase")
 
 ND=ND[,toUse]
 ND[-16]=round(ND[-16],3)
 ND[,18]=rep("ND",nrow(ND))
 colnames(ND)=c(row.names(features),"Length_D1","Width_D1","Area_D1","Length_D2","Width_D2","Area_D2","batch","cell","phase")

 assign(paste(toDo,"_data",sep=""),value=rbind(D,ND))
}

 save(list=paste(toDo,"_data",sep=""),file=paste(toDo,"_201014.rda",sep=""))
 print(paste("saved file ",paste(toDo,"_201014.rda",sep="")," to working directory",sep=""))
 gc()
}

library(RColorBrewer)
library(MASS)

IS_plot=function(dat,x=1,my.pch=1,my.cex=2,batch=NULL,type=c("density","dotplot"))
{
 par(mfrow=c(3,5),mar=c(5,5,5,5))
## 
 k <- 11
 my.cols <- rev(brewer.pal(k, "RdYlBu"))

 phase=unique(dat[,"phase"])
 print(phase)
 print(length(phase))
 if(length(phase)==5)
 {
  print("in 5")
  phase=c("G2","M","G1","S","D")
 }
 else if (length(phase)==4)
 {
  print("in 4")
  phase=c("SG2","M","G1","D")
 }
 else if (length(phase)==3)
 {
  print("in 3")
  phase=c("ND","SG2","D")
 }
 else if (length(phase)==2)
 {
  print("in 2")
  phase=c("ND","D")
 }
 my.col=c(rgb(0,0,0,alpha=0.05),rgb(1,0,0,alpha=0.05),rgb(0,1,0,alpha=0.05),rgb(0,0,1,alpha=0.05),rgb(1,0.65,0,alpha=0.05))
 my.col1=c(rgb(0,0,0),rgb(1,0,0),rgb(0,1,0),rgb(0,0,1),rgb(1,0.65,0))
 print(phase)
 print(my.col[1:length(phase)]) 
##
 if(is.null(batch))
 {
  batch=c("D1","D2","D3","D4","D5","D6")
 }
 for(i in 1:9)
 {
  toplot=which((dat[,"phase"]==phase[1]) & (dat[,"batch"] %in% batch))
  if(type=="dotplot")
  {
   ###adjusts ylim values###
   if(i %in% c(2,11,14))
   {
    max.ylim=10
   }
   else if (i %in% c(3,12))
   {
    max.ylim=max(dat[,3],na.rm=F)
   }
   else
   {
    max.ylim=max(dat[which(dat[,"phase"]=="D"),i],na.rm=T)
   }
   #########################
   max.xlim=max(dat[which(dat[,"phase"]=="D"),x],na.rm=T)
   
   plot(dat[toplot,x],dat[toplot,i],main=colnames(dat)[i],xlab=colnames(dat)[x],ylab=colnames(dat)[i],pch=my.pch,cex=my.cex,col=my.col[1],xlim=c(0,max.xlim),ylim=c(0,max.ylim))
  }
  else if (type=="density")
  {
   max.ylim=vector()
   max.ylim[1]=max(density(dat[which(dat[,"phase"]=="D"),i])$y)
   if("M" %in% dat[,"phase"])
   {
    max.ylim[2]=max(density(dat[which(dat[,"phase"]=="M"),i])$y)
   }
   else
   {
    max.ylim[2]=max(density(dat[which(dat[,"phase"]=="ND"),i])$y)
   }
   max.ylim=max.ylim[order(max.ylim)]
   max.xlim=max(dat[,x])
   
   plot(density(dat[toplot,i],na.rm=T),main=colnames(dat)[i],col=my.col1[1],xlim=c(0,max.xlim),ylim=c(0,max.ylim[2]))
  }
  for(k in 2:length(phase))
  { 
   toplot=which((dat[,"phase"]==phase[k]) & (dat[,"batch"] %in% batch))
   
   if(type=="dotplot")
   {
    points(dat[toplot,x],dat[toplot,i],pch=my.pch,cex=my.cex,col=my.col[k])
   }
   else if (type=="density")
   {
    lines(density(dat[toplot,i],na.rm=T),col=my.col1[k])
   }
  }
  if(i==1) 
  {
   legend(x="topleft",legend=phase,text.col=my.col1[1:length(phase)],bty="n",cex=2)
  }
  if(type=="dotplot")
  {
 #  z <- kde2d(dat[,x],dat[,i], n=100)
 #  contour(z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE, lwd=2)
  }
 }
 for(i in 10:12)
 {
  toplot=which((dat[,"phase"]=="D") & (dat[,"batch"] %in% batch))
  j=i+3
  if(type=="dotplot")
  {
   ###adjusts ylim values###
   if(i %in% c(2,11,14))
   {
    max.ylim=10
   }
   else if (i %in% c(3,12))
   {
    max.ylim=50
   }
  else
   {
    max.ylim=max(dat[which(dat[,"phase"]=="D"),i],na.rm=T)
   }
   #########################
   max.xlim=max(dat[,x],na.rm=T)
 
   plot(dat[toplot,x],dat[toplot,i],main=colnames(dat)[i],xlab=colnames(dat)[x],ylab=colnames(dat)[i],pch=my.pch,cex=my.cex,col=my.col[1],ylim=c(0,max.ylim))
   points(dat[toplot,x],dat[toplot,j],pch=my.pch,cex=my.cex,col=my.col[2])
  }
  if(type=="density")
  {
   plot(density(dat[toplot,i],na.rm=T),main=colnames(dat)[i],col=my.col1[1])
   lines(density(dat[toplot,j],na.rm=T),col=my.col1[2])
  }
  if(i==10)
  {
   legend(x="topleft",legend=c("D1","D2"),text.col=my.col1[1:2],bty="n",cex=2)
  }
 }
 for(i in 10:12)
 {
  toplot=which((dat[,"phase"]=="D") & (dat[,"batch"] %in% batch))
  j=i+3
  if(type=="dotplot")
  {
   max.xlim=max(c(dat[,i],dat[,j]),na.rm=T)
   max.ylim=max(c(dat[,i],dat[,j]),na.rm=T)
 
   plot(dat[toplot,i],dat[toplot,j],main=colnames(dat)[i],xlab=colnames(dat)[i],ylab=colnames(dat)[j],pch=my.pch,cex=my.cex,col=my.col[1],xlim=c(0,max.xlim),ylim=c(0,max.ylim))
   abline(0,1)
   abline(2,1,lty=2)
   abline(-2,1,lty=2)
   co=signif(cor(dat[toplot,i],dat[toplot,j]),2)
   legend(x="topleft",legend=bquote(paste(R[Pearson],"=",.(co),sep='')),bty="n",cex=1.3)
  }


 } 
}














