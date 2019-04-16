n185 <- function(FlowScale,HeadScale, HeadUnits) {
  
  
  n <- FlowScale
  
  MaxP <- HeadScale*100
  
  
  if (HeadUnits == 1)
    HeadText <- "Head (FT)"
  else if (HeadUnits == 2)
    HeadText <- "Head (PSI)"
  else
    HeadText <- "Head (_____)"
  
  
  if (HeadUnits!=0) {
    
    if (HeadUnits == 1)
      HeadScaleUnits = " FT"
    else if (HeadUnits == 2)
      HeadScaleUnits = " PSI"
    else
      HeadScaleUnits = ""
    
    
    if (HeadScale == 1)
      HeadScaleText <- paste0("Max Head 100",HeadScaleUnits)
    else if (HeadScale == 2)
      HeadScaleText <- paste0("Max Head 200",HeadScaleUnits)
    else
      HeadScaleText <- "Max Head __________"
    
    if (FlowScale == 1)
      FlowScaleText <- "Scale A"
    else if (FlowScale == 2)
      FlowScaleText <- "Scale B"
    else if (FlowScale == 4)
      FlowScaleText <- "Scale C"
    else
      FlowScaleText <- "Scale _____"
  }
  else {
    HeadScaleText <- "Max Head __________"
    FlowScaleText <- "Scale _____"
    
  }
  
  
  if (HeadUnits!=0)
    AxisColor = "grey75"
  else
    AxisColor = "black"
  
  x1Col <- AxisColor
  x2Col <- AxisColor
  x3Col <- AxisColor
  y1Col <- AxisColor
  y2Col <- AxisColor
  
  if (FlowScale == 1)
    x1Col = "black"
  else if (FlowScale == 2)
    x2Col = "black"
  else if (FlowScale == 4)
    x3Col = "black"
  
  if (HeadScale == 1)
    y1Col = "black"
  else if (HeadScale == 2)
    y2Col = "black"
  
  
  
  
  
  Q <- seq(n*100,n*1000,n*100)
  plot(0,0, xaxt="n", yaxt="n",type="l", xaxs="i", yaxs="i", xlab="", ylab="", ylim=c(0,MaxP), xlim=c(0,(1000*n)^1.85))
  
  abline(h=seq(0,100*HeadScale,10*HeadScale), v=seq(n*100,n*1000,n*50)^1.85, col="gray75")
  
  axis(1,at=Q^1.85,labels=prettyNum(1/n*Q, big.mark=","), line=0, col.axis=x1Col, cex.axis=0.75)
  axis(1,at=Q^1.85,labels=prettyNum(2/n*Q, big.mark=","), line=1, col.axis=x2Col, cex.axis=0.75, tick=F)
  axis(1,at=Q^1.85,labels=prettyNum(4/n*Q, big.mark=","), line=2, col.axis=x3Col, cex.axis=0.75, tick=F)
  axis(1,at=seq(70*n,1000*n,10*n)^1.85,labels=NA, tck=0.02)
  axis(1,at=seq(150*n,990*n,5*n)^1.85,labels=NA, tck=0.01)
  axis(3,at=seq(70*n,1000*n,10*n)^1.85,labels=NA, tck=0.02)
  axis(3,at=seq(150*n,990*n,5*n)^1.85,labels=NA, tck=0.01)
  #axis(2, at=seq(0,100,10), labels=seq(0,50,5), cex.axis=0.75, line=0)
  axis(2, at=seq(0,100*HeadScale,10*HeadScale), labels=seq(0,100,10), col.axis=y1Col, cex.axis=0.75, line=0, tick=F)
  axis(2, at=seq(10*HeadScale,100*HeadScale,10*HeadScale), labels=seq(20,200,20), col.axis=y2Col, cex.axis=0.75, line=1, tick=F)
  axis(2,at=seq(0,100*HeadScale,5*HeadScale), labels=NA, tck=-0.02)
  axis(2,at=seq(0,100*HeadScale,1*HeadScale), labels=NA, tck=-0.01)
  axis(4,at=seq(0,100*HeadScale,5*HeadScale), labels=NA, tck=0.02)
  axis(4,at=seq(0,100*HeadScale,1*HeadScale), labels=NA, tck=0.01)
  #abline(h=20,lwd=2)
  box(lwd=2)
  mtext("Scale A", side=1, line=1, cex=0.75, at=-(n*200)^1.85, family="serif", col=x1Col)
  mtext("Scale B", side=1, line=2, cex=0.75, at=-(n*200)^1.85, family="serif", col=x2Col)
  mtext("Scale C", side=1, line=3, cex=0.75, at=-(n*200)^1.85, family="serif", col=x3Col)
  mtext("Flow (GPM)", side=1, line=4, cex=0.9, family="serif")
  mtext(HeadText, side=2, line=3, cex=0.9, family="serif")
  mtext(paste0(FlowScaleText,"          ", HeadScaleText), side=4, cex=0.9, family="serif", line=1)
  #text((n*946)^1.85, MaxP*0.95, "SCALE _____", pos=1)
}
ff <- function(Ps, Qt, Pt, color="black", LineType = 1) {
  Qi <- seq(0,10000,1000)
  Pi <- Ps - (Ps-Pt)*(Qi/Qt)^1.85
  
  lines(Qi^1.85,Pi, col=color, lwd=2, lty=LineType)
  
  k_psi <- (Ps-Pt)/(Qt^1.85)
  
  return(k_psi)
}
ffk <- function(Ps, Qt, Pt) {
  Qi <- seq(0,10000,1000)
  Pi <- Ps - (Ps-Pt)*(Qi/Qt)^1.85
  
  #lines(Qi^1.85,Pi, col=color, lwd=2, lty=LineType)
  
  k_psi <- (Ps-Pt)/(Qt^1.85)
  
  return(k_psi)
}
shift <- function(Ps,k, color="black", LineType=1) {
  ff(Ps,1000,Ps - k*1000^1.85, color, LineType)
}
tilt <- function(static,k,kf,color="black", LineType=1) {
  Qt = 2000
  
  ff(static,Qt,static-Qt^1.85*(kf+k), color,LineType)
}
fp <- function(Qi, Ps, Qt, Pt) {
  Pi <- Ps - (Ps-Pt)*(Qi/Qt)^1.85
  return(Pi)
  
}
pt <- function(Qt, Pt, color){
  points(Qt^1.85,Pt, pch=21, bg="white", col=color, lwd=2, cex=2)
}
kl <- function(k, color="black", LineType=2, static=0){
  Qi <- seq(0,10000,1000)
  Pi <- k*Qi^1.85+static
  lines(Qi^1.85,Pi, col=color, lwd=2, lty=LineType)
}
kp <- function(q,k) {
  h <- k*q^1.85
  return(h)
}
Load_Fire_Flow_Matrix <- function(data) {
  FID <- data[,"FID"]
  Ps <- data[,"Static"]
  Pt <- data[,"Residual"]
  Qt <- as.numeric(gsub(",","",data[,"Flow_Test"]))
  FireFlowData <- cbind(FID,Ps,Qt, Pt)
  
  return(FireFlowData)
}

# Standard JEA Colors
JEA.Dark <- rgb(t(matrix(c(20, 43, 108)/255)))
JEA.Blue <- rgb(t(matrix(c(0, 106, 151)/255)))
JEA.Green <- rgb(t(matrix(c(65, 173, 73)/255)))
JEA.Orange <- rgb(t(matrix(c(244, 199, 33)/255)))
JEA.Grey <- rgb(t(matrix(c(109, 110, 113)/255)))
JEA.Grey <- rgb(109/255, 110/255, 113/255, 0.5)

Calc.C <- function(k.target, k.source, d.pipe, L.pipe) {
  C.pipe <- 1/((2.31*(k.target-k.source)*d.pipe^4.87)/(10.44*L.pipe))^(1/1.85)
  return(C.pipe)
}

Calc.Q.at20 <- function(S,k) {
  Q <- ((S-20)/k)^(1/1.85)
  return(Q)
}

# 
# Draw.Legend <- function(LineText, LineColor, LineType) {
#   legend("bottomleft"
#          , LineText#, c("A: ROOSEVELT BV 1013' N OF 120TH ST (87049)", "A corrected to B location (C=90)", "B: GOLDEN WINGS RD 283' W OF NORMAN ST (242321)", "A corrected to furthest node (C=135)")
#          , lwd=rep(2,length(LineType))#c(2,2,2,2)
#          , lty=LineType#c(1,2,1,2)
#          , col=LineColor#c(JEA.Green, JEA.Green, JEA.Blue, "red")
#          , inset=c(0.005,0.05)
#          , seg.len = 4
#          , cex=0.6
#          , y.intersp=0.5
#          , bty = "n"
#          , box.col=rgb(1,1,1,0.75)
#          , bg=rgb(1,1,1,0.75)
#          , horiz=FALSE#TRUE
#          , text.font=2
#   )
# }

n100 <- function(FlowScale,HeadScale, HeadUnits) {
  
  
  n <- FlowScale
  
  MaxP <- HeadScale*100
  
  
  if (HeadUnits == 1)
    HeadText <- "Head (FT)"
  else if (HeadUnits == 2)
    HeadText <- "Head (PSI)"
  else
    HeadText <- "Head (_____)"
  
  
  if (HeadUnits!=0) {
    
    if (HeadUnits == 1)
      HeadScaleUnits = " FT"
    else if (HeadUnits == 2)
      HeadScaleUnits = " PSI"
    else
      HeadScaleUnits = ""
    
    
    if (HeadScale == 1)
      HeadScaleText <- paste0("Max Head 100",HeadScaleUnits)
    else if (HeadScale == 2)
      HeadScaleText <- paste0("Max Head 200",HeadScaleUnits)
    else
      HeadScaleText <- "Max Head __________"
    
    if (FlowScale == 1)
      FlowScaleText <- "Scale A"
    else if (FlowScale == 2)
      FlowScaleText <- "Scale B"
    else if (FlowScale == 4)
      FlowScaleText <- "Scale C"
    else
      FlowScaleText <- "Scale _____"
  }
  else {
    HeadScaleText <- "Max Head __________"
    FlowScaleText <- "Scale _____"
    
  }
  
  
  if (HeadUnits!=0)
    AxisColor = "grey75"
  else
    AxisColor = "black"
  
  x1Col <- AxisColor
  x2Col <- AxisColor
  x3Col <- AxisColor
  y1Col <- AxisColor
  y2Col <- AxisColor
  
  if (FlowScale == 1)
    x1Col = "black"
  else if (FlowScale == 2)
    x2Col = "black"
  else if (FlowScale == 4)
    x3Col = "black"
  
  if (HeadScale == 1)
    y1Col = "black"
  else if (HeadScale == 2)
    y2Col = "black"
  
  
  
  
  
  Q <- seq(n*100,n*1000,n*100)
  plot(0,0, xaxt="n", yaxt="n",type="l", xaxs="i", yaxs="i", xlab="", ylab="", ylim=c(0,MaxP), xlim=c(0,(1000*n)^1.00))
  
  abline(h=seq(0,100*HeadScale,10*HeadScale), v=seq(n*100,n*1000,n*50)^1.00, col="gray50")
  
  axis(1,at=Q^1.00,labels=prettyNum(1/n*Q, big.mark=","), line=0, col.axis=x1Col, cex.axis=0.75)
  axis(1,at=Q^1.00,labels=prettyNum(2/n*Q, big.mark=","), line=1, col.axis=x2Col, cex.axis=0.75, tick=F)
  axis(1,at=Q^1.00,labels=prettyNum(4/n*Q, big.mark=","), line=2, col.axis=x3Col, cex.axis=0.75, tick=F)
  axis(1,at=seq(70*n,1000*n,10*n)^1.00,labels=NA, tck=0.02)
  axis(1,at=seq(150*n,990*n,5*n)^1.00,labels=NA, tck=0.01)
  axis(3,at=seq(70*n,1000*n,10*n)^1.00,labels=NA, tck=0.02)
  axis(3,at=seq(150*n,990*n,5*n)^1.00,labels=NA, tck=0.01)
  #axis(2, at=seq(0,100,10), labels=seq(0,50,5), cex.axis=0.75, line=0)
  axis(2, at=seq(0,100*HeadScale,10*HeadScale), labels=seq(0,100,10), col.axis=y1Col, cex.axis=0.75, line=0, tick=F)
  axis(2, at=seq(10*HeadScale,100*HeadScale,10*HeadScale), labels=seq(20,200,20), col.axis=y2Col, cex.axis=0.75, line=1, tick=F)
  axis(2,at=seq(0,100*HeadScale,5*HeadScale), labels=NA, tck=-0.02)
  axis(2,at=seq(0,100*HeadScale,1*HeadScale), labels=NA, tck=-0.01)
  axis(4,at=seq(0,100*HeadScale,5*HeadScale), labels=NA, tck=0.02)
  axis(4,at=seq(0,100*HeadScale,1*HeadScale), labels=NA, tck=0.01)
  #abline(h=20,lwd=2)
  box(lwd=2)
  mtext("Scale A", side=1, line=1, cex=0.75, at=-(n*20)^1.00, family="serif", col=x1Col)
  mtext("Scale B", side=1, line=2, cex=0.75, at=-(n*20)^1.00, family="serif", col=x2Col)
  mtext("Scale C", side=1, line=3, cex=0.75, at=-(n*20)^1.00, family="serif", col=x3Col)
  mtext("Flow (GPM)", side=1, line=4, cex=0.9, family="serif")
  mtext(HeadText, side=2, line=3, cex=0.9, family="serif")
  mtext(paste0(FlowScaleText,"          ", HeadScaleText), side=4, cex=0.9, family="serif", line=1)
  #text((n*946)^1.85, MaxP*0.95, "SCALE _____", pos=1)
}