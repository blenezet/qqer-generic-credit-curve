library(ggplot2)
library(dplyr)
library(reshape2)
library(XML)
library(xtable)

source("Q:\\Projects\\GenericCreditCurve\\RiskMag\\cds_functions.R")

vasicekCDF = function(x,p,rho) pnorm((sqrt(1-rho)*qnorm(x)-qnorm(p))/sqrt(rho))
vasicekPDF = function(x,p,rho) sqrt((1-rho)/rho)*exp(-(1/(2*rho))*(sqrt(1-rho)*qnorm(x)-qnorm(p))^2+(1/2)*qnorm(x)^2)

# ----------------------------------------------
# Figure 2.1
# ----------------------------------------------

gccstruct = data.frame(X1=rep(NA,11),
                       X2=rep(NA,11),
                       X3=rep(NA,11),
                       X4=rep(NA,11))#,
                       #X5=rep(NA,29))
X1 = c("Senior Unsecured","Subordinated")
X2 = c("Basic Materials","Consumer Goods","Consumer Services","Enegery","Financial","Government","Healthcare","Industrials","Technology","Telecom","Utilities")
X3 = c("AAA","AA","A","BBB","BB","B","CCC")
X4 = c("1Y","3Y","5Y","10Y")
#X5 = c("AED","AUD","CAD","CHF","CNH","CNY","CZK","EUR","GBP","HKD","HUF","IDR","ILS","INR","JPY", "KRW","MXN","NOK","NZD","PEN","PLN","RON","RUB","SEK","SGD","TRY","TWD","USD","ZAR")

gccstruct$X1 = c(X1,rep(NA,nrow(gccstruct)-length(X1)))
gccstruct$X2 = c(X2,rep(NA,nrow(gccstruct)-length(X2)))
gccstruct$X3 = c(X3,rep(NA,nrow(gccstruct)-length(X3)))
gccstruct$X4 = c(X4,rep(NA,nrow(gccstruct)-length(X4)))
#gccstruct$X5 = c(X5,rep(NA,nrow(gccstruct)-length(X5)))

colnames(gccstruct) = c("Debt Tier","Sector","Rating","Tenor")#,"Currency")

latex.gccstruct = xtable(gccstruct)
print(latex.gccstruct,floating=FALSE,NA.string="",include.rownames=FALSE,scalebox=0.7)



# ----------------------------------------------
# Figure 2.2
# ----------------------------------------------


p = 0.04
rho = 0.1
X = seq(0,0.3,by=0.0001)
u = max(X)/7

CDF = sapply(X,vasicekCDF,p=p,rho=rho)
PDF = sapply(X,vasicekPDF,p=p,rho=rho)
PDF = PDF/max(PDF,na.rm=TRUE) #scale picture
CDF = CDF/max(CDF,na.rm=TRUE) #scale picture

data = data.frame(X=X,PDF=PDF,CDF=CDF)
data.melt = melt(data,id.vars=c("X"))

g = ggplot(data.melt)
g = g + geom_line(aes(x = X, y=value, group = variable, color = variable, linetype= variable))
g = g + geom_vline(xintercept = (0:7)*(u), colour="grey", linetype = "longdash")
g = g + geom_hline(yintercept = 0)
g = g + scale_color_manual(values = c("black", "black"))
g = g +  scale_linetype_manual(values=c("solid", "dashed"))
g = g + theme_bw()
g = g +  theme(
  axis.line=element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.x=element_text(angle = 90, hjust = 0, size=14),
  axis.text.y = element_blank(),
  axis.title.x=element_blank(), 
  axis.title.y=element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  legend.position="none") 
g = g + scale_y_continuous(expand = c(0, 0)) # supress margin on y axis
g = g + scale_x_continuous(expand = c(0, 0),breaks=0.5*u + (0:6)*u, labels=c("u","2u","3u","4u","5u","6u","7u"))
g = g #+ coord_flip() #+ scale_x_reverse()
#g = g + geom_vline(xintercept = S0 )
#g = g + ggtitle("Probability Density Function")
#g = g + xlab("Asset value")
#g = g + ylab("Probability mass")
plot(g)
ggsave(file="Q:\\Projects\\GenericCreditCurve\\RiskMag\\pdf-cdf.svg", plot=g, width=7, height=3)

# ----------------------------------------------
# Figure 3.1
# ----------------------------------------------

markit.doc <- xmlParse("Q:\\Projects\\GenericCreditCurve\\QQER\\cds_premium_20150323.xml")
markit.data <- xmlToDataFrame(nodes=getNodeSet(markit.doc,"//data/row"))
markit.data$AvRating = factor(markit.data$AvRating,levels = c("AAA","AA","A","BBB","BB","B","CCC"),ordered = TRUE)
markit.data$Sector = factor(markit.data$Sector, levels=c("Basic Materials","Consumer Goods","Consumer Services","Energy","Financials","Government", "Healthcare","Industrials","Technology","Telecommunications Services","Utilities"),labels=c("BAS","GOOD","SVCS","ENRG","FIN","GOVT", "HEAL","INDU","TECH","TELC","UTIL"),ordered=TRUE)

               

quotesbyccyrating = addmargins(table(markit.data$Ccy,markit.data$AvRating), FUN = list(Total = sum), quiet = TRUE)
quotesbyccyrating[quotesbyccyrating == 0] = NA
latex.quotesbyccyrating = xtable(quotesbyccyrating)
print(latex.quotesbyccyrating,floating=FALSE,NA.string="\\cellcolor{gray!20}",scalebox=0.58)

quotesbyccysector = addmargins(table(markit.data$Ccy,markit.data$Sector), FUN = list(Total = sum), quiet = TRUE)
quotesbyccysector[quotesbyccysector == 0] = NA
latex.quotesbyccysector = xtable(quotesbyccysector)
print(latex.quotesbyccysector,floating=FALSE,NA.string="\\cellcolor{gray!20}",scalebox=0.58)


# ----------------------------------------------
# Main Example
# ----------------------------------------------

data = read.csv("Q:\\Projects\\GenericCreditCurve\\RiskMag\\GenericCreditCurve-SurvivalProbabilities-JPY-TECH-23Mar2015.csv")
data$Rating = factor(data$Rating, levels=c("AAA","AA","A","BBB","BB","B","CCC"),ordered=TRUE)

data.befvasicek = select(data,Rating,"1y"=SurvProb1y,"3y"=SurvProb3y,"5y"=SurvProb5y,"10y"=SurvProb10y)

data.aftvasicek = data.befvasicek
data.fullmodel = data.befvasicek

vasicek.param = data.frame(rep(NA,4),rep(NA,4),rep(NA,4),rep(NA,4))
colnames(vasicek.param) = c("1y","3y","5y","10y")
rownames(vasicek.param) = c("p","rho","u","SSE")

f = function(sp,param) {
  # sp: AAA to CCC 
  # param[1]: p
  # param[2]: rho
  # param[3]: u
  
  if (length(sp) == 7) {
    
    sumsqrdiff = 0
    for (i in seq(1,length(sp))) {
      sp.est = vasicekCDF((8-i)*param[3],param[1],param[2])
      
      if (!is.na(sp[i])) {
        sumsqrdiff = sumsqrdiff + (sp[i]-sp.est)^2
      }      
    }
    
    return(sumsqrdiff)
  }
  else {
    warning("sp vector is not length 7")
  } 
}

CalibrateCDO = function(sp) {
  
  #lowerbnd = c(0.000001, 0.000001, 0.000001)
  #upperbnd = c(0.999999, 0.999999, 0.14)
  #param0 = 0.5*(lowerbnd + upperbnd)
  
  lowerbnd = c(0.000001,0.000001)
  upperbnd = c(0.999999,0.999999)
  param0 = 0.5*(lowerbnd + upperbnd)
  
  res = optim(par = c(param0),
              fn = function(x) f(sp,c(x,0.10)), 
              method = "L-BFGS",
              lower = lowerbnd, 
              upper = upperbnd)

  res
}

# small tests
f(data.befvasicek[,4],c(0.1,0.30,0.1))
CalibrateCDO(data.befvasicek[,4])

# main loop
for (i in seq(1,ncol(data.befvasicek)-1)) {
  # perform calibration
  res = CalibrateCDO(data.befvasicek[,1+i])
  
  # save calib param
  vasicek.param[1,i] = res$par[1]
  vasicek.param[2,i] = res$par[2]
  vasicek.param[3,i] = res$par[3]
  vasicek.param[4,i] = res$value
  
  # compute missing Survival Probabilities
  for (j in seq(1,nrow(data.aftvasicek))) {
    if (is.na(data.befvasicek[j,1+i])) {
      #data.aftvasicek[j,1+i] = vasicekCDF((8-i)*res$par[3],res$par[1],res$par[2])
      data.aftvasicek[j,1+i] = vasicekCDF((8-j)*0.10,res$par[1],res$par[2])
    }
  }
  
  # compute model sp
  for (j in seq(1,nrow(data.fullmodel))) {
      data.fullmodel[j,1+i] = vasicekCDF((8-j)*0.10,res$par[1],res$par[2])
  }
  
}

# data summary
data.befvasicek
data.aftvasicek
data.fullmodel
round(vasicek.param,4)

# read spreads from csv
write.csv(t(data.aftvasicek),"Q:\\Projects\\GenericCreditCurve\\RiskMag\\spaftervasicek.csv")
data.spread.aftvasicek = read.csv("Q:\\Projects\\GenericCreditCurve\\RiskMag\\GenericCreditCurve-Spreads-JPY-TECH-23Mar2015.csv")

# plot vasicek for each tenor

#X = c(seq(0,0.01,by=0.00001),seq(0.01,0.3,by=0.0001))
X = seq(0.0,0.7,by=0.00001)

pdfcurve1y = sapply(X,function(x) vasicekPDF(x,vasicek.param[1,1],vasicek.param[2,1]))
pdfcurve3y = sapply(X,function(x) vasicekPDF(x,vasicek.param[1,2],vasicek.param[2,2]))
pdfcurve5y = sapply(X,function(x) vasicekPDF(x,vasicek.param[1,3],vasicek.param[2,3]))
pdfcurve10y = sapply(X,function(x) vasicekPDF(x,vasicek.param[1,4],vasicek.param[2,4]))

pdf.scale = max(pdfcurve1y,pdfcurve3y,pdfcurve5y,pdfcurve10y,na.rm=TRUE)
pdfcurve1y = pdfcurve1y/pdf.scale; pdfcurve3y = pdfcurve3y/pdf.scale; pdfcurve5y = pdfcurve5y/pdf.scale; pdfcurve10y = pdfcurve10y/pdf.scale;

cdfcurve1y = sapply(X,function(x) vasicekCDF(x,vasicek.param[1,1],vasicek.param[2,1]))
cdfcurve3y = sapply(X,function(x) vasicekCDF(x,vasicek.param[1,2],vasicek.param[2,2]))
cdfcurve5y = sapply(X,function(x) vasicekCDF(x,vasicek.param[1,3],vasicek.param[2,3]))
cdfcurve10y = sapply(X,function(x) vasicekCDF(x,vasicek.param[1,4],vasicek.param[2,4]))

cdf.scale = max(cdfcurve1y,cdfcurve3y,cdfcurve5y,cdfcurve10y,na.rm=TRUE)
cdfcurve1y = cdfcurve1y/cdf.scale; cdfcurve3y = cdfcurve3y/cdf.scale; cdfcurve5y = cdfcurve5y/cdf.scale; cdfcurve10y = cdfcurve10y/cdf.scale;

pdf.data = data.frame(X,curve1y=pdfcurve1y,curve3y=pdfcurve3y,curve5y=pdfcurve5y,curve10y=pdfcurve10y)
pdf.data.melt = melt(pdf.data,id.vars=("X"))
pdf.data.melt$type = "pdf"
pdf.data.melt$type = factor(pdf.data.melt$type,levels=c("pdf","cdf"),labels=c("PDF","CDF"),ordered=TRUE)
pdf.data.melt$variable = factor(pdf.data.melt$variable,levels=c("curve1y","curve3y","curve5y","curve10y"),labels=c("1Y","3Y","5Y","10Y"),ordered=TRUE)

cdf.data = data.frame(X,curve1y=cdfcurve1y,curve3y=cdfcurve3y,curve5y=cdfcurve5y,curve10y=cdfcurve10y)
cdf.data.melt = melt(cdf.data,id.vars=("X"))
cdf.data.melt$type = "cdf"
cdf.data.melt$type = factor(cdf.data.melt$type,levels=c("pdf","cdf"),labels=c("PDF","CDF"),ordered=TRUE)
cdf.data.melt$variable = factor(cdf.data.melt$variable,levels=c("curve1y","curve3y","curve5y","curve10y"),labels=c("1Y","3Y","5Y","10Y"),ordered=TRUE)

data.melt = rbind(pdf.data.melt,cdf.data.melt)

Xbar = seq(0.7,0.1,by=0.1)
cdfbar1y = sapply(Xbar,function(x) vasicekCDF(x,vasicek.param[1,1],vasicek.param[2,1]))
cdfbar3y = sapply(Xbar,function(x) vasicekCDF(x,vasicek.param[1,2],vasicek.param[2,2]))
cdfbar5y = sapply(Xbar,function(x) vasicekCDF(x,vasicek.param[1,3],vasicek.param[2,3]))
cdfbar10y = sapply(Xbar,function(x) vasicekCDF(x,vasicek.param[1,4],vasicek.param[2,4]))

cdfbar = data.frame(X=Xbar,curve1y=cdfbar1y,curve3y=cdfbar3y,curve5y=cdfbar5y,curve10y=cdfbar10y)
cdfbar.melt = melt(cdfbar,id.vars=("X"))
cdfbar.melt$type = "cdf"
cdfbar.melt$type = factor(cdfbar.melt$type,levels=c("pdf","cdf"),labels=c("PDF","CDF"),ordered=TRUE)
cdfbar.melt$variable = factor(cdfbar.melt$variable,levels=c("curve1y","curve3y","curve5y","curve10y"),labels=c("1Y","3Y","5Y","10Y"),ordered=TRUE)

#g = ggplot(cdfbar.melt)
#g = g + geom_bar(aes(x=X, y=value,group = variable),stat="identity")
#g = g + facet_grid(variable ~ .) 
#plot(g)

# Plot combine PDF and CDF
g = ggplot(data.melt)
#g = ggplot(subset(data.pdf,Tenor==0.5))
g = g + geom_line(aes(x = X, y=value, group = variable + type))
g = g + geom_vline(xintercept=seq(0,7)*0.1,colour="lightgrey", linetype = "longdash")
g = g + geom_bar(data = cdfbar.melt, aes(x=X, y=value,group = variable + type),stat="identity",fill = "grey", alpha = 0.5)
#g = g + ggtitle("Probability Density Function")
#g = g + xlab("Asset value")
#g = g + ylab("Probability mass")
g = g + facet_grid(variable ~ type) 
g = g + theme_bw()
g = g +  theme(
  strip.text.y = element_text(angle=0),
  strip.text.x = element_text(angle=0),
  axis.line = element_line(),
  axis.ticks.x = element_blank(),
#  axis.ticks.y = element_blank(),
#  axis.text.x= element_blank(),
#  axis.text.y = element_blank(),
  axis.title.x=element_blank(), 
  axis.title.y=element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(),
  panel.background = element_blank(),
  legend.position="none") 
g = g + scale_x_continuous(expand = c(0, 0),breaks=((1:7)*0.10), labels=c("CCC","B","BB","BBB","A","AA","AAA"))
g = g + scale_y_continuous(expand = c(0, 0.1))
plot(g)
ggsave(file="Q:\\Projects\\GenericCreditCurve\\RiskMag\\example-results.svg", plot=g, width=7, height=7)

# Plot PDF

g = ggplot(data.melt[data.melt$type == "PDF",])
g = g + geom_line(aes(x = X, y=value, group = variable + type))
g = g + geom_vline(xintercept=seq(0,7)*0.1,colour="lightgrey", linetype = "longdash")
g = g + geom_bar(data = cdfbar.melt[cdfbar.melt$type == "PDF",], aes(x=X, y=value,group = variable + type),stat="identity",fill = "grey", alpha = 0.5)
g = g + facet_grid(variable ~ type) 
g = g + theme_bw()
g = g +  theme(
  strip.text.y = element_text(angle=0),
  strip.text.x = element_text(angle=0),
  axis.line.x = element_line(),
  axis.line.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  #  axis.text.x= element_blank(),
  axis.text.y = element_blank(),
  axis.title.x=element_blank(), 
  axis.title.y=element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(),
  panel.background = element_blank(),
  legend.position="none") 
g = g + scale_x_continuous(expand = c(0,0),breaks=((1:7)*0.10), labels=c("CCC","B","BB","BBB","A","AA","AAA"))
g = g + scale_y_continuous(expand = c(0, 0.1))
plot(g)
ggsave(file="Q:\\Projects\\GenericCreditCurve\\QQER\\curve-pdf.png", plot=g, width=5.5, height=7)
ggsave(file="Q:\\Projects\\GenericCreditCurve\\QQER\\curve-pdf.svg", plot=g, width=3.5, height=7)

# Plot CDF

g = ggplot(data.melt[data.melt$type == "CDF",])
g = g + geom_line(aes(x = X, y=value, group = variable + type))
g = g + geom_vline(xintercept=seq(0,7)*0.1,colour="lightgrey", linetype = "longdash")
g = g + geom_bar(data = cdfbar.melt[cdfbar.melt$type == "CDF",], aes(x=X, y=value,group = variable + type),stat="identity",fill = "grey", alpha = 0.5)
g = g + facet_grid(variable ~ type) 
g = g + theme_bw()
g = g +  theme(
  strip.text.y = element_text(angle=0),
  strip.text.x = element_text(angle=0),
  axis.line.x = element_line(),
  axis.line.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  #  axis.text.x= element_blank(),
  axis.text.y = element_blank(),
  axis.title.x=element_blank(), 
  axis.title.y=element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(),
  panel.background = element_blank(),
  legend.position="none") 
g = g + scale_x_continuous(expand = c(0,0),breaks=((1:7)*0.10), labels=c("CCC","B","BB","BBB","A","AA","AAA"))
g = g + scale_y_continuous(expand = c(0, 0.1))
plot(g)
ggsave(file="Q:\\Projects\\GenericCreditCurve\\QQER\\curve-cdf.png", plot=g, width=5.5, height=7)
ggsave(file="Q:\\Projects\\GenericCreditCurve\\QQER\\curve-cdf.svg", plot=g, width=3.5, height=7)

# render arrays

x=rnorm(1000)
y=rnorm(1000)
lm1=lm(y~x)
slm1=summary(lm1)
latex(slm1)

library(xtable)
data(tli)
## Demonstrate data.frame

table.befvasicek = data.befvasicek
rownames(table.befvasicek) = c("AAA","AA","A","BBB","BB","B","CCC")
table.befvasicek$Rating = NULL
table.befvasicek = table.befvasicek*100
latex.befvasicek = xtable(table.befvasicek)
print(latex.befvasicek,floating=FALSE,NA.string="\\cellcolor{gray!20}")


table.aftvasicek = data.aftvasicek
rownames(table.aftvasicek) = c("AAA","AA","A","BBB","BB","B","CCC")
table.aftvasicek$Rating = NULL
table.aftvasicek = table.aftvasicek*100
latex.aftvasicek = xtable(table.aftvasicek)
print(latex.aftvasicek,floating=FALSE,NA.string="NA")

table.spdaftvasicek = t(data.spread.aftvasicek)
table.spdaftvasicek = table.spdaftvasicek[-1,]
table.spdaftvasicek = apply(table.spdaftvasicek, 1:2, function(x) 10000 * as.numeric(x)) # apply to all cells / convert to bps
colnames(table.spdaftvasicek) = c("1y","3y","5y","10y")
latex.spdaftvasicek = xtable(table.spdaftvasicek)
print(latex.spdaftvasicek,floating=FALSE,NA.string="NA")




