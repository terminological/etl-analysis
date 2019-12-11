#!/usr/bin/R
# install.packages("tikzDevice")
# devtools::install_github("thomasp85/patchwork")
library(cowplot);
library(patchwork);
library(phdUtils)

setwd("~/Dropbox/costOptimisation/")

pageWidth = 5.5

plot <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))+xlim(0,1)+theme_bw()

F <- function(a,b) {
  return(function(x) 1-(1-x^a)^b);
}

f <- function(a,b) {
  return(function(x) a*b*x^(a-1)*(1-x^a)^(b-1));
}



a=2.329462888137902
b=5.253279125193206

p1 <- plot +
  stat_function(geom="area",fun = f(a,b),colour=NA,fill="grey",alpha=0.2)+
  stat_function(fun = f(a,b),colour="black")+ylab("f(x)")

p2 <- plot +
  stat_function(fun = F(a,b),colour="blue")+ylab("F(x)")
  
p3 <- plot_grid(p1, p2, align = "hv", nrow=1, labels=c("A","B"))

p3

phdUtils::saveThesisThirdPage("figs/01_kumuraswamyDistribution")

Q <- function(a,b) {return(F(a,b))}
q <- function(a,b) {return(f(a,b))}
P <- function(a,b) {return(
  function(x){ return(1-F(a,b)(1-x))}
)}
p <- function(a,b) {return(
  function(x){ return(f(a,b)(1-x))}
)}

aP=5.86176321720409
aQ=5.86176321720409
bP=16.73555640674832
bQ=16.73555640674832

l = 0.3

p4 <- plot+
  # stat_function(geom="area",fun = function(x) {l*p(aP,bP)(x)+(1-l)*q(aQ,bQ)(x)},colour=NA,fill="grey",alpha=0.1)+
  stat_function(geom="area",fun = function(x) {l*p(aP,bP)(x)},colour=NA,fill="blue",alpha=0.1)+
  stat_function(geom="area",fun = function(x) {(1-l)*q(aQ,bQ)(x)},colour=NA,fill="red",alpha=0.1)+
  stat_function(fun = function(x) {l*p(aP,bP)(x)+(1-l)*q(aQ,bQ)(x)},colour="#404040")+
  stat_function(fun = function(x) {l*p(aP,bP)(x)},colour="blue")+
  stat_function(fun = function(x) {(1-l)*q(aQ,bQ)(x)},colour="red")+
  ylab("probability density")+xlab("prediction")
  
p5 <- plot+
  stat_function(fun = function(x) {l*P(aP,bP)(x)},colour="blue")+
  stat_function(fun = function(x) {(1-l)*Q(aQ,bQ)(x)},colour="red")+
  stat_function(fun = function(x) {l*P(aP,bP)(x)+(1-l)*Q(aQ,bQ)(x)},colour="#404040")+
  ylab("cumulative frequency")+xlab("prediction")

p6 <- plot_grid(p4, p5, align = "hv", nrow=1, labels=c("A","B"))

p6

phdUtils::saveThesisThirdPage("figs/02_classifierModel")
