panel.lm = function(x,y,...){
  points(x,y,...)
  abline(a = lm(y ~ x)$coefficients[1] , b = lm(y ~ x)$coefficients[2] , ...)
}

panel.cor = function(x, y, digits=2, prefix="", cex.cor){
  usr = par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r = cor(x, y)
  txt = format(c(r, 0.123456789), digits=digits)[1] 
  txt = paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex = 0.8/strwidth(txt) 
  
  test = cor.test(x,y) 
  # borrowed from printCoefmat
  Signif = symnum(test$p.value, corr = FALSE, na = FALSE, 
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex=3) 
  text(0.8, 0.8, Signif, cex=3, col=2) 
}