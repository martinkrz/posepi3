# Cumulative disease burden

output$text4intro = renderPrint({ 
  paramsa = params4a()
  paramsb = params4b()
  paramsc = params4c()
  paramsd = params4d()
  
  params = paramsa

  df   = df4()
  Bmax = max(df$B)
  tmax = max(df$time)
  
  table(title="SEIRS parameters",rows=makerows(c(
    "R0",paste(varfmt(value=paramsb$R0,prec=1),varfmt(value=paramsa$R0,prec=1),sep="&ndash;"),
    "omega",paste(varfmt(value=365*paramsa$omega,prec=2),varfmt(value=365*paramsc$omega,prec=2,units="/year"),sep="&ndash;"),
    "beta",paste(varfmt(value=paramsb$beta,prec=2),varfmt(value=paramsa$beta,prec=2,units="/day"),sep="&ndash;"),
    "gamma",varfmt(value=paramsa$gamma,prec=2,units="/day"),
    "sigma",varfmt(value=paramsa$sigma,prec=2,units="/day"),
    "mu",varfmt(value=365*paramsa$mu,prec=3,units="/year"),
    "alpha",varfmt(value=paramsa$alpha,prec=3,units="/day"),
    "p",varfmt(value=365*paramsa$p,prec=0,percent=1),
    "B<sub>max</sub>",sprintf("%s at %s",varfmt(value=Bmax,prec=2),varfmt(value=tmax/365,prec=1,units="years"))    
  )))

  cat(paste("<p>..."))
  cat(paste("<p>...."))
  cat(paste("<h4>key observations</h4>"))
  cat(paste("..."))
  
})