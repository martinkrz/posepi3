# EVPI

output$text2intro = renderPrint({ 
  paramsa = params2a()
  paramsb = params2b()
  paramsc = params2c()
  paramsd = params2d()
  
  out  = df2()  
  df   = out[[1]]
  Bmax = max(df$B)
  tmax = paramsa$tmax
   
  str0 = sprintf("<i>R</i><sub>0</sub> = %.1f, <i>p</i> = %.0f%%",paramsb$R0,paramsb$p*365*100)
  str1 = sprintf("<i>R</i><sub>0</sub> = %.1f, <i>p</i> = %.0f%%",paramsa$R0,paramsa$p*365*100)
  str2 = sprintf("1/<i>&omega;</i> = %.2f years",paramsa$id/365)
  str3 = sprintf("1/<i>&omega;</i> = %.2f years",paramsc$id/365)

  table(title="SEIRS parameters",rows=makerows(c(
    "M1",str2,
    "M2",str3,
    "A1",str0,
    "A2",str1,
        
    #"beta",paste(varfmt(value=paramsb$beta,prec=2),varfmt(value=paramsa$beta,prec=2,units="/day"),sep="&ndash;"),
    "gamma",varfmt(value=paramsa$gamma,prec=2,units="/day"),
    "sigma",varfmt(value=paramsa$sigma,prec=2,units="/day"),
    "mu",varfmt(value=365*paramsa$mu,prec=3,units="/year"),
    "alpha",varfmt(value=paramsa$alpha,prec=3,units="/day"),
    #"p",varfmt(value=365*paramsa$p,prec=0,percent=1),
    "B<sub>max</sub>",sprintf("%s at %s",varfmt(value=Bmax,prec=2),varfmt(value=tmax/365,prec=1,units="years"))    
  )))

  cat(paste("<p>..."))
  cat(paste("<p>...."))
  cat(paste("<h4>key observations</h4>"))
  cat(paste("..."))
  
})