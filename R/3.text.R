# Vaccination

output$text3intro = renderPrint({ 
  params = params3()
  
  df   = df3()
  tmax = params$tmax
  Bmax = max(df$B)  
    
  table(title="SEIRS parameters",rows=makerows(c(
    "R0",   varfmt(value=params$R0,prec=1),
    "omega",varfmt(value=365*params$omega,prec=2,units="/year"),
    "beta", varfmt(value=params$beta,prec=2,units="/day"),
    "gamma",varfmt(value=params$gamma,prec=2,units="/day"),
    "sigma",varfmt(value=params$sigma,prec=2,units="/day"),
    "mu",   varfmt(value=365*params$mu,prec=3,units="/year"),
    "alpha",varfmt(value=params$alpha,prec=3,units="/day"),
    "p",    sprintf("%.0f &ndash; %.0f%%",100*365*min(df$p),100*365*max(df$p)),
    "B<sub>max</sub>",sprintf("%s at %s",varfmt(value=Bmax,prec=2),varfmt(value=tmax/365,prec=1,units="years"))
  )))

  cat(paste("<p>..."))
  cat(paste("<p>...."))
  cat(paste("<h4>key observations</h4>"))
  cat(paste("..."))
  
})