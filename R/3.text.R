output$text3intro = renderPrint({ 
  params = params3()
  
  df   = df3()
  
  table(title="SEIRS parameters 2",class="t2",rows=makerows(c(    
    "R0",varfmt(value=params$R0,prec=1),
    "beta",varfmt(value=params$beta,prec=3,units="/day"),
    "gamma",varfmt(value=params$gamma,prec=3,units="/day"),
    "sigma",varfmt(value=params$sigma,prec=3,units="/day"),
    "omega",varfmt(value=365*params$omega,prec=3,units="/year"),
    "mu",varfmt(value=365*params$mu,prec=3,units="/year"),
    "alpha",varfmt(value=params$alpha,prec=3,units="/day"),
    "T_E",varfmt(value=params$period/365,prec=2,units="years"),
    "Sinf",varfmt(value=params$stars$S,prec=1,percent=1),
    "Einf",varfmt(value=params$stars$E,prec=1,percent=1),
    "Iinf",varfmt(value=params$stars$I,prec=1,percent=1),
    "Rinf",varfmt(value=params$stars$R,prec=1,percent=1),
    "A",varfmt(value=params$A/365,prec=1,units="years"),
    "CFR",varfmt(value=params$cfr,prec=2,percent=1),
    "p",varfmt(value=params$p,prec=0,percent=1),
    "pcrit",varfmt(value=params$R0,prec=0,percent=1)
  )))
  table(title="SEIRS parameters 1",class="t1",rows=makerows(c(    
                                                     "R0",varfmt(value=params$R0,prec=1),
                                                     "beta",varfmt(value=params$beta,prec=3,units="/day"),
                                                     "gamma",varfmt(value=params$gamma,prec=3,units="/day"),
                                                     "sigma",varfmt(value=params$sigma,prec=3,units="/day"),
                                                     "omega",varfmt(value=365*params$omega,prec=3,units="/year"),
                                                     "mu",varfmt(value=365*params$mu,prec=3,units="/year"),
                                                     "alpha",varfmt(value=params$alpha,prec=3,units="/day"),
                                                     "T_E",varfmt(value=params$period/365,prec=2,units="years"),
                                                     "Sinf",varfmt(value=params$stars$S,prec=1,percent=1),
                                                     "Einf",varfmt(value=params$stars$E,prec=1,percent=1),
                                                     "Iinf",varfmt(value=params$stars$I,prec=1,percent=1),
                                                     "Rinf",varfmt(value=params$stars$R,prec=1,percent=1),
                                                     "A",varfmt(value=params$A/365,prec=1,units="years"),
                                                     "CFR",varfmt(value=params$cfr,prec=2,percent=1),
                                                     "p",varfmt(value=params$p,prec=0,percent=1),
                                                     "pcrit",varfmt(value=params$pc,prec=0,percent=1)
                                                     
  )))

  cat(paste("<p>..."))
  cat(paste("<p>...."))
  cat(paste("<h4>key observations</h4>"))
  cat(paste("..."))
  
})