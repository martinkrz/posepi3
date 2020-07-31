output$text2intro = renderPrint({ 
  paramsa = params2a()
  paramsb = params2b()
  paramsc = params2c()
  paramsd = params2d()
  
  df      = df2()
  
  table(title="SEIRS parameters 2",class="t2",rows=makerows(c(    
    "R0",varfmt(value=paramsa$R0,prec=1),
    "beta",varfmt(value=paramsa$beta,prec=3,units="/day"),
    "gamma",varfmt(value=paramsa$gamma,prec=3,units="/day"),
    "sigma",varfmt(value=paramsa$sigma,prec=3,units="/day"),
    "omega",varfmt(value=365*paramsa$omega,prec=3,units="/year"),
    "mu",varfmt(value=365*paramsa$mu,prec=3,units="/year"),
    "alpha",varfmt(value=paramsa$alpha,prec=3,units="/day"),
    #"T_E",varfmt(value=params2a$period/365,prec=2,units="years"),
    #"Sinf",varfmt(value=params2a$stars$S,prec=1,percent=1),
    #"Einf",varfmt(value=params2a$stars$E,prec=1,percent=1),
    #"Iinf",varfmt(value=params2a$stars$I,prec=1,percent=1),
    #"Rinf",varfmt(value=params2a$stars$R,prec=1,percent=1),
    #"A",varfmt(value=params2a$A/365,prec=1,units="years"),
    #"CFR",varfmt(value=params2a$cfr,prec=2,percent=1),
    "p",varfmt(value=paramsa$p*365,prec=0,percent=1)
    #"pcrit",varfmt(value=params2a$R0,prec=0,percent=1)
  )))
  table(title="SEIRS parameters 1",class="t1",rows=makerows(c(    
                                                     "R0",varfmt(value=paramsb$R0,prec=1),
                                                     "beta",varfmt(value=paramsb$beta,prec=3,units="/day"),
                                                     "gamma",varfmt(value=paramsb$gamma,prec=3,units="/day"),
                                                     "sigma",varfmt(value=paramsb$sigma,prec=3,units="/day"),
                                                     "omega",varfmt(value=365*paramsb$omega,prec=3,units="/year"),
                                                     "mu",varfmt(value=365*paramsb$mu,prec=3,units="/year"),
                                                     "alpha",varfmt(value=paramsb$alpha,prec=3,units="/day"),
                                                     #"T_E",varfmt(value=params2b$period/365,prec=2,units="years"),
                                                     #"Sinf",varfmt(value=params2b$stars$S,prec=1,percent=1),
                                                     #"Einf",varfmt(value=params2b$stars$E,prec=1,percent=1),
                                                     #"Iinf",varfmt(value=params2b$stars$I,prec=1,percent=1),
                                                     #"Rinf",varfmt(value=params2b$stars$R,prec=1,percent=1),
                                                     #"A",varfmt(value=params2b$A/365,prec=1,units="years"),
                                                     #"CFR",varfmt(value=params2b$cfr,prec=2,percent=1),
                                                     "p",varfmt(value=paramsb$p*365,prec=0,percent=1)
                                                     #"pcrit",varfmt(value=params2b$pc,prec=0,percent=1)
                                                     
  )))

  cat(paste("<p>..."))
  cat(paste("<p>...."))
  cat(paste("<h4>key observations</h4>"))
  cat(paste("..."))
  
})