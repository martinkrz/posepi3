output$text1intro = renderPrint({ 

  params1a = params1a()
  params1b = params1b()
  params1c = params1c()
  params1d = params1d()
  params   = params1a
 
  out     = df1() 
  df1a    = out[[1]]  # R0max idmin
  df1b    = out[[2]]  # R0min idmin
  df2a    = out[[3]]  # R0max idmin
  df2b    = out[[4]]  # R0min idmin
  
  tmax    = max(df1a$time)
  # max burden
  Bmax = max(df1a$B)
  
  table(title="SEIRS parameters",rows=makerows(c(
    "R0",paste(varfmt(value=params1b$R0,prec=1),varfmt(value=params1a$R0,prec=1),sep="&ndash;"),
    "omega",paste(varfmt(value=365*params1a$omega,prec=2),varfmt(value=365*params1c$omega,prec=2,units="/year"),sep="&ndash;"),
    "beta",paste(varfmt(value=params1b$beta,prec=2),varfmt(value=params1a$beta,prec=2,units="/day"),sep="&ndash;"),
    "gamma",varfmt(value=params$gamma,prec=2,units="/day"),
    "sigma",varfmt(value=params$sigma,prec=2,units="/day"),
    "mu",varfmt(value=365*params$mu,prec=3,units="/year"),
    "alpha",varfmt(value=params$alpha,prec=3,units="/day"),
    "p",varfmt(value=365*params$p,prec=0,percent=1),
    "B<sub>max</sub>",sprintf("%s at %s",varfmt(value=Bmax,prec=2),varfmt(value=tmax/365,prec=1,units="years"))
    #"T_E",varfmt(value=params$period/365,prec=2,units="years"),
    #"Sinf",varfmt(value=params$stars$S,prec=1,percent=1),
    #"Einf",varfmt(value=params$stars$E,prec=1,percent=1),
    #"Iinf",varfmt(value=params$stars$I,prec=1,percent=1),
    #"Rinf",varfmt(value=params$stars$R,prec=1,percent=1),
    #"A",varfmt(value=params$A/365,prec=1,units="years"),
    #"CFR",varfmt(value=params$cfr,prec=2,percent=1),  
  )))
  
  cat(paste("<p>These interactive figures show ...")) #predictions from the SEIRS model (see Equations tab) change as a function of parameters such infectious period ",varfmt("(1/gamma),"),"basic reproduction number",varfmt("(R0),"),"latent period",varfmt("(1/sigma),"),"immunity duration",varfmt("(1/omega),"),"life expectancy",varfmt("(1/mu),"),"death onset",varfmt("(1/alpha)"),"and vaccination level",varfmt("p."),sep=" ")) 
  cat(paste("<h4>key observations</h4>"))
  cat(paste("<p>...")) #When",varfmt("R0 > 1"),"and when there is susceptibility recruitment through loss of immunity",varfmt("(omega)"),"or birth",varfmt("(mu)"),"the epidemic is said to be 'open'. The SEIRS trajectories eventually reach endemic equilibrium:",varfmt("Sinf = 1/R0",value=1/params$R0,prec=1,percent=1),"and both ",varfmt("Einf",params$stars$E,prec=1,percent=1),"and",varfmt("Iinf",params$stars$I,prec=1,percent=1),"will be non-zero.")) 
  cat(paste("<p>...")) # On the way to equilibrium, the trajectories exhibit epidemic waves whose period",varfmt("T_E",params$period/365,units="years",prec=2),"(inter-epidemic interval) is a function of model parameters. These waves should not be confused with seasonal epidemic trends, which are a result of temporal variation in",varfmt("beta.")))
  
  if(params$p > 0) {
    cat(paste("<p>The annual vaccination rate...",varfmt("p",value=365*params$p,prec=0,percent=1),"influences ..."))
  }
  cat("<br clear=both>")

})

