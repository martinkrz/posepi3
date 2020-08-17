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

  cat(paste("<p>Vaccination, even if partially effective, can slow or even halt an outbreak. Here, we model vaccination as a continuous process that vaccinates a fraction",varfmt("p"),"of the population per year. At each time step, vaccination directly transfers individuals from the susceptible to the recovered group (see Equation tab)."))
  cat(paste("<h4>key observations</h4>"))
  cat(paste("<p>If the vaccination fraction is sufficiently high, the second peak may never happen. This is because immunity gain from vaccination is depleting the pool of susceptible individuals."))
  cat(paste("<p>For a one-time vaccination at birth that confers permanent immunity, the critical vaccination fraction that achieves herd immunity is",varfmt("pcrit = 1-1/R0.",1-1/params$R0,prec=0,percent=1),"However, because in our case we are vaccinating continuously, the fraction to halt the outbreak will be higher. We explored one-time vaccination in the other columns in this series (see Downloads & Credits tab)."))
})