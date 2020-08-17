output$text1intro = renderPrint({ 

  params1a = params1a()
  params1b = params1b()
  params1c = params1c()
  params1d = params1d()
  params   = params1a
 
  data = df1() 

  out   = data[[1]]
  peaks = data[[2]]

  df1a = out %>% filter(R0==params1a$R0 & id==params1a$id) # R0max idmin
  df1b = out %>% filter(R0==params1b$R0 & id==params1b$id) # R0min idmin
  df2a = out %>% filter(R0==params1c$R0 & id==params1c$id) # R0max idmax
  df2b = out %>% filter(R0==params1d$R0 & id==params1d$id) # R0min idmax
 
  tmax = params1a$tmax  
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
    "B<sub>max</sub>",sprintf("%s at %s",varfmt(value=Bmax,prec=2),varfmt(value=tmax/365,prec=2,units="years"))
  )))
  
  cat(paste("<p>These interactive figures show the impact of parameter uncertainty on two important metrics of an outbreak: infection peaks and cumulative disease burden. We will use a SEIRS model (see Equation tab) and visualize the infection peaks using the trajectory of",varfmt("I(t)"),"and the cumulative disease burden using the total number of cases",varfmt("B(t)"),"from the start of the outbreak.")) 
  cat(paste("<p>To illustrate the impact of uncertainty, we will model a range of values for the basic reproduction number",varfmt("R0",paren=1),"and immunity duration",sprintf("(%s).",varfmt("1/omega")),"We highlight each peak flu season (Nov-Feb) and include the possibility for continuous vaccination of ",varfmt("p"),"fraction of the population per year. Vaccination can have substantial impact on how the outbreak progresses, even if the vaccine is only partially effective."))
  cat(paste("<p>We assume that all other parameters are accurately estimated. Practically, parameters associated with fast rates are the first to be accurately assessed and those for slow rates, such as loss of immunity rate",varfmt("omega,"),"may take several years to determine."))
  
  t0 = peaks %>% filter(R0==params1a$R0 & id==params1a$id & i == 1) %>% select(time) %>% first
  I0 = peaks %>% filter(R0==params1a$R0 & id==params1a$id & i == 1) %>% select(y) %>% first
  t1 = peaks %>% filter(R0==params1b$R0 & id==params1a$id & i == 1) %>% select(time) %>% first
  I1 = peaks %>% filter(R0==params1b$R0 & id==params1a$id & i == 1) %>% select(y) %>% first
  t2 = peaks %>% filter(R0==params1a$R0 & id==params1a$id & i == 2) %>% select(time) %>% first
  I2 = peaks %>% filter(R0==params1a$R0 & id==params1a$id & i == 2) %>% select(y) %>% first
  t3 = peaks %>% filter(R0==params1b$R0 & id==params1a$id & i == 2) %>% select(time) %>% first
  I3 = peaks %>% filter(R0==params1b$R0 & id==params1a$id & i == 2) %>% select(y) %>% first
  
  t2L = peaks %>% filter(R0==params1a$R0 & id==params1c$id & i == 2) %>% select(time) %>% first
  I2L = peaks %>% filter(R0==params1a$R0 & id==params1c$id & i == 2) %>% select(y) %>% first
  t3L = peaks %>% filter(R0==params1b$R0 & id==params1c$id & i == 2) %>% select(time) %>% first
  I3L = peaks %>% filter(R0==params1b$R0 & id==params1c$id & i == 2) %>% select(y) %>% first
  
  cat(paste("<h4>key observations</h4>"))
  cat(paste("<p>Uncertainty in",varfmt("R0"),"has a large impact on the prediction of the position and size of the first infection peak. When",varfmt("R0,",params1a$R0,prec=1),"peak infections are high",varfmt("I",I0),"and come early",varfmt("t.",t0/365),"For an outbreak with a lower",varfmt("R0,",params1b$R0,prec=1),"peak infections are low",varfmt("I",I1),"and happen later",varfmt("t.",t1/365),"A lower",varfmt("R0"),"also results in a lower cumulative burden. For",varfmt("R0,",params1a$R0,prec=1),"the burden at",varfmt("t",tmax/365),"is ",varfmt("B",max(df1b$B),prec=2),"which is",varfmt("B",max(df1b$B)/Bmax,prec=1,percent=1),"of the burden with high",varfmt("R0,",params1a$R0),sprintf("(%s).",varfmt("Bmax",Bmax,prec=2)),"This is the 'flattening the curve' effect&mdash;achieved by a lower",varfmt("R0"),"either through mitigation or a more accurate assessment that lowers the estimate."))
  cat(paste("<p>Because the latency",varfmt("(sigma)"),"transmission",varfmt("(beta)"),"and recovery",varfmt("(gamma)"),"rates are typically much faster than rate of immunity loss",varfmt("(omega)"),"uncertainty in immunity duration does not have a substantial impact on the first peak of the outbreak. During this peak, only a small part of cases are reinfections. However, increasing immunity duration from short",varfmt("1/omega",params1a$id/365,unit="years",prec=2,paren=1),"to long",varfmt("1/omega",params1c$id/365,unit="years",prec=2,paren=1),"has a substantial effect on delaying (but not flattening) the second peak. With long immunity, the second peak at",varfmt("R0,",params1a$R0),"is delayed from",varfmt("t",t2/365),"to",varfmt("t",t2L/365),"and at",varfmt("R0,",params1b$R0),"it is delayed from",varfmt("t",t3/365),"to",varfmt("t.",t3L/365),"Longer immunity also lowers the cumulative burden, because it delays infection peaks. When immunity duration is long, the burden at",varfmt("t",tmax/365),"drops to",varfmt("B",max(df2a$B)/Bmax,prec=1,percent=1),"and",varfmt("B",max(df2b$B)/Bmax,prec=1,percent=1),"for high and low values of ",varfmt("R0,"),"respectively, when compared to",varfmt("Bmax.")))

    cat(paste("<p>The possibility of coinfections during the annual peak flu season make accurate assessment of",varfmt("R0"),"and immunity duration important&mdash;depending on the exact values, infection peaks may fall within the peak flu season."))
  
  
  if(params$p > 0) {
    cat(paste("<p>The annual vaccination rate...",varfmt("p",value=365*params$p,prec=0,percent=1),"influences ..."))
  }
  cat("<br clear=both>")

})

