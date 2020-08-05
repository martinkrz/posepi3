# EVPI

output$evpi = renderPrint({ 
  paramsa = params2a()
  paramsb = params2b()
  paramsc = params2c()
  paramsd = params2d()
  
  out   = df2()  
  df    = out[[1]]
  Bmax  = max(df$B)
  tmax  = paramsa$tmax
  
  B11 = df %>% filter(a=="A1" & m=="M1") %>% select(B) %>% first() / Bmax
  B12 = df %>% filter(a=="A2" & m=="M1") %>% select(B) %>% first() / Bmax
  B21 = df %>% filter(a=="A1" & m=="M2") %>% select(B) %>% first() / Bmax
  B22 = df %>% filter(a=="A2" & m=="M2") %>% select(B) %>% first() / Bmax
   
  str0 = sprintf("<i>R</i><sub>0</sub> = %.1f, <i>p</i> = %.0f%%",paramsb$R0,paramsb$p*365*100)
  str1 = sprintf("<i>R</i><sub>0</sub> = %.1f, <i>p</i> = %.0f%%",paramsa$R0,paramsa$p*365*100)
  str2 = sprintf("1/<i>&omega;</i> = %.2f years",paramsa$id/365)
  str3 = sprintf("1/<i>&omega;</i> = %.2f years",paramsc$id/365)

  M = matrix(c(B11,B12,B21,B22),nrow=2,byrow=TRUE)
  
  avgs = colMeans(M)
  opts = apply(M,1,min)           

  evpi_table(title="Expected value of perfect information (EVPI)",
             models=c(paramsa$id/365,paramsc$id/365),
             actionsR0=c(paramsb$R0,paramsa$R0),
             actionsp=c(paramsb$p*365*100,paramsa$p*365*100),
             matrix=M)
 
})