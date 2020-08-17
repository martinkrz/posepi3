# EVPI

plots2 = function(params2a,params2b,params2c,params2d) {

  #      R0   p   id
  # 2a  max  max  min  M1 A1
  # 2b  min  min  min  M1 A2
  # 2c  max  max  max  M2 A1
  # 2d  min  min  max  M2 A2
  
  out   = df2()
  t0    = Sys.time()
  df1   = out[[1]]
  tmax  = params2a$tmax
  Bmax  = max(df1$B)
  #df1$B = df1$B/Bmax
  plot1 = ggplot()
  
  pmin  = min(params2a$p,params2b$p,params2c$p,params2d$p)
  pmax  = max(params2a$p,params2b$p,params2c$p,params2d$p)
  idmin = min(params2a$id,params2b$id,params2c$id,params2d$id)
  idmax = max(params2a$id,params2b$id,params2c$id,params2d$id)
  R0min = params2b$R0
  R0max = params2a$R0

  df1 = df1 %>% group_by(id,R0) %>% mutate(Bmin=min(B),Bmax=max(B)) %>% ungroup(id) %>% ungroup(R0)

  plot1 = plot1 + geom_vline(xintercept=params2a$R0,color=palette["C3"])
  plot1 = plot1 + geom_vline(xintercept=params2b$R0,color=palette["C3"])
 
  plot1 = plot1 + geom_ribbon(data=df1,aes(ymin=Bmin,ymax=Bmax,x=R0,group=m,fill=m),alpha=0.3)
  plot1 = plot1 + geom_line(data=df1,aes(x=R0,y=B,color=pstr,group=paste(m,pstr)),size=plot_line_width) 

  #plot1 = plot1 + geom_point(data=df1%>%filter(a!=FALSE),aes(x=R0,y=B,fill=pstr,color=m),stroke=1,shape=21,size=3) 

  plot1 = plot1 + geom_point(data=df1%>%filter(p==pmin & id==idmin & R0==R0min),aes(x=R0,y=B),fill=palette["p2"],color=palette["M1"],stroke=1,shape=21,size=3) 
  plot1 = plot1 + geom_point(data=df1%>%filter(p==pmin & id==idmax & R0==R0min),aes(x=R0,y=B),fill=palette["p2"],color=palette["M2"],stroke=1,shape=21,size=3) 
  plot1 = plot1 + geom_point(data=df1%>%filter(p==pmax & id==idmin & R0==R0max),aes(x=R0,y=B),fill=palette["p3"],color=palette["M1"],stroke=1,shape=21,size=3) 
  plot1 = plot1 + geom_point(data=df1%>%filter(p==pmax & id==idmax & R0==R0max),aes(x=R0,y=B),fill=palette["p3"],color=palette["M2"],stroke=1,shape=21,size=3) 


  plot1 = plot1 + geom_label(aes(x=params2b$R0,y=Bmax,label="A1"),fill=palette["p2"],color="white")
  plot1 = plot1 + geom_label(aes(x=params2a$R0,y=Bmax,label="A2"),fill=palette["p3"],color="white")  
  y1    = mean( (df1 %>% filter(R0 == max(df1$R0) & id==idmin))$B)
  y2    = mean( (df1 %>% filter(R0 == max(df1$R0) & id==idmax))$B)  
  plot1 = plot1 + geom_label(aes(x=max(df1$R0)+0.15,y=y1,label="M1"),fill=palette["M1"],alpha=0.3)
  plot1 = plot1 + geom_label(aes(x=max(df1$R0)+0.15,y=y2,label="M2"),fill=palette["M2"],alpha=0.3)

  #plot1 = plot1 + scale_color_manual(values = palette)
  #plot1 = plot1 + scale_fill_manual(values = palette)
  
  str0 = sprintf("<i>R</i><sub>0</sub> = %.1f, <i>p</i> = %.0f%%",params2b$R0,params2b$p*365*100)
  str1 = sprintf("<i>R</i><sub>0</sub> = %.1f, <i>p</i> = %.0f%%",params2a$R0,params2a$p*365*100)
  str2 = sprintf("1/<i>&omega;</i> = %.2f years",params2a$id/365)
  str3 = sprintf("1/<i>&omega;</i> = %.2f years",params2c$id/365)
  
  title1 = sprintf("Effect of management actions %s (%s) and %s (%s) on the cumulative disease burden under two immunity duration models %s (%s) and %s (%s).",varfmt("A1"),str0,varfmt("A2"),str1,varfmt("M1"),str2,varfmt("M2"),str3)
  title2 = sprintf("Calculation of the expected value of perfect information (EVPI) for two management actions %s (%s) and %s (%s) on the cumulative disease burden under two immunity duration models %s (%s) and %s (%s).",varfmt("A1"),str0,varfmt("A2"),str1,varfmt("M1"),str2,varfmt("M2"),str3)

  plot1 = plot1 + scale_fill_manual("MODEL", 
                      breaks = c("M1","M2"),                 
                      labels = c("M1","M2"),                                 
                      values = palette)
  plot1 = plot1 + scale_color_manual("VACCINATION",
                      breaks = c("p1","p2","p3"),                 
                      labels = c(0,sprintf("%.0f%%",100*365*pmin),sprintf("%.0f%%",100*365*pmax)),
                      values = palette)
  plot1 = plot1 + guides(fill  = guide_legend(order = 1,nrow=2,title.position="top"),
                         color = guide_legend(order = 0,nrow=3,title.position="top"))
  
#direction = "horizontal",
#      barwidth=10,barheight=0.5,nbin=10,ticks=TRUE,ticks.linewidth=2,title.position="top",title.hjust=0.5

  plot1 = plot1 + my.plot_axis(xlab="R0",ylab="cumulative burden, B",
                               xmin=1,
                               xmax=max(df1$R0) + 0.25,
                               ymin=0,
                               ymax=max(df1$B),
                               ysec=1,
                               ypercent=0)

  param_text = sprintf("1/<i>&gamma;</i> = %d days, 1/<i>&sigma;</i> = %d days, 1/<i>&mu;</i> = %d years and <i>&alpha;</i> = %.3f/day.",
                       #params2a$R0,
                       #params2a$beta,
                       params2a$ip,
                       params2a$lp,
                       #params2a$id/365,
                       params2a$le/365,
                       params2a$alpha)
                       
  captionb = sprintf("The cumulative burden is normalized to %s at %s for the scenario with shortest immunity (%s) and highest infectivity (%s).",
    varfmt("Bmax",Bmax,prec=2),
    varfmt("t",tmax/365,prec=1,units="years"),
    varfmt(name="1/omega",value=params2a$id/365,prec=2,units="years"),
    varfmt(name="R0",value=max(df1$R0),prec=1))
  caption1 = paste("The effect of two management actions",varfmt("A1"),sprintf("(%s)",str0),"and",varfmt("A2"),sprintf("(%s)",str1),"on the cumulative disease burden over",varfmt(value=tmax/365,prec=1,units="years"),"of two immunity duration models",varfmt("M1"),sprintf("(%s)",str2),"and",varfmt("M2"),sprintf("(%s)",str3),"for an outbreak with",param_text)
  caption1 = paste(caption1,captionb,sir_caption(tmax,params2a$p))
  caption2 = paste("The expected value of perfect information (EVPI) for two management actions",varfmt("A1"),sprintf("(%s)",str0),"and",varfmt("A2"),sprintf("(%s)",str1),"on the cumulative disease burden over",varfmt(value=tmax/365,prec=1,units="years"),"of two immunity duration models",varfmt("M1"),sprintf("(%s)",str2),"and",varfmt("M2"),sprintf("(%s)",str3),"for an outbreak with",param_text)

  report_timing(t0,"plot2")

  return(list(
    list(plot_theme(plot1)),
    list(8,9),
    list(title1,title2),
    list(caption1,caption2)))
  
}
