### Effect of vaccination

plots3 = function(params) {

  df    = df3()  
  t0    = Sys.time()
  tmax  = params$tmax
  Bmax  = max(df$B)
  #df$B  = df$B/Bmax
  plot1 = ggplot()
  plot2 = ggplot()
  
  plot1 = plot1 + geom_line(data=df %>% filter(p==0),aes(x=time/365,y=B),size=plot_line_width,color=palette["C3"])
  plot1 = plot1 + geom_line(data=df %>% filter(p>0), aes(x=time/365,y=B,group=p,color=round(100*p*365)),size=plot_line_width) 

  plot1 = plot1 + geom_label(data=df %>% filter(time==tmax),aes(x=tmax/365,y=B,label=100*365*p),hjust="left",fill="white",nudge_x = 0.1)

  plot2 = plot2 + geom_path(data=df %>% filter(p==0),aes(x=S,y=I),size=plot_line_width,color=palette["C3"])
  plot2 = plot2 + geom_path(data=df %>% filter(p>0),aes(x=S,y=I,group=p,color=round(100*365*p)),size=plot_line_width)# + scale_color_distiller(palette = "Spectral")

  plot1 = plot1 + scale_colour_gradient(
      "annual vaccination rate (%)",
      low   = "#006837",
      high  = "#8cc63f",
      space = "Lab",
      aesthetics = "color",      
      guide = guide_colourbar(direction = "horizontal",
      barwidth=10,barheight=0.5,nbin=10,ticks=TRUE,ticks.linewidth=2,title.position="top",title.hjust=0.5),
      breaks=c(10,30,50,100),
      labels=c(10,30,50,100)
    )
  plot2 = plot2 + scale_colour_gradient(
      "annual vaccination rate (%)",
      low   = "#006837",
      high  = "#8cc63f",
      space = "Lab",
      aesthetics = "color",      
      guide = guide_colourbar(direction = "horizontal",
      barwidth=10,barheight=0.5,nbin=10,ticks=TRUE,ticks.linewidth=2,title.position="top",title.hjust=0.5),
      breaks=c(10,30,50,100),
      labels=c(10,30,50,100)
    )

  plot1 = plot1 + my.plot_axis(xlab="time (years)",
                               ylab="cumulative burden, B",
                               yseclab="normalized cumulative burden, B (%)",
                               xmin=0,
                               xmax=tmax/365+0.25,
                               ymin=0,
                               ymax=Bmax,
                               ypercent=0,
                               ysec=1)

  plot2 = plot2 + my.plot_axis(xlab="susceptible fraction (%)",ylab="infected fraction (%)",
                               xmin=min(df$S),
                               xmax=max(df$S),
                               ymin=0,
                               ymax=max(df$I),
                               xpercent=1,
                               ypercent=1)

  param_text = sprintf("<i>R</i><sub>0</sub> = %.1f, 1/<i>&beta;</i> = %.2f, 1/<i>&gamma;</i>
  = %d days, 1/<i>&sigma;</i> = %d days, 1/<i>&omega;</i> = %.2f years, 1/<i>&mu;</i> = %d years and <i>&alpha;</i> = %.3f/day.", params$R0, params$beta, params$ip, params$lp,
  params$id/365,params$le/365,params$alpha)

  str0  = sprintf("over the first %s of an outbreak.",
                  varfmt(name="tmax",value=tmax/365,prec=1,units="years"))
  str1  = sprintf("(<i>p</i> = %.0f&ndash;%.0f%% in steps of %.0f%%)",
                  p3_min*100,p3_max*100,p3_step*100)

  title1 = sprintf("Effect of annual vaccination rate, <i>p</i>, on the cumulative disease burden, <i>B</i>, %s",str0)
  title2 = sprintf("Effect of annual vaccination rate, <i>p</i>, on the phase plane of <i>I</i>(<i>t</i>) vs <i>S</i>(<i>t</i>) %s",str0)
  
  captionb = sprintf("The cumulative burden is normalized to %s at %s.",
    varfmt("Bmax",Bmax,prec=2),
    varfmt("t",tmax/365,prec=1,units="years"))
    
  caption1 = paste("The normalized cumulative disease burden, <i>B</i>, with varying levels of annual vaccination",str1,"over the first",varfmt("tmax",tmax/365,prec=1,units="years"),"of an outbreak with",param_text,captionb,sir_caption(tmax,params$p))
  caption2 = paste("The normalized cumulative disease burden, <i>B</i>, with varying levels of annual vaccination",str1,"over the first",varfmt("tmax",tmax/365,prec=1,units="years"),"of an outbreak with",param_text,captionb,sir_caption(tmax,params$p))
  caption2 = paste("The SEIRS model phase plane of",varfmt("I(t)"),"vs",varfmt("S(t)"),"with varying levels of annual vaccination",str1,"over the first",varfmt("tmax",tmax/365,prec=1,units="years"),"of an outbreak with",param_text,captionb,sir_caption(tmax,params$p))

  report_timing(t0,"plot3")
  
  return(list(
    list(plot_theme(plot1),plot_theme(plot2)),
    list(6,7),
    list(title1,title2),
    list(caption1,caption2)))
  
}
