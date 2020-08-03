# Cumulative burden

plots4 = function(paramsa,paramsb,paramsc,paramsd) {

  df    = df4()  
  t0    = Sys.time()
  tmax  = paramsa$tmax
  Bmax  = max(df$B)
  #df$B  = df$B/Bmax
  log   = input$log4
  plot1 = ggplot()
 
  R0max = paramsa$R0
  plot1 = plot1 + geom_line(data=df,aes(x=R0,y=B,group=id,color=id/365),size=plot_line_width)

  plot1 = plot1 + geom_label(data=df %>% filter(R0==R0max),aes(x=R0,y=B,label=varfmt(value=id/365,prec=2)),hjust="left",fill="white",size=4,nudge_x=0.025)


  ids        = sort(unique(df$id/365))
  ids_labels = c()
  for(i in 1:length(ids)) {
    id = ids[i]    
    if(is_int(id) | i == 1 | i == length(ids)) {
      label = id      
    } else {
      label = ""
    }
    ids_labels = append(ids_labels,label)
  }

  plot1 = plot1 + 
    scale_colour_gradient(
      "immunity duration (years)",
      low = "#006837",
      high = "#8cc63f",
      space = "Lab",
      aesthetics = "color",      
      guide = guide_colourbar(direction = "horizontal",
      barwidth=10,barheight=0.5,nbin=length(ids),ticks=TRUE,ticks.linewidth=2,title.position="top",title.hjust=0.5),
      breaks=ids,
      labels=ids_labels
    ) +  
    my.plot_axis(xlab="R0",ylab="cumulative burden, B",
                           yseclab="normalized cumulative burden, B",
                               xmin = paramsb$R0,
                               xmax = paramsa$R0 + 0.25,
                               ymin      = round(min(df$B),1)-.1,
                               ylog10min = min(df$B),                               
                               ysec = 1,
                               ymax = Bmax,
                               ypercent = 0)

  str0  = sprintf("over the first %s of an outbreak.",
                  varfmt(name="tmax",value=tmax/365,prec=1,units="years"))
                  
  title1 = sprintf("Effect of <i>R</i><sub>0</sub> and immunity duration on the normalized cumulative disease burden, <i>B</i>, %s",str0)
  
  param_text = sprintf("<i>R</i><sub>0</sub> = %.1f&ndash;%.1f (in steps of %.2f), 1/<i>&beta;</i> = %.2f&ndash;%.2f days, 1/<i>&gamma;</i>
  = %d days, 1/<i>&sigma;</i> = %d days, 1/<i>&omega;</i> = %.2f&ndash;%.2f years (in steps of %.2f, labeled), 1/<i>&mu;</i> = %d years and <i>&alpha;</i> = %.3f/day with %.0f%% annual vaccination rate.", paramsa$R0, paramsb$R0, R0_step4, paramsa$beta, paramsb$beta, paramsa$ip, paramsa$lp,
  paramsa$id/365,paramsc$id/365,R0_step4,paramsa$le/365,paramsa$alpha, paramsa$p*365*100)

  captionb = sprintf("The cumulative burden is normalized to %s at %s for the scenario with shortest immunity (%s) and highest infectivity (%s).",
    varfmt("Bmax",Bmax,prec=2),
    varfmt("t",tmax/365,prec=1,units="years"),
    varfmt(name="1/omega",value=paramsa$id/365,prec=1,units="years"),
    varfmt(name="R0",value=paramsa$R0,prec=1))

  caption1 = paste("The normalized cumulative disease burden, <i>B</i>, over the first",varfmt("tmax",tmax/365,prec=1,units="years"),"of outbreak scenarios with",param_text,captionb,sir_caption(tmax,params$p))
  
  report_timing(t0,"plot4")
  
  return(list(
    list(plot_theme(plot1)),
    list(5),
    list(title1),
    list(caption1)))
  
}
