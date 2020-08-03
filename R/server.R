server = function(input, output, session) {

  source("R/models.R",local=TRUE)
  source("R/reactives.R",local=TRUE)
  source("R/1.plot.R",local=TRUE)
  source("R/1.text.R",local=TRUE)
  source("R/2.text.R",local=TRUE)
  source("R/2.plot.R",local=TRUE)
  source("R/3.text.R",local=TRUE)
  source("R/3.plot.R",local=TRUE)
  source("R/4.text.R",local=TRUE)
  source("R/4.plot.R",local=TRUE)
  
  ### UI QOL settings
  # reset form
  observeEvent(input$refresh1,  { shinyjs::reset("form1") })
  observeEvent(input$text1,     { toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".copy1", condition = input$text1) })
  observeEvent(input$captions1, { toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".caption1", condition = input$captions1) })

  observeEvent(input$refresh2, { shinyjs::reset("form2") })
  observeEvent(input$text2,    { toggle(anim = TRUE, animType = "slide", time = 0.5,  selector =".copy2", condition = input$text2) })
  observeEvent(input$captions2,{ toggle(anim = TRUE, animType = "slide", time = 0.5,  selector =".caption2", condition = input$captions2) })
  
  observeEvent(input$refresh3, { shinyjs::reset("form3") })
  observeEvent(input$text3,    { toggle(anim = TRUE, animType = "slide", time = 0.5,  selector =".copy3", condition = input$text3) })
  observeEvent(input$captions3,{ toggle(anim = TRUE, animType = "slide", time = 0.5,  selector =".caption3", condition = input$captions3) })
  
  observeEvent(input$refresh4, { shinyjs::reset("form4") })
  observeEvent(input$text4,    { toggle(anim = TRUE, animType = "slide", time = 0.5,  selector =".copy4", condition = input$text4) })
  observeEvent(input$captions4,{ toggle(anim = TRUE, animType = "slide", time = 0.5,  selector =".caption4", condition = input$captions4) })
  
  #Make sure that vaccination slider maxes out at p_c  
  #observe(updateSliderInput(session, "R0max2", max = 4+(input$R02[2]>4) floor(100*(1-1/input$R01))))

  # calculate parameters from all inputs
  params1a = reactive(get_params(input$R01[2], input$ip1, input$lp1, input$id1[1], input$le1, input$al1, input$p1/100, input$tmax1)) %>% throttle(1000)
  params1b = reactive(get_params(input$R01[1], input$ip1, input$lp1, input$id1[1], input$le1, input$al1, input$p1/100, input$tmax1)) %>% throttle(1000)
  params1c = reactive(get_params(input$R01[2], input$ip1, input$lp1, input$id1[2], input$le1, input$al1, input$p1/100, input$tmax1)) %>% throttle(1000)
  params1d = reactive(get_params(input$R01[1], input$ip1, input$lp1, input$id1[2], input$le1, input$al1, input$p1/100, input$tmax1)) %>% throttle(1000)
  
  params2a = reactive(get_params(input$R02[2],input$ip2,input$lp2, input$id2[1],   input$le2,input$al2,input$p2[2]/100, input$tmax2)) %>% throttle(1000)
  params2b = reactive(get_params(input$R02[1],input$ip2,input$lp2, input$id2[1],   input$le2,input$al2,input$p2[1]/100, input$tmax2)) %>% throttle(1000)
  params2c = reactive(get_params(input$R02[2],input$ip2,input$lp2, input$id2[2],   input$le2,input$al2,input$p2[2]/100, input$tmax2)) %>% throttle(1000)
  params2d = reactive(get_params(input$R02[1],input$ip2,input$lp2, input$id2[2],   input$le2,input$al2,input$p2[1]/100, input$tmax2)) %>% throttle(1000)
  
  params3 = reactive(get_params(input$R03, input$ip3, input$lp3, input$id3, input$le3, input$al3, 0, input$tmax3)) %>% throttle(1000)

  params4a = reactive(get_params(input$R04[2], input$ip4, input$lp4, input$id4[1], input$le4, input$al4, input$p4/100, input$tmax4)) %>% throttle(1000)
  params4b = reactive(get_params(input$R04[1], input$ip4, input$lp4, input$id4[1], input$le4, input$al4, input$p4/100, input$tmax4)) %>% throttle(1000)
  params4c = reactive(get_params(input$R04[2], input$ip4, input$lp4, input$id4[2], input$le4, input$al4, input$p4/100, input$tmax4)) %>% throttle(1000)
  params4d = reactive(get_params(input$R04[1], input$ip4, input$lp4, input$id4[2], input$le4, input$al4, input$p4/100, input$tmax4)) %>% throttle(1000)

  df1 = reactive(calculate1(params1a(),params1b(),params1c(),params1d())) %>% throttle(1000)
  p1  = reactive(    plots1(params1a(),params1b(),params1c(),params1d())) %>% throttle(1000)

  df2 = reactive(calculate2(params2a(),params2b(),params2c(),params2d())) %>% throttle(1000)
  p2  = reactive(    plots2(params2a(),params2b(),params2c(),params2d())) %>% throttle(1000)
  
  df3 = reactive(calculate3(params3())) %>% throttle(1000)
  p3  = reactive(    plots3(params3())) %>% throttle(1000)

  df4 = reactive(calculate4(params4a(),params4b(),params4c(),params4d())) %>% throttle(1000)
  p4  = reactive(    plots4(params4a(),params4b(),params4c(),params4d())) %>% throttle(1000)
  
  report_timing = function(t0,msg="timing") {
    t1 = Sys.time()
    if(do_timing) {
      print(sprintf("%s %.3f seconds",msg,t1-t0))
    }
  }
  
  source("R/titles.R",local=TRUE)
  source("R/masthead.captions.R",local=TRUE)
  
}