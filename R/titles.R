# This is messy but I don't know how else to do it.

output$title1 = renderPrint({
  cat("<div class=paneltitle>The SEIRS model of infection spread</div>")
})

output$title1a = renderPrint({
  index = p1()[[2]][[1]]
  title = p1()[[3]][[1]]
  cat(figure_title(index, title))
})
output$title1b = renderPrint({
  index = p1()[[2]][[2]]
  title = p1()[[3]][[2]]
  cat(figure_title(index, title))
})
output$title1c = renderPrint({
  index = p1()[[2]][[3]]
  title = p1()[[3]][[3]]
  cat(figure_title(index, title))
})
output$title1d = renderPrint({
  index = p1()[[2]][[4]]
  title = p1()[[3]][[4]]
  cat(figure_title(index, title))
})

output$plot1a = renderPlot({
  p1()[[1]][[1]]
})
output$plot1b = renderPlot({
  p1()[[1]][[2]]
})
output$plot1c = renderPlot({
  p1()[[1]][[3]]
})
output$plot1d = renderPlot({
  p1()[[1]][[4]]
})

output$caption1a = renderPrint({
  cat(paste("<p>", p1()[[4]][[1]], "</p>", sep = ""))
})
output$caption1b = renderPrint({
  cat(paste("<p>", p1()[[4]][[2]], "</p>", sep = ""))
})
output$caption1c = renderPrint({
  cat(paste("<p>", p1()[[4]][[3]], "</p>", sep = ""))
})
output$caption1d = renderPrint({
  cat(paste("<p>", p1()[[4]][[4]], "</p>", sep = ""))
})

output$title2a = renderPrint({
  index = p2()[[2]][[1]]
  title = p2()[[3]][[1]]
  cat(figure_title(index, title))
})
output$plot2a = renderPlot({
  p2()[[1]][[1]]
}) #,height=600,width="auto")
output$caption2a = renderPrint({
  cat(paste("<p>", p2()[[4]][[1]], "</p>", sep = ""))
})

output$title3a = renderPrint({
  index = p3()[[2]][[1]]
  title = p3()[[3]][[1]]
  cat(figure_title(index, title))
})
output$title3b = renderPrint({
  index = p3()[[2]][[2]]
  title = p3()[[3]][[2]]
  cat(figure_title(index, title))
})

output$plot3a = renderPlot({
  p3()[[1]][[1]]
})
output$plot3b = renderPlot({
  p3()[[1]][[2]]
})

output$caption3a = renderPrint({
  cat(paste("<p>", p3()[[4]][[1]], "</p>", sep = ""))
})
output$caption3b = renderPrint({
  cat(paste("<p>", p3()[[4]][[2]], "</p>", sep = ""))
})

