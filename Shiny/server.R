
function(input, output){
  # output$hist=renderGvis(
  #   gvisHistogram(state_stat[,input$selected,drop=F])
  # )
  
  label <- paste(scores$school_name, 'Address:', scores$address, ', Zip:', scores$zip,
                 ', Borough', scores$borough, ', Enrollment:', scores$enrollment,
                 ', SAT Math:', scores$avg_score_math,
                 ', SAT Reading:', scores$avg_score_reading, ', SAT Writing:', 
                 scores$avg_score_writing, ', Percent Tested:', scores$percent_tested)
  
  output$map <- renderLeaflet({
    leaflet(scores) %>% 
    addTiles() %>% 
    addMarkers(~scores$longitude, ~scores$latitude, 
               label = scores$school_name,
               popup = label) 
  })
  # output$map <- 
    
    
  #   renderGvis({
  #   gvisGeoChart(state_stat, "state.name", input$selected,
  #                options=list(region="US", displayMode="regions", 
  #                             resolution="provinces",
  #                             width="auto", height="auto"))
  #   # using width="auto" and height="auto" to
  #   # automatically adjust the map size
  # })
  
  output$plot1 <- renderPlotly({
    plot_ly(data = full, x = ~avg_score_math, y = ~avg_score_reading, 
            text = ~school_name, type = 'scatter', jitter = .3, color = ~borough,
            colors = 'Set1', marker = list(opacity = .5)) %>% 
      layout(title ='Scores By School', xaxis = list(title = 'Avg Math SAT Score'), 
             yaxis = list(title = 'Avg Reading SAT Score'))
    
  })
  
  output$plot2 <- renderPlotly({
    plot_ly(data = full, x = ~avg_score_math, y = ~log(household_income), color = ~borough,
            colors = 'Set1', text = ~school_name, type = 'scatter', jitter = .3,
            marker = list(opacity = .5)) %>% 
      layout(title ='Scores By Income', xaxis = list(title = 'Avg Math SAT Score'), 
             yaxis = list(title = 'Household Income (log)'))
  })
  
  output$plot99 <- renderPlotly({
    plot_ly(data = full, x = ~avg_score_math, y = ~log(household_income), color = ~borough,
            colors = 'Set1', text = ~school_name, type = 'scatter', jitter = .3,
            marker = list(opacity = .5)) %>% 
      layout(title ='Scores By Income', xaxis = list(title = 'Avg Math SAT Score'), 
             yaxis = list(title = 'Household Income (log)'))
  })
  
  output$plot3 <- renderPlotly({
      plot_ly(data = scores, y = ~avg_score_math, type = 'box', name = 'Math', 
              text = ~school_name) %>% 
      add_trace(y = ~avg_score_reading, type = 'box', name = 'Reading', 
                text = ~school_name) %>% 
      add_trace(y = ~avg_score_writing, type = 'box', name = 'Writing', 
                text = ~school_name) %>% 
      layout(yaxis = list(title = ''), title = 'SAT Scores In NYC Public High Schools', 
             showlegend = F, colors = 'set1')
  })
  
  
  output$plot4 <- renderPlot({
      ggplot(scores_total, aes(x = sat_total)) +
      stat_density(aes(color=borough), geom="line",position="identity") +
      labs(title='Density of Avg SAT Score By Borough', 
           x='Score', 
           y='Density') +
      scale_colour_brewer(palette='Set1') +
      theme_bw()
    
  })
  
  output$plot5 <- renderPlot({
    ggplot(priv, aes(x = year, y=enroll, group=1)) +
      geom_point(shape=21, color="black", fill="orange", size=6) +
      geom_line() +
      labs(title='NYC Private School Enrollment', 
           x='', 
           y='Enrollment') +
      ylim(60000, 80000) +
      scale_colour_brewer(palette='Set1') +
      theme_bw()
  })
  
  output$plot6 <- renderPlotly({
    plot_ly(data = chart_count, x = ~year_open, y = ~count, type = 'bar', colors = 'Set1') %>% 
      layout(title = 'New Charter School Openings By Year', xaxis = list(title = 'Year'), 
             yaxis = list(title = 'New Openings'))
  })
  
  output$plot7 <- renderPlotly({
    plot_ly(data = char_borough, x = ~Borough, y = ~count, type = 'bar', colors = 'Set1') %>% 
      layout(title = 'Charter Schools by Borough', xaxis = xform3, 
             yaxis = list(title = 'Count'))
  })
  
  output$plot8 <- renderPlotly({
    plot_ly(comp, x = ~type, y = ~Reading, name = 'Reading', type = 'bar', colors = 'Set1') %>% 
      add_trace(y = ~Math, name = 'Math') %>% 
      add_trace(y = ~Writing, name = 'Writing') %>% 
      layout(title = 'SAT Scores By School Type', xaxis = xform2, 
             yaxis = list(title = 'Test Score'))
  })
  
  output$plot9 <- renderPlotly({
  plot_ly(scores_income, x = ~income, y = ~`Critical Reading`, name = 'Reading', type = 'bar', colors = 'Set1') %>% 
    add_trace(y = ~Math, name = 'Math') %>% 
    add_trace(y = ~Writing, name = 'Writing') %>% 
    layout(title = 'SAT Scores By Income Level', xaxis = xform, 
           yaxis = list(title = ''), barmode = 'stack')
  })
  
  output$table=renderDataTable(
    datatable(full,rownames = T) %>% 
      formatStyle(input$selected,  
                  background="skyblue", fontWeight='bold')
  )
  
  output$table2=renderDataTable(
    datatable(charter,rownames = T)
  
  )
  
  output$hist=renderGvis(
    gvisHistogram(scores[,input$selected,drop=F], options=list(legend='none',
                                                               width = "automatic",
                                                               height = "automatic"))
  )

  
  output$maxBox=renderInfoBox({
    max=max(scores[,input$selected])
    title=scores[scores[,input$selected]==max,'school_name']
    infoBox(title,max,icon = icon("hand-o-up"))
  })

  output$minBox <- renderInfoBox({
    min_value <- min(scores[,input$selected])
    min_state <-
      scores$school_name[scores[,input$selected]==min_value]
    infoBox(min_state, min_value, icon = icon("hand-o-down"))
  })
  output$avgBox <- renderInfoBox(
    infoBox(paste("AVG.", input$selected),
            round(mean(scores[,input$selected]), 1),
            icon = icon("calculator"), fill = TRUE))
}

