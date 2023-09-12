##
# Render for dashboard

## loal all dataset


# actual render
dashbd_p1_plot_data <- reactive({
  withProgress(message = 'Retrieving transaction details ...', {
    data1 <- transdata_full() %>% 
      dplyr::filter(transaction_date >= input$pf_ipt_par_begdt) %>% 
      dplyr::filter(transaction_date <= input$pf_ipt_par_enddt)
    plot_data1 <- DashbdPlotDataFormat(data1, 'All')
    
    data2 <- data1 %>% 
      dplyr::filter(property != "n/a")
    plot_data2 <- DashbdPlotDataFormat(data2, 'Property')
    
    data3 <- data1 %>% 
      dplyr::filter(property == "n/a")
    plot_data3 <- DashbdPlotDataFormat(data3, 'Non-Property')
    
    plot_data <- rbind.data.frame(plot_data1, plot_data2, plot_data3)
    plot_data
  })
})


output$pf_rpt_dashbd_p1 <- renderPlot({
    SummaryPlot(dashbd_p1_plot_data(), legend_pos = 'bottom')
})

output$pf_rpt_dashbd_p2 <- renderPlot({
  withProgress(message = 'Retrieving transaction details ...', {
    data1 <- transdata_full() %>% 
      dplyr::filter(lubridate::year(transaction_date) == lubridate::year(input$pf_ipt_par_begdt))
    plot_data1 <- DashbdPlotDataFormat(data1, 'All')
    
    data2 <- data1 %>% 
      dplyr::filter(property != "n/a")
    plot_data2 <- DashbdPlotDataFormat(data2, 'Property')
    
    data3 <- data1 %>% 
      dplyr::filter(property == "n/a")
    plot_data3 <- DashbdPlotDataFormat(data3, 'Non-Property')
    
    plot_data <- rbind.data.frame(plot_data1, plot_data2, plot_data3)
    
    SummaryPlot(plot_data, legend_pos = 'bottom')
  })

})

output$pf_rpt_dashbd_p3 <- renderPlot({
  withProgress(message = 'Retrieving transaction details ...', {
    data1 <- transdata_full() %>% 
      dplyr::filter(lubridate::year(transaction_date) == lubridate::year(input$pf_ipt_par_begdt) - 1)
    plot_data1 <- DashbdPlotDataFormat(data1, 'All')
    
    data2 <- data1 %>% 
      dplyr::filter(property != "n/a")
    plot_data2 <- DashbdPlotDataFormat(data2, 'Property')
    
    data3 <- data1 %>% 
      dplyr::filter(property == "n/a")
    plot_data3 <- DashbdPlotDataFormat(data3, 'Non-Property')
    
    plot_data <- rbind.data.frame(plot_data1, plot_data2, plot_data3)
    
    SummaryPlot(plot_data, legend_pos = 'bottom')
  })
  
})

output$pf_rpt_dashbd_p4l <- renderPlot({
  withProgress(message = 'Retrieving transaction details ...', {
    plot_data <- transdata_full() %>% 
      dplyr::filter(transaction_date >= input$pf_ipt_par_begdt) %>% 
      dplyr::filter(transaction_date <= input$pf_ipt_par_enddt) %>% 
      dplyr::filter(property != 'n/a') %>% 
      dplyr::mutate(period = paste0(property, " (", operation_type, ")")) %>% 
      dplyr::mutate(category = ifelse(hyper_category == "Income","Income",ifelse(category == 'Mortgage Principal',"Mortgage Principal","Expense"))) %>% 
      dplyr::mutate(type = ifelse(category == "Income", "Inflow", "Outflow")) %>% 
      dplyr::group_by(period, type, category) %>% 
      dplyr::summarise(value = sum(amount)) %>% 
      dplyr::filter(value > 0)
    
    SummaryPlot(plot_data, legend_pos = 'bottom')
  })
  
})

output$pf_rpt_dashbd_p4r <- renderPlot({
  withProgress(message = 'Retrieving transaction details ...', {
    plot_data <- transdata_full() %>% 
      dplyr::filter(lubridate::year(transaction_date) == lubridate::year(input$pf_ipt_par_begdt)) %>%
      dplyr::filter(property != 'n/a') %>% 
      dplyr::mutate(period = paste0(property, " (", operation_type, ")")) %>% 
      dplyr::mutate(category = ifelse(hyper_category == "Income","Income",ifelse(category == 'Mortgage Principal',"Mortgage Principal","Expense"))) %>% 
      dplyr::mutate(type = ifelse(category == "Income", "Inflow", "Outflow")) %>% 
      dplyr::group_by(period, type, category) %>% 
      dplyr::summarise(value = sum(amount)) %>% 
      dplyr::filter(value > 0)
    
    SummaryPlot(plot_data, legend_pos = 'bottom')
  })
  
})

