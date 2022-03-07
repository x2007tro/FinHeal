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
    plot_data2 <- DashbdPlotDataFormat(data2, 'Rental')
    
    data3 <- data1 %>% 
      dplyr::filter(property == "n/a")
    plot_data3 <- DashbdPlotDataFormat(data3, 'All ex. Rental')
    
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
    plot_data2 <- DashbdPlotDataFormat(data2, 'Rental')
    
    data3 <- data1 %>% 
      dplyr::filter(property == "n/a")
    plot_data3 <- DashbdPlotDataFormat(data3, 'All ex. Rental')
    
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
    plot_data2 <- DashbdPlotDataFormat(data2, 'Rental')
    
    data3 <- data1 %>% 
      dplyr::filter(property == "n/a")
    plot_data3 <- DashbdPlotDataFormat(data3, 'All ex. Rental')
    
    plot_data <- rbind.data.frame(plot_data1, plot_data2, plot_data3)
    
    SummaryPlot(plot_data, legend_pos = 'bottom')
  })
  
})

output$pf_rpt_dashbd_t1 <- DT::renderDataTable({
  withProgress(message = 'Retrieving transaction details ...', {
    DT::datatable(
      dashbd_p1_plot_data(),
      options = list(
        pageLength = 10,
        orderClasses = FALSE,
        searching = TRUE,
        paging = TRUE
      )
    ) %>% 
      DT::formatRound('value', digits = 0)
      
  })
  
})