##
# Render for dashboard
output$pf_rpt_dashbd_p1 <- renderPlot({
  
  data1 <- transdata_full %>% 
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
  dashbd_p1_plot_daa <<- plot_data
  
  SummaryPlot(plot_data)

})

output$pf_rpt_dashbd_p2 <- renderPlot({

  data1 <- transdata_full %>% 
    dplyr::filter(lubridate::year(transaction_date) == lubridate::year(input$pf_ipt_par_begdt))
  plot_data1 <- DashbdPlotDataFormat(data1, 'All')
  
  data2 <- data1 %>% 
    dplyr::filter(property != "n/a")
  plot_data2 <- DashbdPlotDataFormat(data2, 'Rental')
  
  data3 <- data1 %>% 
    dplyr::filter(property == "n/a")
  plot_data3 <- DashbdPlotDataFormat(data3, 'All ex. Rental')
  
  plot_data <- rbind.data.frame(plot_data1, plot_data2, plot_data3)
  
  SummaryPlot(plot_data)
  
})

output$pf_rpt_dashbd_p3 <- renderPlot({
  
  data1 <- transdata_full %>% 
    dplyr::filter(lubridate::year(transaction_date) == lubridate::year(input$pf_ipt_par_begdt) - 1)
  plot_data1 <- DashbdPlotDataFormat(data1, 'All')
  
  data2 <- data1 %>% 
    dplyr::filter(property != "n/a")
  plot_data2 <- DashbdPlotDataFormat(data2, 'Rental')
  
  data3 <- data1 %>% 
    dplyr::filter(property == "n/a")
  plot_data3 <- DashbdPlotDataFormat(data3, 'All ex. Rental')
  
  plot_data <- rbind.data.frame(plot_data1, plot_data2, plot_data3)
  
  SummaryPlot(plot_data)
  
})

output$pf_rpt_dashbd_t1 <- DT::renderDataTable({
  dashbd_p1_plot_daa
})