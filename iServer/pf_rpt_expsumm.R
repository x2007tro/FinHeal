#
# handling economic indicators
#
withProgress(message = 'Retrieving transaction details ...', {
  prelim_data <- transdata_full %>% 
    dplyr::filter(property == "n/a") %>% 
    dplyr::mutate(category = ifelse(hyper_category == "Income","Income",hyper_category)) %>% 
    dplyr::mutate(type = ifelse(category == "Income", "Inflow", "Outflow")) 
})


output$pf_rpt_expsumm_p1 <- renderPlot({
  withProgress(message = 'Retrieving transaction details ...', {
    data_cm <- prelim_data %>% 
      dplyr::filter(transaction_date >= input$pf_ipt_par_begdt) %>% 
      dplyr::filter(transaction_date <= input$pf_ipt_par_enddt) %>% 
      dplyr::filter(category != 'Income') %>% 
      dplyr::group_by(category) %>% 
      dplyr::summarise(value = round(sum(amount))) %>% 
      dplyr::filter(value > 0) %>% 
      dplyr::arrange(value)
    
    ExpCatPlot(data_cm, c('pie','treemap')[2])
  })
  
})

output$pf_rpt_expsumm_p2 <- renderPlot({
  withProgress(message = 'Retrieving transaction details ...', {
  })
  data_cm <- prelim_data %>% 
    dplyr::filter(lubridate::year(transaction_date) == lubridate::year(input$pf_ipt_par_begdt)) %>% 
    dplyr::filter(category != 'Income') %>% 
    dplyr::group_by(category) %>% 
    dplyr::summarise(value = round(sum(amount))) %>% 
    dplyr::filter(value > 0) %>% 
    dplyr::arrange(value)
  
  ExpCatPlot(data_cm, c('pie','treemap')[2])
})

output$pf_rpt_expsumm_p3 <- renderPlot({
  withProgress(message = 'Retrieving transaction details ...', {
    data <- prelim_data %>%
      dplyr::filter(transaction_date >= (input$pf_ipt_par_begdt) - months(4)) %>%
      dplyr::mutate(period = format(transaction_date, '%Y-%m')) %>%
      dplyr::group_by(period, type, category) %>%
      dplyr::summarise(value = round(sum(amount))) %>%
      dplyr::filter(value >= 0)
    
    SummaryPlot(data, 0)
  })
  
})

output$pf_rpt_expsumm_p4 <- renderPlot({
  withProgress(message = 'Retrieving transaction details ...', {
    data <- prelim_data %>%
      dplyr::filter(transaction_date >= (input$pf_ipt_par_begdt) - years(4)) %>%
      dplyr::mutate(period = format(transaction_date, '%Y')) %>%
      dplyr::group_by(period, type, category) %>%
      dplyr::summarise(value = round(sum(amount))) %>%
      dplyr::filter(value >= 0)
    
    SummaryPlot(data, 0)
  })
  
})
