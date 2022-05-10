#
# handling economic indicators
#
prelim_data <- reactive({
  withProgress(message = 'Retrieving transaction details ...', {
    prelim_data <- transdata_full() %>% 
      dplyr::filter(property == "n/a") %>% 
      dplyr::mutate(category = ifelse(hyper_category == "Income","Income",hyper_category)) %>% 
      dplyr::mutate(type = ifelse(category == "Income", "Inflow", "Outflow")) 
  })
})

output$pf_rpt_expsumm_p1 <- renderPlot({
  withProgress(message = 'Retrieving transaction details ...', {
    data_cm <- prelim_data() %>% 
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
  data_cm <- prelim_data() %>% 
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
    data <- prelim_data() %>%
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
    data <- prelim_data() %>%
      dplyr::filter(transaction_date >= (input$pf_ipt_par_begdt) - years(4)) %>%
      dplyr::mutate(period = format(transaction_date, '%Y')) %>%
      dplyr::group_by(period, type, category) %>%
      dplyr::summarise(value = round(sum(amount))) %>%
      dplyr::filter(value >= 0)
    
    SummaryPlot(data, 0)
  })
  
})

output$pf_rpt_cc_summ <- renderUI({
  
  tagList(
    lapply(1:1, function(i){
      fluidRow(
        column(
          12,
          
          tags$div(
            class = 'block_inner_frame',
            fluidRow(
              column(
                12,
                tags$h4(class = 'block_title', 'By Year'),
                plotOutput(paste0('pf_rpt_cc_summ_byyear', i))
              )
            ),
            fluidRow(
              column(
                12,
                tags$h4(class = 'block_title', 'By Month'),
                plotOutput(paste0('pf_rpt_cc_summ_bymonth', i))
              )
            )
          
          )
        )
      )
    })
  )
  
})

observe({
  
  cc_trans_byyear <- creditcards_trans_summ()$by_year
  cc_trans_bymonth <- creditcards_trans_summ()$by_month
  
  lapply(1:1, function(i){
    
    tmp_byyear <- cc_trans_byyear %>% 
      dplyr::rename(type = transaction_year, value = amt)
    
    tmp_bymonth <- cc_trans_bymonth %>% 
      dplyr::filter(transaction_year == lubridate::year(Sys.Date())) %>% 
      # dplyr::mutate(type = paste0(transaction_year, "-", transaction_month)) %>% 
      dplyr::mutate(type = paste0(transaction_month)) %>% 
      dplyr::rename(value = amt)
    
    output[[paste0('pf_rpt_cc_summ_byyear', i)]] <- renderPlot({
      SimpleSummaryPlot(tmp_byyear, legend_pos = 'bottom')
    })
    
    output[[paste0('pf_rpt_cc_summ_bymonth', i)]] <- renderPlot({
      SimpleSummaryPlot(tmp_bymonth, legend_pos = 'bottom')
    })
    
  })
  
  
})