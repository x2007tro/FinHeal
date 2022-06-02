#
# handling economic indicators
#
output$pf_rpt_rntsumm_ui <- renderUI({
  do.call(tabsetPanel, c(id = "pf_rpt_rntsumm_tab", lapply(1:nrow(property_show()), function(i){
    
    tabPanel(
      paste(property_show()$name[i], "(",property_show()$operation_type[i],")","(",ifelse(property_show()$active[i]==1,'a','i'),")"),
      uiOutput(paste0("pf_rpt_rntsumm_", property_show()$id[i]))
    )
    
  })))
})

observe({
  lapply(1:nrow(property_show()), function(i){
    
    curr_ppty_id <- property_show()$id[i]
    curr_ppty_nm <- property_show()$name[i]
    curr_ppty_ot <- property_show()$operation_type[i]
    
    withProgress(message = 'Retrieving transaction details ...', {
      breakdown_data_cy <- transdata_full() %>% 
        dplyr::filter(property == curr_ppty_nm & operation_type == curr_ppty_ot) %>%
        dplyr::filter(lubridate::year(transaction_date) == lubridate::year(input$pf_ipt_par_begdt)) %>% 
        #dplyr::filter(transaction_date <= input$pf_ipt_par_enddt) %>% 
        dplyr::filter(hyper_category != 'Income') %>% 
        dplyr::group_by(category) %>% 
        dplyr::summarise(value = round(sum(amount))) %>% 
        dplyr::filter(value > 0) %>% 
        dplyr::arrange(value)
    })
    head(breakdown_data_cy)
    
    withProgress(message = 'Retrieving transaction details ...', {
      summ_data_ay <- transdata_full() %>% 
        dplyr::filter(property == curr_ppty_nm & operation_type == curr_ppty_ot) %>% 
        dplyr::mutate(period = lubridate::year(transaction_date)) %>% 
        dplyr::mutate(category = ifelse(hyper_category == "Income","Income",ifelse(category == 'Mortgage Principal',"Mortgage Principal","Expense"))) %>% 
        dplyr::mutate(type = ifelse(category == "Income", "Inflow", "Outflow")) %>% 
        dplyr::group_by(period, type, category) %>% 
        dplyr::summarise(value = sum(amount)) %>% 
        dplyr::filter(value > 0)
    })
    
    output[[paste0("pf_rpt_rntsumm_", property_show()$id[i])]] <- renderUI({
      withProgress(message = 'Retrieving transaction details ...', {
        curr_pptytax <- pptytax_show() %>% 
          dplyr::filter(assessment_year == lubridate::year(input$pf_ipt_par_begdt) & property_id == curr_ppty_id) %>% 
          dplyr::mutate(amount = round(annual_amount/12))
        
        curr_amort <- amort_show() %>% 
          dplyr::filter(pmt_date == input$pf_ipt_par_begdt & property_id == curr_ppty_id) %>% 
          dplyr::mutate(
            principal = round(principal),
            interest = round(interest),
            end_balance = round(end_balance)
          )
        
        net_income <- sum(summ_data_ay[summ_data_ay$category == 'Income','value']) - sum(summ_data_ay[summ_data_ay$category == 'Expense','value'])
        cap_gain <- ifelse(property_show()$sale_price[i] != 0, property_show()$sale_price[i] - property_show()$purchase_price[i], property_show()$appraisal_value[i] - property_show()$purchase_price[i])
        profit <- net_income + cap_gain
        
        tagList(
          fluidRow(
            column(
              12,
              tags$div(
                class = 'block_outter_frame',
                
                tags$div(
                  class = 'block_inner_frame',
                  fluidRow(
                    column(
                      12,
                      tags$h4(class = 'block_title', 'Summary'),
                      tags$div(class = 'block_summary', tags$h5(
                        paste0('PROFIT ($', scales::comma(profit, accuracy = 1),
                               ') consists of NET INCOME ($', scales::comma(net_income, accuracy = 1),
                               ') and CAPITAL GAIN ($', scales::comma(cap_gain, accuracy = 1), ")")
                      ))
                    )
                  )
                ),
                
                tags$div(
                  class = 'block_inner_frame',
                  fluidRow(
                    column(
                      3,
                      tags$div(
                        class = 'block_inner_frame',
                        tags$h4(class = 'block_title', "Basic"),
                        tags$div(class = "pf_rpt_rntsumm_div", textAreaInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_bas_addr"), label = "address", value = property_show()$address[i], width = entry_wid_l)),
                        tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_bas_priv"), label = "province", value = property_show()$province[i], width = entry_wid_l)),
                        tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_bas_area"), label = "area", value = property_show()$area[i], width = entry_wid_l)),
                        tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_bas_otype"), label = "ownership type", value = property_show()$ownership[i], width = entry_wid_l)),
                        tags$div(class = "pf_rpt_rntsumm_div", textAreaInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_bas_assv"), label = paste0("property tax source"), value = curr_pptytax$source, height = '100px', width = entry_wid_l))
                      )
                    ),
                    column(
                      3,
                      tags$div(
                        class = 'block_inner_frame',
                        tags$h4(class = 'block_title', paste0("Property Value (", curr_pptytax$assessment_year,")")),
                        tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pv_aprv"), label = paste0("appraisal value"), value = scales::comma(curr_pptytax$appraisal_value, accuracy = 1), width = entry_wid_l)),
                        tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pv_assv"), label = paste0("assessment value"), value =  scales::comma(curr_pptytax$assessment_value, accuracy = 1), width = entry_wid_l)),
                        tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pv_apt"), label = paste0("annual property tax amount"), value =  scales::comma(curr_pptytax$annual_amount, accuracy = 1), width = entry_wid_l)),
                        tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pv_aprv"), label = paste0("annual property tax rate"), value = scales::percent(curr_pptytax$tax_rate, accuracy = 0.00001), width = entry_wid_l))
                      )
                    ),
                    column(
                      3,
                      tags$div(
                        class = 'block_inner_frame',
                        tags$h4(class = 'block_title', "Purchase and Sale"),
                        tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pns_purp"), label = "purchase price", value =  scales::comma(property_show()$purchase_price[i], accuracy = 1), width = entry_wid_l)),
                        tags$div(class = "pf_rpt_rntsumm_div", dateInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pns_purd"), label = "purchase date", value = property_show()$purchase_date[i], width = entry_wid_l)),
                        tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pns_sldp"), label = "sold price", value =  scales::comma(property_show()$sale_price[i], accuracy = 1), width = entry_wid_l)),
                        tags$div(class = "pf_rpt_rntsumm_div", dateInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pns_sldd"), label = "sold date", value = property_show()$sale_date[i], width = entry_wid_l))
                      )
                    ),
                    column(
                      3,
                      tags$div(
                        class = 'block_inner_frame',
                        tags$h4(class = 'block_title', "Mortgage"),
                        tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_ir"), label = "interest rate", value = property_show()$interest_rate[i], width = entry_wid_l)),
                        tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_lnamt"), label = "loan amount", value =  scales::comma(property_show()$loan_amount[i], accuracy = 1), width = entry_wid_l)),
                        tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_lnpmt"), label = "loan payment", value =  scales::comma(property_show()$loan_payment[i], accuracy = 1), width = entry_wid_l)),
                        tags$div(class = "pf_rpt_rntsumm_div", dateInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_bedd"), label = "loan begin date", value = property_show()$loan_beg[i], width = entry_wid_l)),
                        tags$div(class = "pf_rpt_rntsumm_div", dateInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_endd"), label = "loan end date", value = property_show()$loan_end[i], width = entry_wid_l)),
                        tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_lnbal"), label = "loan balance", value =  scales::comma(curr_amort$end_balance, accuracy = 1), width = entry_wid_l))
                      )
                    )
                  )
                  
                ),
                fluidRow(
                  column(
                    6,
                    tags$div(
                      class = 'block_inner_frame',
                      tags$h4(class = 'block_title', "Current Year Breakdown"),
                      plotOutput(paste0("pf_rpt_rntsumm_", curr_ppty_id, "_plot1"))
                    )
                  ),
                  column(
                    6,
                    tags$div(
                      class = 'block_inner_frame',
                      tags$h4(class = 'block_title', "All Years Summary"),
                      plotOutput(paste0("pf_rpt_rntsumm_", curr_ppty_id, "_plot2"))
                    )
                  )
                )
              )
            )
          )
        )
      })
    })
    
    output[[paste0("pf_rpt_rntsumm_", curr_ppty_id, "_plot1")]] <- renderPlot({
      withProgress(message = 'Retrieving transaction details ...', {
        ExpCatPlot(breakdown_data_cy, c('pie','treemap')[2])
      })
      
    })
    
    output[[paste0("pf_rpt_rntsumm_", curr_ppty_id, "_plot2")]] <- renderPlot({
      withProgress(message = 'Retrieving transaction details ...', {
        SummaryPlot(summ_data_ay, 0)
      })
      
    })
    
  })
})




