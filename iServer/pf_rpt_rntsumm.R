#
# handling economic indicators
#


lapply(1:nrow(property_show), function(i){
  
  curr_ppty_id <- property_show$id[i]
  curr_ppty_nm <- property_show$name[i]
  
  withProgress(message = 'Retrieving transaction details ...', {
    summ_data <- transdata_full %>% 
      dplyr::filter(property == curr_ppty_nm) %>% 
      dplyr::mutate(period = lubridate::year(transaction_date)) %>% 
      dplyr::mutate(category = ifelse(hyper_category == "Income","Income",ifelse(category == 'Mortgage Principal',"Mortgage Principal","Expense"))) %>% 
      dplyr::mutate(type = ifelse(category == "Income", "Inflow", "Outflow")) %>% 
      dplyr::group_by(period, type, category) %>% 
      dplyr::summarise(value = sum(amount)) %>% 
      dplyr::filter(value > 0)
  })
  
  output[[paste0("pf_rpt_rntsumm_", property_show$id[i])]] <- renderUI({
    withProgress(message = 'Retrieving transaction details ...', {
      curr_pptytax <- pptytax_show %>% 
        dplyr::filter(assessment_year == lubridate::year(input$pf_ipt_par_begdt) & property_id == curr_ppty_id) %>% 
        dplyr::mutate(amount = round(annual_amount/12))
      
      curr_amort <- amort_show %>% 
        dplyr::filter(pmt_date == input$pf_ipt_par_begdt & property_id == curr_ppty_id) %>% 
        dplyr::mutate(
          principal = round(principal),
          interest = round(interest),
          end_balance = round(end_balance)
        )
      
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
                    3,
                    tags$div(
                      class = 'block_inner_frame',
                      tags$h4(class = 'block_title', "Basic"),
                      tags$div(class = "pf_rpt_rntsumm_div", textAreaInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_bas_addr"), label = "address", value = property_show$address[i], width = entry_wid_l)),
                      tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_bas_priv"), label = "province", value = property_show$province[i], width = entry_wid_l)),
                      tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_bas_area"), label = "area", value = property_show$area[i], width = entry_wid_l)),
                      tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_bas_otype"), label = "ownership type", value = property_show$ownership[i], width = entry_wid_l)),
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
                      tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pns_purp"), label = "purchase price", value =  scales::comma(property_show$purchase_price[i], accuracy = 1), width = entry_wid_l)),
                      tags$div(class = "pf_rpt_rntsumm_div", dateInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pns_purd"), label = "purchase date", value = property_show$purchase_date[i], width = entry_wid_l)),
                      tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pns_sldp"), label = "sold price", value =  scales::comma(property_show$sale_price[i], accuracy = 1), width = entry_wid_l)),
                      tags$div(class = "pf_rpt_rntsumm_div", dateInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pns_sldd"), label = "sold date", value = property_show$sale_date[i], width = entry_wid_l))
                    )
                  ),
                  column(
                    3,
                    tags$div(
                      class = 'block_inner_frame',
                      tags$h4(class = 'block_title', "Mortgage"),
                      tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_ir"), label = "interest rate", value = property_show$interest_rate[i], width = entry_wid_l)),
                      tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_lnamt"), label = "loan amount", value =  scales::comma(property_show$loan_amount[i], accuracy = 1), width = entry_wid_l)),
                      tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_lnpmt"), label = "loan payment", value =  scales::comma(property_show$loan_payment[i], accuracy = 1), width = entry_wid_l)),
                      tags$div(class = "pf_rpt_rntsumm_div", dateInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_bedd"), label = "loan begin date", value = property_show$loan_beg[i], width = entry_wid_l)),
                      tags$div(class = "pf_rpt_rntsumm_div", dateInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_endd"), label = "loan end date", value = property_show$loan_end[i], width = entry_wid_l)),
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
                    tags$h4(class = 'block_title', "Summary by Year"),
                    plotOutput(paste0("pf_rpt_rntsumm_", curr_ppty_id, "_plot"))
                  )
                ),
                column(
                  6,
                  tags$div(
                    class = 'block_inner_frame',
                    tags$h4(class = 'block_title', "Summary by Year"),
                    DT::dataTableOutput(paste0("pf_rpt_rntsumm_", curr_ppty_id, "_table"))
                  )
                )
              )
            )
          )
        )
      )
    })
  })
  
  output[[paste0("pf_rpt_rntsumm_", curr_ppty_id, "_table")]] <- DT::renderDataTable({
    withProgress(message = 'Retrieving transaction details ...', {
      DT::datatable(
        summ_data,
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
  
  output[[paste0("pf_rpt_rntsumm_", curr_ppty_id, "_plot")]] <- renderPlot({
    withProgress(message = 'Retrieving transaction details ...', {
      SummaryPlot(summ_data, 0)
    })
    
  })
  
})

