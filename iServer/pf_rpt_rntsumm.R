#
# handling economic indicators
#


lapply(1:nrow(property_show), function(i){
  
  curr_ppty_id <- property_show$id[i]
  curr_ppty_nm <- property_show$name[i]
  
  summ_data <- transdata_full %>% 
    dplyr::filter(property == curr_ppty_nm) %>% 
    dplyr::mutate(period = lubridate::year(transaction_date)) %>% 
    dplyr::mutate(category = ifelse(hyper_category == "Income","Income",ifelse(hyper_category == 'Investments',"Mortgage Principal","Expense"))) %>% 
    dplyr::mutate(type = ifelse(category == "Income", "Inflow", "Outflow")) %>% 
    dplyr::group_by(period, type, category) %>% 
    dplyr::summarise(value = sum(amount)) %>% 
    dplyr::filter(value != 0)
  
  output[[paste0("pf_rpt_rntsumm_", property_show$id[i])]] <- renderUI({
    
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
			      
			      fluidRow(
			        column(
			          3,
			          tags$div(
			            class = 'block_inner_frame',
			            tags$h3("Basic"),
			            tags$div(class = "pf_rpt_rntsumm_div", textAreaInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_bas_addr"), label = "address", value = property_show$address[i], width = entry_wid_l)),
			            tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_bas_priv"), label = "province", value = property_show$province[i], width = entry_wid_l)),
			            tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_bas_otype"), label = "ownership type", value = property_show$ownership[i], width = entry_wid_l)),
			            tags$div(class = "pf_rpt_rntsumm_div", textAreaInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_bas_assv"), label = paste0("property tax source"), value = curr_pptytax$source, width = entry_wid_l))
			          )
			        ),
			        column(
			          3,
			          tags$div(
			            class = 'block_inner_frame',
			            tags$h3("Property Value"),
			            tags$div(class = "pf_rpt_rntsumm_div", numericInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pv_aprv"), label = "appraisal value", value = property_show$appraisal_value[i], width = entry_wid_l)),
			            tags$div(class = "pf_rpt_rntsumm_div", numericInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pv_assv"), label = paste0("assessment value (", curr_pptytax$assessment_year,")"), value = curr_pptytax$assessment_value, width = entry_wid_l)),
			            tags$div(class = "pf_rpt_rntsumm_div", numericInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pv_apt"), label = paste0("annual property tax (", curr_pptytax$assessment_year,")"), value = curr_pptytax$annual_amount, width = entry_wid_l)),
			            tags$div(class = "pf_rpt_rntsumm_div", numericInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pv_aprv"), label = "annual property tax rate", value = curr_pptytax$tax_rate, width = entry_wid_l)),
			            tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pv_ptt"), label = paste0("property tax type"), value = curr_pptytax$tax_rate_type, width = entry_wid_l))
			          )
			        ),
			        column(
			          3,
			          tags$div(
			            class = 'block_inner_frame',
			            tags$h3("Purchase and Sale"),
			            tags$div(class = "pf_rpt_rntsumm_div", numericInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pns_purp"), label = "purchase price", value = property_show$purchase_price[i], width = entry_wid_l)),
			            tags$div(class = "pf_rpt_rntsumm_div", dateInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pns_purd"), label = "purchase date", value = property_show$purchase_date[i], width = entry_wid_l)),
			            tags$div(class = "pf_rpt_rntsumm_div", numericInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pns_sldp"), label = "sold price", value = property_show$sale_price[i], width = entry_wid_l)),
			            tags$div(class = "pf_rpt_rntsumm_div", dateInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_pns_sldd"), label = "sold date", value = property_show$sale_date[i], width = entry_wid_l))
			          )
			        ),
			        column(
			          3,
			          tags$div(
			            class = 'block_inner_frame',
			            tags$h3("Mortgage"),
			            tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_ir"), label = "interest rate", value = property_show$interest_rate[i], width = entry_wid_l)),
			            tags$div(class = "pf_rpt_rntsumm_div", textInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_lnamt"), label = "loan amount", value = property_show$loan_amount[i], width = entry_wid_l)),
			            tags$div(class = "pf_rpt_rntsumm_div", numericInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_lnpmt"), label = "loan payment", value = property_show$loan_payment[i], width = entry_wid_l)),
			            tags$div(class = "pf_rpt_rntsumm_div", dateInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_bedd"), label = "loan begin date", value = property_show$loan_beg[i], width = entry_wid_l)),
			            tags$div(class = "pf_rpt_rntsumm_div", dateInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_endd"), label = "loan end date", value = property_show$loan_end[i], width = entry_wid_l)),
			            tags$div(class = "pf_rpt_rntsumm_div", numericInput(paste0("pf_rpt_rntsumm_",curr_ppty_id,"_mtg_lnbal"), label = "loan balance", value = curr_amort$end_balance, width = entry_wid_l))
			          )
			        )
			      ),
			      fluidRow(
			        column(
			          6,
			          tags$div(
			            class = 'block_inner_frame',
			            tags$h3("Plot"),
			            plotOutput(paste0("pf_rpt_rntsumm_", curr_ppty_id, "_plot"))
			          )
			        ),
			        column(
			          6,
			          tags$div(
			            class = 'block_inner_frame',
			            tags$h3("Data"),
			            DT::dataTableOutput(paste0("pf_rpt_rntsumm_", curr_ppty_id, "_table"))
			          )
			        )
			      )
		      )
        )
      )
    )
  })
  
  output[[paste0("pf_rpt_rntsumm_", curr_ppty_id, "_table")]] <- DT::renderDataTable({
    summ_data
  })
  
  output[[paste0("pf_rpt_rntsumm_", curr_ppty_id, "_plot")]] <- renderPlot({
    SummaryPlot(summ_data, 90)
  })
  
})


