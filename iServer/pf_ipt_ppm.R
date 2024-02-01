#
# handling economic indicators
#
output$pf_ipt_ppm_ui <- renderUI({
  do.call(tabsetPanel, c(id = "pf_ipt_ppm_tab", lapply(1:nrow(property_shownact()), function(i){
    
    curr_ppty_id <- property_shownact()$id[i]
    curr_ppty_nm <- property_shownact()$name[i]
    
    tabPanel(
      paste(property_shownact()$name[i], "(",property_shownact()$operation_type[i],")"),
      uiOutput(paste0("pf_ipt_ppm_", property_shownact()$id[i]))
    )
    
  })))
})

observe({
  lapply(1:nrow(property_shownact()), function(i){
    
    curr_ppty_id <- property_shownact()$id[i]
    curr_ppty_nm <- property_shownact()$name[i]
    
    output[[paste0("pf_ipt_ppm_", property_shownact()$id[i])]] <- renderUI({
      
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
      
      tagList(
        fluidRow(
          column(
            12,
            tags$div(
              class = 'block_outter_frame',
              
              fluidRow(
                column(
                  12,
                  
                  tags$div(
                    class = 'block_inner_frame',
                    fluidRow(
                      column(
                        3,
                        
                        fluidRow(
                          column(
                            12,
                            tags$div(
                              class = 'block_inner_frame',
                              tags$h4(class = 'block_title', "Income"),
                              tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_inc_rent"), label = "rent", value = 0, width = entry_wid_l)),
                              tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_inc_laundry"), label = "laundry", value = 0, width = entry_wid_l)),
                              tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_inc_storage"), label = "storage", value = 0, width = entry_wid_l)),
                              tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_inc_other"), label = "other", value = 0, width = entry_wid_l))
                            )
                            
                          )
                        ),
                        fluidRow(
                          column(
                            12,
                            tags$h4(class = 'block_title', "Mortgage"),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_mort_prl"), label = "principal", value = curr_amort$principal, width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_mort_int"), label = "interest", value = curr_amort$interest,  width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_mort_bal"), label = "balance", value = curr_amort$end_balance,  width = entry_wid_l))
                          )
                        )
                        
                      ),
                      
                      column(
                        3,
                        
                        fluidRow(
                          column(
                            12,
                            tags$h4(class = 'block_title', "Property Management"),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_ppm_mgmtfee"), label = "management fee", value = property_shownact()$exp_pm_fee_perc[i], width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_ppm_lawncare"), label = "lawn care", value = property_shownact()$exp_lawn_care[i], width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_ppm_snowrem"), label = "snow removal", value = property_shownact()$exp_snow_removal[i], width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_ppm_phonebill"), label = "phone bill", value = property_shownact()$exp_phone_bill[i], width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_ppm_cleaning"), label = "cleaning", value = property_shownact()$exp_cleaning[i], width = entry_wid_l))
                          )
                        )
                      ),
                      
                      column(
                        3,
                        
                        fluidRow(
                          column(
                            12,
                            tags$h4(class = 'block_title', "Expense"),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_exp_pptytax"), label = "property tax", value = curr_pptytax$amount*0, width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_exp_util"), label = "utility", value = 0, width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_exp_insu"), label = "insurance", value = property_shownact()$exp_insurance[i], width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_exp_watnsew"), label = "water & sewer", value = 0, width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_exp_main"), label = "maintenance", value = 0, width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_exp_strata"), label = "strata", value = property_shownact()$exp_strata[i], width = entry_wid_l))
                          )
                        )
                      ),
                      
                      column(
                        3,
                        fluidRow(
                          column(
                            12,
                            tags$h4(class = 'block_title', "Additional"),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_addi_1"), label = "banking fee", value = 0, width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_addi_2"), label = "rent reimburse", value = 0, width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_addi_3"), label = "labor", value = 0, width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_addi_4"), label = "equipment", value = 0, width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_addi_5"), label = "upgrades", value = 0, width = entry_wid_l)),
                            tags$div(class = "pf_ipt_ppm_div", numericInput(paste0("fp_ipt_ppm_",curr_ppty_id,"_addi_6"), label = "reserved", value = 0, width = entry_wid_l))
                          )
                        )
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  tags$div(
                    class = 'block_inner_frame',
                    actionButton(class = "btn-success", paste0("fp_ipt_ppm_",curr_ppty_id,"_enter"), "Enter into DB"),
                    actionButton(class = "btn-primary", paste0("fp_ipt_ppm_",curr_ppty_id,"_clear"), "Clear from DB")
                  )
                  
                )
              )
            )
          )
        )
      )
      
      
    })
    
    # Update ppm fee
    observeEvent(input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_inc_rent")]], {
      
      updateNumericInput(session, paste0("fp_ipt_ppm_",curr_ppty_id,"_ppm_mgmtfee"), value = input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_inc_rent")]] * property_shownact()$exp_pm_fee_perc[i] * (1+property_shownact()$gst[i]))
      
    })
    
    observeEvent(input[[paste0("fp_ipt_ppm_", curr_ppty_id, "_clear")]], {
      showNotification("Transactions are being removed from  DB...", type = 'error')
      
      sql_str <- paste0("DELETE FROM `* Input 02 : Transactions *` WHERE property = '", curr_ppty_nm, 
                        "' AND transaction_date >= '", input$pf_ipt_par_begdt, 
                        "' AND transaction_date <= '", input$pf_ipt_par_enddt,"'")
      GetQueryResFromSS(db_obj, sql_str)
      
    })
    
    observeEvent(input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_enter")]], {
      
      # Process ppm expense
      showNotification("Transactions are being entered to DB...", type = 'message')
      
      dates <- input$pf_ipt_par_begdt
      desc <- c('rental income','Laundry income','Storage income','other income',
                'mortgage principal','mortgage interest',
                'management fee','lawn care','snow removal','phone bill','cleaning',
                'property tax','utility','insurance','water & sewer','maintenance','strata',
                'banking fee', 'rent reimburse', 'labor', 'equipment', 'upgrade', 'additional')
      amnt <- c( input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_inc_rent")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_inc_laundry")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_inc_storage")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_inc_other")]],
                 input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_mort_prl")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_mort_int")]],
                 input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_ppm_mgmtfee")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_ppm_lawncare")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_ppm_snowrem")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_ppm_phonebill")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_ppm_cleaning")]], 
                 input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_exp_pptytax")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_exp_util")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_exp_insu")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_exp_watnsew")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_exp_main")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_exp_strata")]],
                 input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_addi_1")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_addi_2")]],  input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_addi_3")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_addi_4")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_addi_5")]], input[[paste0("fp_ipt_ppm_",curr_ppty_id,"_addi_6")]])
      ppty <- curr_ppty_nm
      ctgr <- c('Rental Income','Laundry Income','Storage Income','Other Rental Income',
                'Mortgage Principal','Mortgage Interest',
                'Property Management','Lawn Care','Snow Removal','Business Phone Bill','Cleaning',
                'Property Tax','Utilities','Business Insurance','Water & Sewer', 'Building Maintenance', 'Strata',
                'Business Banking Fee','Rent Reimburse','Labor','Building Equipment','Building Upgrade','PM Reserved')
      hyctgr <- sapply(1:length(ctgr), function(x){ transcat_show()$hyper_category[transcat_show()$name == ctgr[x]] })
      recu <- TRUE
      cmts <- ''
      
      #print((desc))
      #print((amnt))
      #print((ctgr))
      #print((hyctgr))
      
      exp_df <- data.frame(
        id = 0,
        transaction_date = dates,
        description = desc,
        amount = amnt,
        category = ctgr,
        hyper_category = hyctgr,
        property = ppty,
        account = 'n/a',
        best_alt_account = 'n/a',
        operation_type = property_shownact()$operation_type[i],
        costco = 0,
        essential = 0,
        refundable = 0,
        receipt_kept = 0,
        recurring = as.numeric(recu),
        active = 1,
        order = 999,
        show = 1,
        entry_datetime = Sys.time(),
        comments = cmts,
        stringsAsFactors = FALSE
      )
      
      if(nrow(exp_df) > 0) WriteDataToSS(db_obj, exp_df, '* Input 02 : Transactions *', apd = TRUE)
      
    })
    
  })
})




