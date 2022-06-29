##
# render
output$pf_rpf_taxtble_ui <- renderUI({
  tags$div(
    class = 'block_outter_frame',
    fluidRow(
      column(
        12,
        tags$div(
          class = 'block_inner_frame',
          fluidRow(
            lapply(1:nrow(demogra_show()), function(i){
              column(
                12/nrow(demogra_show()),
                tags$div(
                  #class = 'block_inner_frame',
                  tags$h4(class = 'block_title', paste0(demogra_show()$name[i], " [", demogra_show()$residence[i],"]")),
                  uiOutput(paste0("pf_rpt_taxtble_demo_", demogra_show()$id[i]))
                )
              )
            })
          )
        )
      )
    ),
    fluidRow(
      lapply(1:nrow(property_shownact()), function(i){
        column(
          12/nrow(property_shownact()),
          tags$div(
            class = 'block_inner_frame',
            tags$h4(class = 'block_title', paste0(property_shownact()$name[i], " ( ",property_shownact()$operation_type[i]," )")),
            DT::dataTableOutput(paste0("pf_rpt_taxtble_ppty_", property_shownact()$id[i]))
          )
        )
      })
    )
  )
})

observe({
  lapply(1:nrow(demogra_show()), function(i){
    curr_pers_id <- demogra_show()$id[i]
    
    output[[paste0("pf_rpt_taxtble_demo_", curr_pers_id)]] <- renderUI({
      
      withProgress(message = 'Retrieving transaction details ...', {
        tax_year <- as.numeric(input$fp_rpt_taxtble_ty)
        curr_taxpar <- taxpar_show() %>% dplyr::filter(year == tax_year)
        curr_pers_salary <- curr_taxpar[[paste0(curr_pers_id, "_salary")]]
        curr_pers_bonus <- curr_taxpar[[paste0(curr_pers_id, "_bonus")]]
        curr_pers_other_income <- curr_taxpar[[paste0(curr_pers_id, "_other_income")]]
        
        full_tax <- PerIncomeTaxCalc(curr_pers_salary+curr_pers_bonus+curr_pers_other_income, tax_year, demogra_show()$residence[i], taxtbl_show(), taxpar_show())
        pspp <- PSPPCalc(curr_pers_salary+curr_pers_bonus+curr_pers_other_income, tax_year, curr_pers_id, taxpar_show())
        
        tagList(
          fluidRow(
            column(
              4,
              tags$div(
                #class = 'block_inner_frame',
                tags$h5(paste0("Income (", tax_year, ")")),
                tags$div(class = "pf_rpt_taxtble_div", numericInput(paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_sal"), label = "base salary", value = curr_pers_salary, min = 0, width = entry_wid_m)),
                tags$div(class = "pf_rpt_taxtble_div", numericInput(paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_bos"), label = "bonus", value = curr_pers_bonus, min = 0, width = entry_wid_m)),
                tags$div(class = "pf_rpt_taxtble_div", numericInput(paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_oin"), label = "other income", value = curr_pers_other_income, min = 0, width = entry_wid_m)),
                tags$div(class = "pf_rpt_taxtble_div", numericInput(paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_off"), label = "offset income", value = 0, min = 0, width = entry_wid_m)),
                tags$div(class = "pf_rpt_taxtble_div", numericInput(paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_ninc"), label = "net income", value = curr_pers_salary + curr_pers_bonus + curr_pers_other_income - (full_tax$cpp + full_tax$ei + full_tax$fed_tax + full_tax$prov_tax + pspp), min = 0, width = entry_wid_m)),
                tags$div(class = "pf_rpt_taxtble_div", numericInput(paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_nincm"), label = "net income (monthly)", value = round((curr_pers_salary + curr_pers_bonus + curr_pers_other_income - (full_tax$cpp + full_tax$ei + full_tax$fed_tax + full_tax$prov_tax + pspp))/12,0), min = 0, width = entry_wid_m))
              )
            ),
            column(
              4,
              tags$div(
                #class = 'block_inner_frame',
                tags$h5(paste0("Deduction (", tax_year, ")")),
                tags$div(class = "pf_rpt_taxtble_div", numericInput(paste0("pf_rpt_taxtble_",curr_pers_id,"_dec_cpp"), label = "CPP", value = full_tax$cpp, width = entry_wid_m)),
                tags$div(class = "pf_rpt_taxtble_div", numericInput(paste0("pf_rpt_taxtble_",curr_pers_id,"_dec_ei"), label = "EI", value = full_tax$ei, width = entry_wid_m)),
                tags$div(class = "pf_rpt_taxtble_div", numericInput(paste0("pf_rpt_taxtble_",curr_pers_id,"_dec_ftax"), label = "federal income tax", value = full_tax$fed_tax, width = entry_wid_m)),
                tags$div(class = "pf_rpt_taxtble_div", numericInput(paste0("pf_rpt_taxtble_",curr_pers_id,"_dec_ptax"), label = "prov income tax", value = full_tax$prov_tax, width = entry_wid_m)),
                tags$div(class = "pf_rpt_taxtble_div", numericInput(paste0("pf_rpt_taxtble_",curr_pers_id,"_dec_pspp"), label = "pension cont", value = pspp, width = entry_wid_m)),
                tags$div(class = "pf_rpt_taxtble_div", numericInput(paste0("pf_rpt_taxtble_",curr_pers_id,"_dec_tdec"), label = "total deduction", value = full_tax$cpp + full_tax$ei + full_tax$fed_tax + full_tax$prov_tax + pspp, width = entry_wid_m))
              )
            ),
            column(
              4,
              tags$div(
                #class = 'block_inner_frame',
                tags$h5(paste0("Other (", tax_year, ")")),
                tags$div(class = "pf_rpt_taxtble_div", numericInput(paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_rrsp"), label = "RRSP cont limit", value = curr_taxpar[[paste0(curr_pers_id, "_rrsp_max_contribution")]], width = entry_wid_m)),
                tags$div(class = "pf_rpt_taxtble_div", numericInput(paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_tfsa"), label = "TFSA cont limit", value = curr_taxpar[[paste0(curr_pers_id, "_tfsa_max_contribution")]], width = entry_wid_m))
              )
            )
          )
        )
      })
      
    })
    
    observeEvent({
      input[[paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_sal")]]
      input[[paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_bos")]]
      input[[paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_oin")]]
      input[[paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_off")]]
    }, {
      tax_year <- as.numeric(input$fp_rpt_taxtble_ty)
      curr_taxpar <- taxpar_show() %>% dplyr::filter(year == tax_year)
      curr_pers_salary <- input[[paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_sal")]]
      curr_pers_bonus <- input[[paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_bos")]]
      curr_pers_other_income <- input[[paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_oin")]]
      curr_pers_offset_income <- input[[paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_off")]]
      
      full_tax <- PerIncomeTaxCalc(curr_pers_salary+curr_pers_bonus+curr_pers_other_income-curr_pers_offset_income, tax_year, demogra_show()$residence[i], taxtbl_show(), taxpar_show())
      pspp <- PSPPCalc(curr_pers_salary+curr_pers_bonus+curr_pers_other_income-curr_pers_offset_income, tax_year, curr_pers_id, taxpar_show())
      
      updateNumericInput(session, paste0("pf_rpt_taxtble_",curr_pers_id,"_dec_cpp"), value = full_tax$cpp)
      updateNumericInput(session, paste0("pf_rpt_taxtble_",curr_pers_id,"_dec_ei"), value = full_tax$ei)
      updateNumericInput(session, paste0("pf_rpt_taxtble_",curr_pers_id,"_dec_ftax"), value = full_tax$fed_tax)
      updateNumericInput(session, paste0("pf_rpt_taxtble_",curr_pers_id,"_dec_ptax"), value = full_tax$prov_tax)
      updateNumericInput(session, paste0("pf_rpt_taxtble_",curr_pers_id,"_dec_pspp"), value = pspp)
      updateNumericInput(session, paste0("pf_rpt_taxtble_",curr_pers_id,"_dec_tdec"), value =  full_tax$cpp + full_tax$ei + full_tax$fed_tax + full_tax$prov_tax + pspp)
      updateNumericInput(session, paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_ninc"), value = curr_pers_salary + curr_pers_bonus + curr_pers_other_income - (full_tax$cpp + full_tax$ei + full_tax$fed_tax + full_tax$prov_tax + pspp))
      updateNumericInput(session, paste0("pf_rpt_taxtble_",curr_pers_id,"_inc_nincm"), value = round((curr_pers_salary + curr_pers_bonus + curr_pers_other_income - (full_tax$cpp + full_tax$ei + full_tax$fed_tax + full_tax$prov_tax + pspp))/12,0))
      
    })
    
  })
  
  lapply(1:nrow(property_shownact()), function(i){
    curr_ppty_id <- property_shownact()$id[i]
    curr_ppty_nm <- property_shownact()$name[i]
    curr_ppty_ot <- property_shownact()$operation_type[i]
    
    output[[paste0("pf_rpt_taxtble_ppty_", curr_ppty_id)]] <- DT::renderDataTable({
      withProgress(message = 'Retrieving transaction details ...', {
        ppty_data <- transdata_full() %>% 
          dplyr::filter(property == curr_ppty_nm & operation_type == curr_ppty_ot) %>% 
          dplyr::filter(lubridate::year(transaction_date) == as.numeric(input$fp_rpt_taxtble_ty)) %>% 
          dplyr::group_by(category) %>% 
          dplyr::summarise(value = sum(amount)) %>% 
          dplyr::mutate(tax_year = as.numeric(input$fp_rpt_taxtble_ty)) %>% 
          dplyr::select(tax_year, category, value) %>% 
          dplyr::arrange(desc(value))
        
        DT::datatable(
          ppty_data,
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
  })
})

