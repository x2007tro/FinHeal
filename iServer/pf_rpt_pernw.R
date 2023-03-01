# render net worth page

output$pr_rpt_pernw_netw <- renderUI({
  
  #browser()
  
  # calculate values
  tmp <- lapply(1:nrow(property_shownact()), function(i){
    
    curr_id <- property_shownact()$id[i]
    curr_nm <- property_shownact()$name[i]
    
    curr_pptytax <- pptytax_show() %>% 
      dplyr::filter(assessment_year == lubridate::year(input$pf_ipt_par_begdt) & property_id == curr_id) 
    curr_mv <- curr_pptytax$appraisal_value
    
    curr_amort <- amort_show() %>% 
      dplyr::filter(pmt_date == input$pf_ipt_par_begdt & property_id == curr_id) %>% 
      dplyr::mutate(
        principal = round(principal),
        interest = round(interest),
        end_balance = round(end_balance)
      )
    curr_mb <- curr_amort$end_balance
    
    net_val <- curr_mv - curr_mb
    return(net_val)
  
  }) 
  ppty_val <- sum(unlist(tmp))
  land_val <- sum(land_show()$appraisal_value)
  
  tmp <- pernw_show()
  assets <- tmp %>% dplyr::filter(category == 'asset')
  assets_val <- sum(assets$amount * assets$exchange_rate)
  
  tmp <- pernw_show()
  liabs <- tmp %>% dplyr::filter(category == 'liability')
  liabs_val <- sum(liabs$amount * liabs$exchange_rate)
  
  tot_assets <- ppty_val + land_val + assets_val
  tot_liabs <- liabs_val
  
  aadate <- max(pernw_show()$as_at_date, input$pf_ipt_par_enddt)
  
  tagList(
    fluidRow(
      column(
        12,
        tags$div(class = "pr_rpt_pernw_div", tags$h5(tags$b(paste0('OF $', scales::comma(tot_assets - tot_liabs, accuracy = 1))))),
        tags$div(
          class = 'pr_rpt_pernw_div',
          tags$table(
            tags$tr(width = "100%",
                    tags$td(width = "70%", div(style = "", paste0("total asset (as at ", aadate,")"))),
                    tags$td(width = "30%", textInput(paste0("pr_rpt_pernw_eqt_","ass"), label = NULL, value = scales::comma(tot_assets, accuracy = 1))))
          )
        ),
        tags$div(
          class = 'pr_rpt_pernw_div',
          tags$table(
            tags$tr(width = "100%",
                    tags$td(width = "70%", div(style = "", paste0("total liability (as at ", aadate,")"))),
                    tags$td(width = "30%", textInput(paste0("pr_rpt_pernw_eqt_","liab"), label = NULL, value = scales::comma(tot_liabs, accuracy = 1))))
          )
        ),
        # tags$div(
        #   class = 'pr_rpt_pernw_div',
        #   tags$table(
        #     tags$tr(width = "100%",
        #             tags$td(width = "70%", div(style = "", paste0("equity (as at ", aadate,")"))),
        #             tags$td(width = "30%", textInput(paste0("pr_rpt_pernw_eqt_","nwor"), label = NULL, value = scales::comma(tot_assets - tot_liabs, accuracy = 1))))
        #   )
        # ),
        tags$br()
      )
    )
  )
  
})

output$pr_rpt_pernw_ast_prop <- renderUI({
  tagList(
    #tags$h5("Properties"),
    
    lapply(1:nrow(property_shownact()), function(i){
      curr_id <- property_shownact()$id[i]
      curr_nm <- property_shownact()$name[i]
      
      curr_pptytax <- pptytax_show() %>% 
        dplyr::filter(assessment_year == lubridate::year(input$pf_ipt_par_begdt) & property_id == curr_id) 
      curr_mv <- curr_pptytax$appraisal_value
      
      curr_amort <- amort_show() %>% 
        dplyr::filter(pmt_date == input$pf_ipt_par_begdt & property_id == curr_id) %>% 
        dplyr::mutate(
          principal = round(principal),
          interest = round(interest),
          end_balance = round(end_balance)
        )
      curr_mb <- -curr_amort$end_balance
  
      fluidRow(
        column(
          12,
          tags$div(class = "pr_rpt_pernw_div", tags$h5(tags$b(curr_nm))),
          tags$div(
            class = 'pr_rpt_pernw_div',
            tags$table(
              tags$tr(width = "100%",
                      tags$td(width = "70%", div(style = "", paste0("market value (as at ", input$pf_ipt_par_enddt,")"))),
                      tags$td(width = "30%", textInput(paste0("pr_rpt_pernw_ast_prop_",curr_id,"_mv"), label = NULL, value = scales::comma(curr_mv, accuracy = 1))))
            )
          ),
          tags$div(
            class = 'pr_rpt_pernw_div',
            tags$table(
              tags$tr(width = "100%",
                      tags$td(width = "70%", div(style = "", paste0("mortgage balance (as at ", input$pf_ipt_par_enddt,")"))),
                      tags$td(width = "30%", textInput(paste0("pr_rpt_pernw_ast_prop_",curr_id,"_mb"), label = NULL, value = scales::comma(curr_mb, accuracy = 1))))
            )
          ),
          tags$br()
        )
      )
    })
    
  )
})

output$pr_rpt_pernw_ast_land <- renderUI({
  tagList(
    #tags$h5("Land"),
    
    lapply(1:nrow(land_show()), function(i){
      curr_id <- land_show()$id[i]
      curr_nm <- land_show()$name[i]
      curr_mv <- land_show()$appraisal_value[i]
      
      fluidRow(
        column(
          12,
          tags$div(class = "pr_rpt_pernw_div", tags$h5(tags$b(curr_nm))),
          tags$div(
            class = 'pr_rpt_pernw_div',
            tags$table(
              tags$tr(width = "100%",
                      tags$td(width = "70%", div(style = "", paste0("market value (as at ", Sys.Date(),")"))),
                      tags$td(width = "30%", textInput(paste0("pr_rpt_pernw_ast_land_",curr_id,"_mv"), label = NULL, value = scales::comma(curr_mv, accuracy = 1))))
            )
          ),
          tags$br()
        )
      )
    })
    
  )
})

output$pr_rpt_pernw_ast_oth <- renderUI({
  tmp <- pernw_show()
  assets <- tmp %>% dplyr::filter(category == 'asset')
  belong_to <- unique(assets$belong_to)
  
  tagList(
    #tags$h5("Other"),
    
    lapply(1:length(belong_to), function(i){
      curr_id <- belong_to[i]
      curr_assets <- assets %>% dplyr::filter(belong_to == curr_id)
      
      tagList(
        tags$div(class = "pr_rpt_pernw_div", tags$h5(tags$b(curr_id))),
        lapply(1:nrow(curr_assets), function(j){
          curr_curr_id <- curr_assets$id[j]
          curr_name <- curr_assets$name[j]
          curr_amt <- curr_assets$amount[j] * curr_assets$exchange_rate[j]
          curr_aadate <- curr_assets$as_at_date[j]
          
          fluidRow(
            column(
              12,
              tags$div(
                class = 'pr_rpt_pernw_div',
                tags$table(
                  tags$tr(width = "100%",
                          tags$td(width = "70%", div(style = "", paste0(curr_name, " (as at ", curr_aadate, ")"))),
                          tags$td(width = "30%", textInput(paste0("pr_rpt_pernw_ast_other_",curr_curr_id), label = NULL, value = scales::comma(curr_amt, accuracy = 1))))
                )
              )
            )
          )
        }),
        tags$br()
      )
      
      
    })
    
  )
})

output$pr_rpt_pernw_liab <- renderUI({
  tmp <- pernw_show()
  liabs <- tmp %>% dplyr::filter(category == 'liability')
  belong_to <- unique(liabs$belong_to)
  
  tagList(
    #tags$h5("Other"),
    
    lapply(1:length(belong_to), function(i){
      curr_id <- belong_to[i]
      curr_liabs <- liabs %>% dplyr::filter(belong_to == curr_id)
      
      tagList(
        tags$div(class = "pr_rpt_pernw_div", tags$h5(tags$b(curr_id))),
        lapply(1:nrow(curr_liabs), function(j){
          curr_curr_id <- curr_liabs$id[j]
          curr_name <- curr_liabs$name[j]
          curr_amt <- curr_liabs$amount[j] * curr_liabs$exchange_rate[j]
          curr_aadate <- curr_liabs$as_at_date[j]
          
          fluidRow(
            column(
              12,
              tags$div(
                class = 'pr_rpt_pernw_div',
                tags$table(
                  tags$tr(width = "100%",
                          tags$td(width = "70%", div(style = "", paste0(curr_name, " (as at ", curr_aadate, ")"))),
                          tags$td(width = "30%", textInput(paste0("pr_rpt_pernw_liab_",curr_curr_id), label = NULL, value = scales::comma(curr_amt, accuracy = 1))))
                )
              )
            )
          )
        }),
        tags$br()
      )
      
      
    })
    
  )
})

