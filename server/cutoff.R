get_suggested_cutoffs <- function(input, output){
  # --- output suggested cutoffs -----------------------------------------------

  # --- Retionol Binding Protein -----------------------------------------------
  output$rbpSug <- renderText({
    req(input$rbp)
    req(input$rbpU)
    req(input$pop)
    if(as.character(input$rbpU) == "\u03BCmol/L"){
      cutoff="0.7 \u03BCmol/L"
    }else if(as.character(input$rbpU) == "\u03BCg/dL"){
      cutoff=paste(
        as.character(0.7 * 0.03491),
        "\u03BCg/dL"
      )
    }
    return(cutoff)
  })

  # --- Retinol ----------------------------------------------------------------
  output$rtSug <- renderText({
    req(input$rt)
    req(input$rtU)
    req(input$pop)
    if(as.character(input$rtU) == "\u03BCmol/L"){
      cutoff="0.7 \u03BCmol/L"
    }else if(as.character(input$rtU) == "\u03BCg/dL"){
      cutoff=paste(
        as.character(0.7 * 0.03491),
        "\u03BCg/dL"
      )
    }
    return(cutoff)
  })

  # --- Ferritin ---------------------------------------------------------------
  output$ftSug <- renderText({
    req(input$ft)
    req(input$ftU)
    req(input$pop)
    if(as.character(input$ftU) == "\u03BCg/L" &
       as.character(input$pop) == "Preschool-age children"){
      cutoff="12 \u03BCg/L"
    }else if(as.character(input$ftU) == "\u03BCg/L" &
             as.character(input$pop) == "Women of Reproductive Age"){
      cutoff="15 \u03BCg/L"
    }else if(as.character(input$ftU) == "ng/mL" &
             as.character(input$pop) == "Preschool-age children"){
      cutoff="12 ng/mL"
    }else if(as.character(input$ftU) == "ng/mL" &
             as.character(input$pop) == "Women of Reproductive Age"){
      cutoff="15 ng/mL"
    }else if(as.character(input$ftU) == "\u03BCg/L" &
             !(as.character(input$pop) %in% c("Women of Reproductive Age","Preschool-age children"))){
      cutoff="12-15 \u03BCg/L"
    }else if(as.character(input$ftU) == "ng/mL" &
             !(as.character(input$pop) %in% c("Women of Reproductive Age","Preschool-age children"))){
      cutoff="12-15 ng/mL"
    }
    return(cutoff)
  })

  # --- Soluble Transferrin Receptor -------------------------------------------
  output$stfrSug <- renderText({
    req(input$stfr)
    req(input$stfrU)
    req(input$pop)
    if(as.character(input$stfrU) == "mg/L"){
      cutoff="8.3 mg/L"
    }else if(as.character(input$stfrU) == "g/L"){
      cutoff="8.3 g/L"
    }
    return(cutoff)
  })

  "\u03BCmol/L"

# --- Zinc ---------------------------------------------------------------------
  output$znSug <- renderText({
    req(input$zn)
    req(input$znU)
    req(input$pop)
    req(input$fastingVar)
    req(input$timeVar)
    if(as.character(input$znU) == "\u03BCmol/L" &
       as.character(input$pop) == "Preschool-age children" &
       as.character(input$timeVar) == "morning"){
      cutoff="9.9 \u03BCmol/L"
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "Preschool-age children" &
             as.character(input$timeVar) != "morning"){
      cutoff="8.7 \u03BCmol/L"
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "Women of Reproductive Age" &
             as.character(input$fastingVar) == "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff="10.7 \u03BCmol/L"
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "Women of Reproductive Age" &
             as.character(input$fastingVar) != "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff="10.1 \u03BCmol/L"
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "Women of Reproductive Age" &
             as.character(input$timeVar) != "morning"){
      cutoff="9.0 \u03BCmol/L"
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             !(as.character(input$pop) %in% c("Women of Reproductive Age","Preschool-age children"))){
      cutoff="9.3-11.3 \u03BCmol/L"
    }
    #########################################################
    else if(as.character(input$znU) == "\u03BCg/L" &
            as.character(input$pop) == "Preschool-age children" &
            as.character(input$timeVar) == "morning"){
      cutoff=paste(as.character(9.9*64.5),
                   "\u03BCg/L")
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "Preschool-age children" &
             as.character(input$timeVar) != "morning"){
      cutoff=paste(as.character(8.7*64.5),
                   "\u03BCg/L")
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "Women of Reproductive Age" &
             as.character(input$fastingVar) == "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff=paste(as.character(10.7*64.5),
                   "\u03BCg/L")
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "Women of Reproductive Age" &
             as.character(input$fastingVar) != "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff=paste(as.character(10.1*64.5),
                   "\u03BCg/L")
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "Women of Reproductive Age" &
             as.character(input$timeVar) != "morning"){
      cutoff=paste(as.character(9.0*64.5),
                   "\u03BCg/L")
    }else if(as.character(input$znU) == "\u03BCg/L" &
             !(as.character(input$pop) %in% c("Women of Reproductive Age","Preschool-age children"))){
      cutoff="599.9-728.9 \u03BCg/L"
    }
    return(cutoff)
  })
}


apply_suggested_cutoffs <- function(input, output, session){

  # --- Retionol Binding Protein -----------------------------------------------
  rbpSugValue <- reactive({
    req(input$rbp)
    req(input$rbpU)
    req(input$pop)
    if(as.character(input$rbpU) == "\u03BCmol/L"){
      cutoff=0.7
    }else if(as.character(input$rbpU) == "\u03BCg/dL"){
      cutoff=0.7 * 0.03491
    } else {
      cutoff=0
    }
    return(cutoff)
  })
  observeEvent(input$applyRBPCutOff, {
      suggestedrbpCutoff <- rbpSugValue()
      if (suggestedrbpCutoff != 0) {
          updateTextInput(session, "rbpC", value = suggestedrbpCutoff)
      }
  })

  # --- Retinol ----------------------------------------------------------------
  rtSugValue <- reactive({
    req(input$rt)
    req(input$rtU)
    req(input$pop)
    if(as.character(input$rtU) == "\u03BCmol/L"){
      cutoff=0.7
    }else if(as.character(input$rtU) == "\u03BCg/dL"){
      cutoff=0.7 * 0.03491
    }
    return(cutoff)
  })
  observeEvent(input$applyRTCutOff, {
      suggestedrtCutoff <- rtSugValue()
      if (suggestedrtCutoff != 0) {
          updateTextInput(session, "rtC", value = suggestedrtCutoff)
      }
  })

  # --- Ferritin ---------------------------------------------------------------
  ftSugValue <- reactive({
    req(input$ft)
    req(input$ftU)
    req(input$pop)
    if(as.character(input$ftU) == "\u03BCg/L" &
       as.character(input$pop) == "Preschool-age children"){
      cutoff=12
    }else if(as.character(input$ftU) == "\u03BCg/L" &
             as.character(input$pop) == "Women of Reproductive Age"){
      cutoff=15
    }else if(as.character(input$ftU) == "ng/mL" &
             as.character(input$pop) == "Preschool-age children"){
      cutoff=12
    }else if(as.character(input$ftU) == "ng/mL" &
             as.character(input$pop) == "Women of Reproductive Age"){
      cutoff=15
    }else if(as.character(input$ftU) == "\u03BCg/L" &
             !(as.character(input$pop) %in% c("Women of Reproductive Age","Preschool-age children"))){
      cutoff=15  # TODO: fix this, it should be 12-15
    }else if(as.character(input$ftU) == "ng/mL" &
             !(as.character(input$pop) %in% c("Women of Reproductive Age","Preschool-age children"))){
      cutoff=15 # TODO: Fix this, it should be 12-15
    }
    return(cutoff)
  })
  observeEvent(input$applyFTCutOff, {
      suggestedftCutoff <- ftSugValue()
      if (suggestedftCutoff != 0) {
          updateTextInput(session, "ftC", value = suggestedftCutoff)
      }
  })

  # --- Soluble Transferrin Receptor -------------------------------------------
  stfrSugValue <- reactive({
    req(input$stfr)
    req(input$stfrU)
    req(input$pop)
    if(as.character(input$stfrU) == "mg/L"){
      cutoff=8.3
    }else if(as.character(input$stfrU) == "g/L"){
      cutoff=8.3
    }
    return(cutoff)
  })
  observeEvent(input$applySTFRCutOff, {
      suggestedstfrCutoff <- stfrSugValue()
      if (suggestedstfrCutoff != 0) {
          updateTextInput(session, "stfrC", value = suggestedstfrCutoff)
      }
  })

  # --- Zinc ---------------------------------------------------------------------
  znSugValue <- reactive({
    req(input$zn)
    req(input$znU)
    req(input$pop)
    req(input$fastingVar)
    req(input$timeVar)
    if(as.character(input$znU) == "\u03BCmol/L" &
       as.character(input$pop) == "Preschool-age children" &
       as.character(input$timeVar) == "morning"){
      cutoff=9.9
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "Preschool-age children" &
             as.character(input$timeVar) != "morning"){
      cutoff=8.7
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "Women of Reproductive Age" &
             as.character(input$fastingVar) == "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff=10.7
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "Women of Reproductive Age" &
             as.character(input$fastingVar) != "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff=10.1
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "Women of Reproductive Age" &
             as.character(input$timeVar) != "morning"){
      cutoff=9.0
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             !(as.character(input$pop) %in% c("Women of Reproductive Age","Preschool-age children"))){
      cutoff=11.3 # TODO: Fix this, it should be 9.3-11.3
    }
    #########################################################
    else if(as.character(input$znU) == "\u03BCg/L" &
            as.character(input$pop) == "Preschool-age children" &
            as.character(input$timeVar) == "morning"){
      cutoff=9.9*64.5
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "Preschool-age children" &
             as.character(input$timeVar) != "morning"){
      cutoff=8.7*64.5
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "Women of Reproductive Age" &
             as.character(input$fastingVar) == "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff=10.7*64.5
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "Women of Reproductive Age" &
             as.character(input$fastingVar) != "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff=10.1*64.5
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "Women of Reproductive Age" &
             as.character(input$timeVar) != "morning"){
      cutoff=9.0*64.5
    }else if(as.character(input$znU) == "\u03BCg/L" &
             !(as.character(input$pop) %in% c("Women of Reproductive Age","Preschool-age children"))){
      cutoff=728.9 # TODO: Fix this, it should be 599.9-728.9
    }
    return(cutoff)
  })
  observeEvent(input$applyZNCutOff, {
      suggestedznCutoff <- znSugValue()
      if (suggestedznCutoff != 0) {
          updateTextInput(session, "znC", value = suggestedznCutoff)
      }
  })
}


calculate_dynamic_zinc_cutoff <- function(input, output, session, imported){
    observeEvent(input$clickNextOnBiomarkerPage, {
    df <- as.data.frame(imported$data())
    print(df)
    input_population <- find_population_code_name(input$pop)

    print("\n\n======================>")
    print(input_population)
    print(input$diff_bdt_column)
    print(input$diff_bdt_morning_val)
    print(input$diff_bdt_afternoon_val)
    print(input$diff_bdt_evening_val)
    print(input$diff_bdt_unknown_val)
    print(input$diff_fs_column)
    print(input$diff_fs_fasted_val)
    print(input$diff_fs_nonfasted_val)
    print(input$diff_fs_unknown_val)
    print("======================>\n\n")

    calculate_zn <- function(row) {
      blood_draw_time <- ""
      fasting_status <- ""

      # Derive blood draw time
      if (input$same_bdt == "yes") {
        if (input$timeVar == "morning") {
          blood_draw_time <- "morning"
        } else {
          blood_draw_time <- "non_morning"
        }
      } else {
        if (input$diff_bdt_column != "" && !is.null(input$diff_bdt_column)) {
          if (row[input$diff_bdt_column] == input$diff_bdt_morning_val) {
            blood_draw_time <- "morning"
          } else {
            blood_draw_time <- "not_morning"
          }
        } else {
          blood_draw_time <- "non_morning"
        }
      }

      # Derive fasting status
      if (input$same_fs == "yes") {
        if (input$fastingVar == "fasted") {
          fasting_status <- "fasted"
        } else {
          fasting_status <- "non_fasted"
        }
      } else {
        if (input$diff_fs_column != "" && !is.null(input$diff_fs_column)) {
          if (row[input$diff_fs_column] == input$diff_fs_fasted_val) {
            fasting_status <- "fasted"
          } else {
            fasting_status <- "non_fasted"
          }
        } else {
          fasting_status <- "non_fasted"
        }
      }
    }



    result <- t(apply(df, 1, function(row) calculate_zn(row)))
    print(result)
  })
}