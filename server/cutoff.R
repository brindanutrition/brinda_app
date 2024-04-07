get_suggested_cutoffs <- function(input, output){
  # --- output suggested cutoffs -----------------------------------------------
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
  output$ftSug <- renderText({
    req(input$ft)
    req(input$ftU)
    req(input$pop)
    if(as.character(input$ftU) == "\u03BCg/L" &
       as.character(input$pop) == "PSC"){
      cutoff="12 \u03BCg/L"
    }else if(as.character(input$ftU) == "\u03BCg/L" &
             as.character(input$pop) == "WRA"){
      cutoff="15 \u03BCg/L"
    }else if(as.character(input$ftU) == "ng/mL" &
             as.character(input$pop) == "PSC"){
      cutoff="12 ng/mL"
    }else if(as.character(input$ftU) == "ng/mL" &
             as.character(input$pop) == "WRA"){
      cutoff="15 ng/mL"
    }else if(as.character(input$ftU) == "\u03BCg/L" &
             !(as.character(input$pop) %in% c("WRA","PSC"))){
      cutoff="12-15 \u03BCg/L"
    }else if(as.character(input$ftU) == "ng/mL" &
             !(as.character(input$pop) %in% c("WRA","PSC"))){
      cutoff="12-15 ng/mL"
    }
    return(cutoff)
  })
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
  output$znSug <- renderText({
    req(input$zn)
    req(input$znU)
    req(input$pop)
    req(input$fastingVar)
    req(input$timeVar)
    if(as.character(input$znU) == "\u03BCmol/L" &
       as.character(input$pop) == "PSC" &
       as.character(input$timeVar) == "morning"){
      cutoff="9.9 \u03BCmol/L"
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "PSC" &
             as.character(input$timeVar) != "morning"){
      cutoff="8.7 \u03BCmol/L"
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "WRA" &
             as.character(input$fastingVar) == "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff="10.7 \u03BCmol/L"
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "WRA" &
             as.character(input$fastingVar) != "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff="10.1 \u03BCmol/L"
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             as.character(input$pop) == "WRA" &
             as.character(input$timeVar) != "morning"){
      cutoff="9.0 \u03BCmol/L"
    }else if(as.character(input$znU) == "\u03BCmol/L" &
             !(as.character(input$pop) %in% c("WRA","PSC"))){
      cutoff="9.3-11.3 \u03BCmol/L"
    }
    #########################################################
    else if(as.character(input$znU) == "\u03BCg/L" &
            as.character(input$pop) == "PSC" &
            as.character(input$timeVar) == "morning"){
      cutoff=paste(as.character(9.9*64.5),
                   "\u03BCg/L")
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "PSC" &
             as.character(input$timeVar) != "morning"){
      cutoff=paste(as.character(8.7*64.5),
                   "\u03BCg/L")
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "WRA" &
             as.character(input$fastingVar) == "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff=paste(as.character(10.7*64.5),
                   "\u03BCg/L")
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "WRA" &
             as.character(input$fastingVar) != "fasted" &
             as.character(input$timeVar) == "morning"){
      cutoff=paste(as.character(10.1*64.5),
                   "\u03BCg/L")
    }else if(as.character(input$znU) == "\u03BCg/L" &
             as.character(input$pop) == "WRA" &
             as.character(input$timeVar) != "morning"){
      cutoff=paste(as.character(9.0*64.5),
                   "\u03BCg/L")
    }else if(as.character(input$znU) == "\u03BCg/L" &
             !(as.character(input$pop) %in% c("WRA","PSC"))){
      cutoff="599.9-728.9 \u03BCg/L"
    }
    return(cutoff)
  })
}
