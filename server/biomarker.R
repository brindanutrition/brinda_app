source("ui/styles/dropdown.R")
MediumDropDownDivStyle <- MediumDropDownDivStyle


observe_biomarker_population_selection <- function(input, output, session) {
    observe({
        req(input$pop)
        req(input$bloodDrawSameTime)


        if (input$pop == "User-defined AGP and CRP cutoffs") {
            output$manAgp <- renderUI({
                textInput("manAgp", "Enter Manual AGP Reference Value")
            })
            output$manCrp <- renderUI({
                textInput("manCrp", "Enter Manual CRP Reference Value")
            })
        } else {
            output$manAgp <- renderUI({})
            output$manCrp <- renderUI({})
        }

        # FASTING STATUS
        if (input$fastingSameStatus == "yes") {
            # updateTextInput(session, "differentBloodDrawFastingVarColumnName", value = "")
            output$differentBloodDrawFastingVarColumnName <- renderUI({})

            output$sameBloodDrawFastingVarSelection <- renderUI({
                div(
                    selectInput("fastingVar",
                        "What is the general fasting status of your data? *",
                        choices = c("fasted", "non-fasted", "unknown")
                    ),
                    style = MediumDropDownDivStyle
                )
            })

            output$differentBloodDrawFastingVarFastedValue <- renderUI({})
            output$differentBloodDrawFastingVarNonFastedValue <- renderUI({})
            output$differentBloodDrawFastingVarUnknownValue <- renderUI({})
        } else if (input$fastingSameStatus == "no") {
            # updateTextInput(session, "fastingVar", value = "unknown")
            output$sameBloodDrawFastingVarSelection <- renderUI({})

            output$differentBloodDrawFastingVarColumnName <- renderUI({
                textInput("differentBloodDrawFastingVarColumnName", "What's the column name in the dataset indicating fasting status?")
            })

            output$differentBloodDrawFastingVarFastedValue <- renderUI({
                textInput("differentBloodDrawFastingVarFastedValue", "\"Fasted\" Value", value = 1)
            })
            output$differentBloodDrawFastingVarNonFastedValue <- renderUI({
                textInput("differentBloodDrawFastingVarNonFastedValue", "\"Non-Fasted\" Value", value = 0)
            })
            output$differentBloodDrawFastingVarUnknownValue <- renderUI({
                textInput("differentBloodDrawFastingVarUnknownValue", "\"Evening\" Value", value = 0)
            })
        }

        # BLOOD DRAW TIME
        if (input$bloodDrawSameTime == "yes") {
            # updateTextInput(session, "differentBloodDrawTimeVarColumnName", value = "")
            output$differentBloodDrawTimeVarColumnName <- renderUI({})
            output$sameBloodDrawTimeVarSelection <- renderUI({
                div(
                    selectInput("timeVar",
                        "What is the general time of blood draw for your data? *",
                        choices = c(
                            "morning",
                            "afternoon",
                            "evening",
                            "unknown"
                        )
                    ),
                    style = MediumDropDownDivStyle
                )
            })
            output$differentBloodDrawTimeVarMorningValue <- renderUI({})
            output$differentBloodDrawTimeVarAfternoonValue <- renderUI({})
            output$differentBloodDrawTimeVarEveningValue <- renderUI({})
            output$differentBloodDrawTimeVarUnknownValue <- renderUI({})
        } else if (input$bloodDrawSameTime == "no") {
            # updateTextInput(session, "timeVar", value = "unknown")
            output$sameBloodDrawTimeVarSelection <- renderUI({})
            output$differentBloodDrawTimeVarColumnName <- renderUI({
                textInput("differentBloodDrawTimeVarColumnName", "What's the column name in the dataset indicating time of blood draw?")
            })
            output$differentBloodDrawTimeVarMorningValue <- renderUI({
                textInput("differentBloodDrawTimeVarMorningValue", "\"Morning\" Value", value = 1)
            })
            output$differentBloodDrawTimeVarAfternoonValue <- renderUI({
                textInput("differentBloodDrawTimeVarAfternoonValue", "\"Afternoon\" Value", value = 0)
            })
            output$differentBloodDrawTimeVarEveningValue <- renderUI({
                textInput("differentBloodDrawTimeVarEveningValue", "\"Evening\" Value", value = 0)
            })
            output$differentBloodDrawTimeVarUnknownValue <- renderUI({
                textInput("differentBloodDrawTimeVarUnknownValue", "\"Unknown\" Value", value = 0)
            })
        }
    })
}
