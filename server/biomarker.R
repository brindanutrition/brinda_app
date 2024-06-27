source("ui/styles/dropdown.R")
MediumDropDownDivStyle <- MediumDropDownDivStyle


observe_biomarker_population_selection <- function(input, output, session) {
    observe({
        req(input$pop)
        req(input$bloodDrawSameTime)

        if (input$bloodDrawSameTime == "yes") {
            # Reset
            updateTextInput(session, "differentBloodDrawFastingVarColumnName", value = "")
            updateTextInput(session, "differentBloodDrawTimeVarColumnName", value = "")
            output$differentBloodDrawFastingVarColumnName <- renderUI({
            })
            output$differentBloodDrawTimeVarColumnName <- renderUI({
            })

            # Render
            if (input$pop == "User-defined AGP and CRP cutoffs") {
                # Render
                output$manAgp <- renderUI({
                    textInput("manAgp", "Enter Manual AGP Reference Value")
                })
                output$manCrp <- renderUI({
                    textInput("manCrp", "Enter Manual CRP Reference Value")
                })
            } else {
                # Reset
                output$manAgp <- renderUI({
                })
                output$manCrp <- renderUI({
                })

                # Render
                output$sameBloodDrawFastingVarSelection <- renderUI({
                    div(
                        selectInput("fastingVar",
                            "What is the general fasting status of your data? *",
                            choices = c("fasted", "non-fasted", "unknown")
                        ),
                        style = MediumDropDownDivStyle
                    )
                })

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
            }
        } else if (input$bloodDrawSameTime == "no") {
            # Reset
            updateTextInput(session, "fastingVar", value = "unknown")
            updateTextInput(session, "timeVar", value = "unknown")
            output$sameBloodDrawFastingVarSelection <- renderUI({
            })
            output$sameBloodDrawTimeVarSelection <- renderUI({
            })
            output$manAgp <- renderUI({
            })
            output$manCrp <- renderUI({
            })

            # Render
            output$differentBloodDrawFastingVarColumnName <- renderUI({
                textInput("differentBloodDrawFastingVarColumnName", "What's the column name in the dataset indicating fasting status?")
            })
            output$differentBloodDrawTimeVarColumnName <- renderUI({
                textInput("differentBloodDrawTimeVarColumnName", "What's the column name in the dataset indicating time of blood draw?")
            })
        }
    })
}
