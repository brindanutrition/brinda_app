source("ui/styles/dropdown.R")
MediumDropDownDivStyle <- MediumDropDownDivStyle


observe_agp_crp_selection <- function(input, output, session) {
    observe({
        req(input$pop)
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
    })
}



#######################
# UI Rendering Functions
#######################
observe_blood_draw_time_selection <- function(input, output, session) {
    observe({
        req(input$pop)
        req(input$same_bdt)

        if (input$same_bdt == "yes") {
            # Display the static blood draw time selection dropdown
            output$same_bdt_ui_selector <- renderUI({
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

            # Set custom column name and value selection UI to empty
            output$diff_bdt_column_ui_selector <- renderUI({})
            output$diff_bdt_morning_val_ui_selector <- renderUI({})
            output$diff_bdt_afternoon_val_ui_selector <- renderUI({})
            output$diff_bdt_evening_val_ui_selector <- renderUI({})
            output$diff_bdt_unknown_val_ui_selector <- renderUI({})
        } else if (input$same_bdt == "no") {
            # Set the static blood draw time selection dropdown to empty
            output$same_bdt_ui_selector <- renderUI({})

            # Display the custom column name and value selection UI
            output$diff_bdt_column_ui_selector <- renderUI({
                selectInput("diff_bdt_column", "What's the column name in the dataset indicating time of blood draw?", choices = "")
            })
        }
    })
}

observe_blood_draw_time_vals_selection <- function(input, output, session) {
    observe({
        req(input$diff_bdt_column)
        if (input$same_bdt == "no" && input$diff_bdt_column != "") {
            output$diff_bdt_morning_val_ui_selector <- renderUI({
                selectInput("diff_bdt_morning_val", "\"Morning\" Value", choices = "")
            })
            output$diff_bdt_afternoon_val_ui_selector <- renderUI({
                selectInput("diff_bdt_afternoon_val", "\"Afternoon\" Value", choices = "")
            })
            output$diff_bdt_evening_val_ui_selector <- renderUI({
                selectInput("diff_bdt_evening_val", "\"Evening\" Value", choices = "")
            })
            output$diff_bdt_unknown_val_ui_selector <- renderUI({
                selectInput("diff_bdt_unknown_val", "\"Unknown\" Value", choices = "")
            })
        } else {
            output$diff_bdt_morning_val_ui_selector <- renderUI({})
            output$diff_bdt_afternoon_val_ui_selector <- renderUI({})
            output$diff_bdt_evening_val_ui_selector <- renderUI({})
            output$diff_bdt_unknown_val_ui_selector <- renderUI({})
        }
    })
}

observe_fasting_status_selection <- function(input, output, session) {
    observe({
        req(input$pop)
        req(input$same_fs)

        # FASTING STATUS
        if (input$same_fs == "yes") {
            # Display the static fasting status selection dropdown
            output$same_fs_ui_selector <- renderUI({
                div(
                    selectInput("fastingVar",
                        "What is the general fasting status of your data? *",
                        choices = c("fasted", "non-fasted", "unknown")
                    ),
                    style = MediumDropDownDivStyle
                )
            })

            # Set custom column name and value selection UI to empty
            output$diff_fs_column_ui_selector <- renderUI({})
            output$diff_fs_fasted_val_ui_selector <- renderUI({})
            output$diff_fs_nonfasted_val_ui_selector <- renderUI({})
            output$diff_fs_unknown_val_ui_selector <- renderUI({})
        } else if (input$same_fs == "no") {
            # Set the static fasting status selection dropdown to empty
            output$same_fs_ui_selector <- renderUI({})

            # Display the custom column name and value selection UI
            output$diff_fs_column_ui_selector <- renderUI({
                selectInput("diff_fs_column", "What's the column name in the dataset indicating fasting status?", choices = "")
            })
        }
    })
}

observe_fasting_status_vals_selection <- function(input, output, session) {
    observe({
        req(input$diff_fs_column)
        if (input$same_fs == "no" && input$diff_fs_column != "") {
            output$diff_fs_fasted_val_ui_selector <- renderUI({
                selectInput("diff_fs_fasted_val", "\"Fasted\" Value", choices = "")
            })
            output$diff_fs_nonfasted_val_ui_selector <- renderUI({
                selectInput("diff_fs_nonfasted_val", "\"Non-Fasted\" Value", choices = "")
            })
            output$diff_fs_unknown_val_ui_selector <- renderUI({
                selectInput("diff_fs_unknown_val", "\"Unknown\" Value", choices = "")
            })
        } else {
            output$diff_fs_fasted_val_ui_selector <- renderUI({})
            output$diff_fs_nonfasted_val_ui_selector <- renderUI({})
            output$diff_fs_unknown_val_ui_selector <- renderUI({})
        }
    })
}

#######################
# Value Update Functions
#######################
observe_blood_draw_time_vals_update <- function(input, output, session, imported) {
    observe({
        updateSelectInput(session, "diff_bdt_column",
            choices = c("", names(imported$data())[!(names(imported$data()) %in% c(input$rt, input$ft, input$stfr, input$zn, input$agp, input$crp, input$fasting, input$time))]),
            selected = input$diff_bdt_column
        )
    })
    observe({
        req(input$diff_bdt_column)
        updateSelectInput(session, "diff_bdt_morning_val",
            choices = c("", imported$data()[input$diff_bdt_column]),
            selected = input$diff_bdt_morning_val
        )
    })
    observe({
        req(input$diff_bdt_column)
        updateSelectInput(session, "diff_bdt_afternoon_val",
            choices = c("", imported$data()[input$diff_bdt_column]),
            selected = input$diff_bdt_afternoon_val
        )
    })
    observe({
        req(input$diff_bdt_column)
        updateSelectInput(session, "diff_bdt_evening_val",
            choices = c("", imported$data()[input$diff_bdt_column]),
            selected = input$diff_bdt_evening_val
        )
    })
    observe({
        req(input$diff_bdt_column)
        updateSelectInput(session, "diff_bdt_unknown_val",
            choices = c("", imported$data()[input$diff_bdt_column]),
            selected = input$diff_bdt_unknown_val
        )
    })
}

observe_fasting_status_vals_update <- function(input, output, session, imported) {
    observe({
        updateSelectInput(session, "diff_fs_column",
            choices = c("", names(imported$data())[!(names(imported$data()) %in% c(input$rt, input$ft, input$stfr, input$zn, input$agp, input$crp, input$fasting, input$time))]),
            selected = input$diff_fs_column
        )
    })
    observe({
        req(input$diff_fs_column)
        updateSelectInput(session, "diff_fs_fasted_val",
            choices = c("", imported$data()[input$diff_fs_column]),
            selected = input$diff_fs_fasted_val
        )
    })
    observe({
        req(input$diff_fs_column)
        updateSelectInput(session, "diff_fs_nonfasted_val",
            choices = c("", imported$data()[input$diff_fs_column]),
            selected = input$diff_fs_nonfasted_val
        )
    })
    observe({
        req(input$diff_fs_column)
        updateSelectInput(session, "diff_fs_unknown_val",
            choices = c("", imported$data()[input$diff_fs_column]),
            selected = input$diff_fs_unknown_val
        )
    })
}
