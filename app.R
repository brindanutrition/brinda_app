# --- Load Libraries -----------------------------------------------------------
library(shiny)
library(ggplot2)
library(datamods)
library(sortable)
library(shinydashboard)
library(shinyalert)
library(DT)
library(tidyverse)
library(patchwork)
library(ggridges)
library(BRINDA)
library(fresh)


# --- Load local R module file paths --------------------------------------------
# Styles
source("ui/styles/color_theme.R")$value
cols <- cols

# UI
body <- source("ui/body.R")$value
sidebar <- source("ui/sidebar.R")$value
header <- source("ui/header.R")$value

# Server
source("server/navigation.R")$value
observe_sidebar_transitions <- observe_sidebar_transitions
observe_tab_navigation <- observe_tab_navigation

source("server/constants.R")$value
find_population_code_name <- find_population_code_name

source("server/biomarker.R")$value
observe_agp_crp_selection <- observe_agp_crp_selection

observe_blood_draw_time_selection <- observe_blood_draw_time_selection
observe_blood_draw_time_vals_selection <- observe_blood_draw_time_vals_selection
observe_blood_draw_time_vals_update <- observe_blood_draw_time_vals_update

observe_fasting_status_selection <- observe_fasting_status_selection
observe_fasting_status_vals_selection <- observe_fasting_status_vals_selection
observe_fasting_status_vals_update <- observe_fasting_status_vals_update

source("server/cutoff.R")
get_suggested_cutoffs <- get_suggested_cutoffs
apply_suggested_cutoffs <- apply_suggested_cutoffs
calculate_dynamic_zinc_cutoff <- calculate_dynamic_zinc_cutoff

# --- Shiny UI -----------------------------------------------------------------
ui <- function(request) {
  (dashboardPage(header, sidebar, body))
}

# --- Shiny Server -------------------------------------------------------------
server <- function(input, output, session) {
  # --- Section to Import Data -------------------------------------------------
  observeEvent(input$importData, {
    import_modal(
      id = "myid",
      from = c("file"),
      title = "Import data to be used in application"
    )
  })

  imported <- import_server("myid", return_class = "tbl_df")
  # --- Render data table ------------------------------------------------------
  output$table <- renderDataTable({
    req(imported$data())
    df <- imported$data()
    datatable(df, options = list(scrollX = T, lengthMenu = c(5, 10, 25)))
  })

  # --- Update Sidebar depending on what step you are on -----------------------
  observe_sidebar_transitions(input, output)

  # --- Update Selected Tab Based on Previous/Next Buttons ---------------------
  observe_tab_navigation(input, output, session)

  # --- Biomarker Tab Dynamic Input Selection ----------------------------------
  observe_agp_crp_selection(input, output, session)
  observe_blood_draw_time_selection(input, output, session)
  observe_blood_draw_time_vals_selection(input, output, session)
  observe_blood_draw_time_vals_update(input, output, session, imported)

  observe_fasting_status_selection(input, output, session)
  observe_fasting_status_vals_selection(input, output, session)
  observe_fasting_status_vals_update(input, output, session, imported)

  # --- Calculate dynamic zinc cutoff ------------------------------------------
  calculate_dynamic_zinc_cutoff(input, output, session, imported)

  # --- Updates Column Names Per User's Data -----------------------------------
  ## Dynamically removes options based on
  ## user's previous choice as to not
  ## select the same column twice
  observe({
    updateSelectInput(session, "rbp",
      label = "Retinol Binding Protein",
      choices = c("", names(imported$data())[!(names(imported$data()) %in% c(input$rt, input$ft, input$stfr, input$zn, input$agp, input$crp, input$fasting, input$time))]),
      selected = input$rbp
    )
    updateSelectInput(session, "rt",
      label = "Retinol",
      choices = c("", names(imported$data())[!(names(imported$data()) %in% c(input$rbp, input$ft, input$stfr, input$zn, input$agp, input$crp, input$fasting, input$time))]),
      selected = input$rt
    )
    updateSelectInput(session, "ft",
      label = "Ferritin",
      choices = c("", names(imported$data())[!(names(imported$data()) %in% c(input$rt, input$rbp, input$stfr, input$zn, input$agp, input$crp, input$fasting, input$time))]),
      selected = input$ft
    )
    updateSelectInput(session, "stfr",
      label = "Soluble Transferrin Receptor",
      choices = c("", names(imported$data())[!(names(imported$data()) %in% c(input$rt, input$ft, input$rbp, input$zn, input$agp, input$crp, input$fasting, input$time))]),
      selected = input$stfr
    )
    updateSelectInput(session, "zn",
      label = "Zinc",
      choices = c("", names(imported$data())[!(names(imported$data()) %in% c(input$rt, input$ft, input$stfr, input$rbp, input$agp, input$crp, input$fasting, input$time))]),
      selected = input$zn
    )
    updateSelectInput(session, "agp",
      label = "AGP *",
      choices = c("", names(imported$data())[!(names(imported$data()) %in% c(input$rt, input$ft, input$stfr, input$zn, input$rbp, input$crp, input$fasting, input$time))]),
      selected = input$agp
    )
    updateSelectInput(session, "crp",
      label = "CRP *",
      choices = c("", names(imported$data())[!(names(imported$data()) %in% c(input$rt, input$ft, input$stfr, input$zn, input$agp, input$rbp, input$fasting, input$time))]),
      selected = input$crp
    )
  })

  # --- Density Plots to be generated ------------------------------------------
  ## each time the user selects what
  ## columns in their data are the marker columns
  output$rbpPlot <- renderPlot({
    req(input$rbp)
    ggplot(imported$data(), aes(x = imported$data()[[as.character(input$rbp)]])) +
      geom_density(fill = cols[1], color = "darkslategrey") +
      theme_minimal() +
      ggtitle(
        paste("Retinol Binding Protein (", as.character(input$rbpU), ")", sep = "")
      ) +
      ylab("") +
      xlab(as.character(input$rbpU))
  })
  output$rtPlot <- renderPlot({
    req(input$rt)
    ggplot(imported$data(), aes(x = imported$data()[[as.character(input$rt)]])) +
      geom_density(fill = cols[2], color = "darkslategrey") +
      theme_minimal() +
      ggtitle(
        paste("Retinol (", as.character(input$rtU), ")", sep = "")
      ) +
      ylab("") +
      xlab(as.character(input$rtU))
  })
  output$ftPlot <- renderPlot({
    req(input$ft)
    ggplot(imported$data(), aes(x = imported$data()[[as.character(input$ft)]])) +
      geom_density(fill = cols[3], color = "darkslategrey") +
      theme_minimal() +
      ggtitle(
        paste("Ferritin (", as.character(input$ftU), ")", sep = "")
      ) +
      ylab("") +
      xlab(as.character(input$ftU))
  })
  output$stfrPlot <- renderPlot({
    req(input$stfr)
    ggplot(imported$data(), aes(x = imported$data()[[as.character(input$stfr)]])) +
      geom_density(fill = cols[4], color = "darkslategrey") +
      theme_minimal() +
      ggtitle(
        paste("Soluble Transferrin Receptor (", as.character(input$stfrU), ")", sep = "")
      ) +
      ylab("") +
      xlab(as.character(input$stfrU))
  })
  output$znPlot <- renderPlot({
    req(input$zn)
    ggplot(imported$data(), aes(x = imported$data()[[as.character(input$zn)]])) +
      geom_density(fill = cols[5], color = "darkslategrey") +
      theme_minimal() +
      ggtitle(
        paste("Zinc (", as.character(input$znU), ")", sep = "")
      ) +
      ylab("") +
      xlab(as.character(input$znU))
  })
  output$agpPlot <- renderPlot({
    req(input$agp)
    ggplot(imported$data(), aes(x = imported$data()[[as.character(input$agp)]])) +
      geom_density(fill = cols[6], color = "darkslategrey") +
      theme_minimal() +
      ggtitle(
        paste("AGP (", as.character(input$agpU), ")", sep = "")
      ) +
      ylab("") +
      xlab(as.character(input$agpU))
  })
  output$crpPlot <- renderPlot({
    req(input$crp)
    ggplot(imported$data(), aes(x = imported$data()[[as.character(input$crp)]])) +
      geom_density(fill = cols[7], color = "darkslategrey") +
      theme_minimal() +
      ggtitle(
        paste("CRP (", as.character(input$crpU), ")", sep = "")
      ) +
      ylab("") +
      xlab(as.character(input$crpU))
  })

  # --- Cutoff Unit Output -----------------------------------------------------
  ## Outputs the same unit as specified in step 2
  output$rbpUnitC <- renderText({
    req(input$rbpU)
    input$rbpU
  })
  output$rtUnitC <- renderText({
    req(input$rtU)
    input$rtU
  })
  output$ftUnitC <- renderText({
    req(input$ftU)
    input$ftU
  })
  output$stfrUnitC <- renderText({
    req(input$stfrU)
    input$stfrU
  })
  output$zn_morn_fast_UnitC <- renderText({
    req(input$znU)
    input$znU
  })
  output$zn_morn_nonfast_UnitC <- renderText({
    req(input$znU)
    input$znU
  })
  output$zn_after_nonfast_UnitC <- renderText({
    req(input$znU)
    input$znU
  })

  # --- apply suggested cutoffs -----------------------------------------------
  apply_suggested_cutoffs(input, output, session)

  # --- output suggested cutoffs -----------------------------------------------
  get_suggested_cutoffs(input, output)

  # --- Plot cutoff density plots ----------------------------------------------
  cutoffBarPlot <- function() {
    cutoff_map <- data.frame(
      variable = c(
        as.character(input$ft),
        as.character(input$stfr),
        as.character(input$rt),
        as.character(input$rbp),
        as.character(input$zn),
        as.character(input$agp),
        as.character(input$crp)
      ),
      cutoff = c(
        as.numeric(input$ftC),
        as.numeric(input$stfrC),
        as.numeric(input$rtC),
        as.numeric(input$rbpC),
        as.numeric(input$znC),
        1,
        5
      ),
      long_name = c(
        paste0("Ferritin", " (", as.character(input$ftU), ")"),
        paste0("Soluble Transferrin Receptor", " (", as.character(input$stfrU), ")"),
        paste0("Retinol", " (", as.character(input$rtU), ")"),
        paste0("Retinol Binding Protein", " (", as.character(input$rbpU), ")"),
        paste0("Zinc", " (", as.character(input$znU), ")"),
        paste0("AGP", " (", as.character(input$agpU), ")"),
        paste0("CRP", " (", as.character(input$crpU), ")")
      )
    )

    def_df <- imported$data() %>%
      reshape2::melt() %>%
      inner_join(.,
        cutoff_map,
        by = "variable"
      ) %>%
      group_by(variable, long_name) %>%
      mutate(
        deficient = ifelse(value < cutoff, "deficient", "normal"),
        stfr_deficient = ifelse(value > cutoff, "deficient", "normal")
      ) %>%
      summarise(
        percent_def = length(deficient[deficient == "deficient"]) /
          length(deficient) * 100,
        stfr_percent_def = length(stfr_deficient[stfr_deficient == "deficient"]) /
          length(stfr_deficient) * 100
      )

    # implementing fix for Soluble Transferrin Receptor deficiency
    def_df$percent_def[def_df$long_name == "Soluble Transferrin Receptor"] <- def_df$stfr_percent_def[def_df$long_name == "Soluble Transferrin Receptor"]

    # ensure order for biomarkers
    def_df$long_name <- factor(def_df$long_name,
      levels = c(
        paste0("Ferritin", " (", as.character(input$ftU), ")"),
        paste0("Soluble Transferrin Receptor", " (", as.character(input$stfrU), ")"),
        paste0("Retinol", " (", as.character(input$rtU), ")"),
        paste0("Retinol Binding Protein", " (", as.character(input$rbpU), ")"),
        paste0("Zinc", " (", as.character(input$znU), ")"),
        paste0("AGP", " (", as.character(input$agpU), ")"),
        paste0("CRP", " (", as.character(input$crpU), ")")
      )
    )

    # plot the deficiency for biomarkers
    ggplot(
      def_df %>%
        mutate(Reference = ifelse(
          grepl("agp|crp", long_name, ignore.case = TRUE),
          "Reference Biomarker",
          "Biomarker"
        )),
      aes(x = long_name, y = percent_def)
    ) +
      geom_bar(position = "dodge", stat = "identity", width = 0.5, fill = cols[6]) +
      theme_bw() +
      scale_y_continuous(labels = scales::percent) +
      ylim(c(0, 100)) +
      labs(
        x = "",
        y = "Deficiency (%)",
        title = "Biomarker Deficiency - Prior to BRINDA"
      ) +
      theme(
        text = element_text(size = 14),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14)
      ) +
      facet_grid(~Reference, scales = "free_x", space = "free")
  }
  output$cutoffBar <- renderPlot({
    print(cutoffBarPlot())
  })
  # --- Download BRINDA adjusted barplot ---------------------------------------
  output$downloadNonAdjBar <- downloadHandler(
    filename = "BRINDA_non_adjusted_barplot.png",
    content = function(file) {
      png(file)
      print(cutoffBarPlot())
      dev.off()
    }
  )
  # --- Apply BRINDA adjustment ------------------------------------------------
  ## make brinda variable available outside observe statement
  brinda <- reactive({
    dataset <- imported$data()
    to_map <- data.frame(
      variable = c(
        as.character(input$ft),
        as.character(input$stfr),
        as.character(input$rt),
        as.character(input$rbp),
        as.character(input$zn),
        as.character(input$agp),
        as.character(input$crp)
      ),
      long_name = c(
        "sf",
        "stfr",
        "sr",
        "rbp",
        "zn",
        "agp",
        "crp"
      )
    ) %>%
      mutate(variable = ifelse(variable == "",
        "not_present",
        variable
      ))
    long_names <- stringi::stri_replace_all_regex(
      str = names(dataset),
      pattern = to_map$variable,
      replacement = to_map$long_name,
      vectorise_all = FALSE
    )

    names(dataset) <- long_names

    all_names <- ifelse(to_map$long_name %in% long_names,
      to_map$long_name,
      NA
    )



    input_population <- find_population_code_name(input$pop)

    args <- list(
      retinol_binding_protein_varname = all_names[4],
      retinol_varname = all_names[3],
      ferritin_varname = all_names[1],
      soluble_transferrin_receptor_varname = all_names[2],
      zinc_varname = all_names[5],
      crp_varname = all_names[7],
      agp_varname = all_names[6],
      population = as.character(input_population),
      output_format = "FULL"
    )

    brinda <- purrr::pmap(args, BRINDA, dataset = dataset)
    if (as.character(input$outputType) == "only output adjusted values") {
      adj_names <- c(names(dataset), paste0(names(dataset), "_adj"))
      adj_names <- adj_names[adj_names %in% names(brinda[[1]])]
      result <- brinda[[1]] %>%
        select(all_of(adj_names))
    } else {
      result <- brinda[[1]]
    }
    return(result)
  })

  # --- output brinda results table --------------------------------------------
  output$brindaTbl <- renderDataTable({
    req(input$pop)
    req(input$outputType)
    df <- brinda()
    datatable(df, options = list(scrollX = T, lengthMenu = c(5, 10, 25)))
  })
  # --- Barplot of Adjusted/Unadjusted Values ----------------------------------
  adjBarPlot <- function() {
    df <- brinda()
    adj_names <- names(df)[grepl("adj|AGP|agp|crp|CRP", names(df))]
    non_adj_names <- gsub("_adj", "", adj_names)
    both_names <- c(non_adj_names, adj_names)
    tmp <- reshape2::melt(
      df %>%
        select(all_of(both_names))
    )
    to_map <- data.frame(
      variable = c(
        variable = c(
          "sf",
          "stfr",
          "sr",
          "rbp",
          "zn",
          "agp",
          "crp",
          "sf_adj",
          "stfr_adj",
          "sr_adj",
          "rbp_adj",
          "zn_adj"
        )
      ),
      long_name = c(
        paste0("Ferritin", " (", as.character(input$ftU), ")"),
        paste0("Soluble Transferrin Receptor", " (", as.character(input$stfrU), ")"),
        paste0("Retinol", " (", as.character(input$rtU), ")"),
        paste0("Retinol Binding Protein", " (", as.character(input$rbpU), ")"),
        paste0("Zinc", " (", as.character(input$znU), ")"),
        paste0("AGP", " (", as.character(input$agpU), ")"),
        paste0("CRP", " (", as.character(input$crpU), ")"),
        paste0("Ferritin - Adjusted", " (", as.character(input$ftU), ")"),
        paste0("Soluble Transferrin Receptor - Adjusted", " (", as.character(input$stfrU), ")"),
        paste0("Retinol - Adjusted", " (", as.character(input$rtU), ")"),
        paste0("Retinol Binding Protein - Adjusted", " (", as.character(input$rbpU), ")"),
        paste0("Zinc - Adjusted", " (", as.character(input$znU), ")")
      )
    )

    plottingDF <- tmp %>%
      group_by(variable) %>%
      summarize(
        geom_mean = exp(mean(log(value), na.rm = T))
      ) %>%
      mutate(Adjusted = ifelse(grepl("adj", variable), "BRINDA Adjusted", "Not Adjusted")) %>%
      inner_join(.,
        to_map,
        by = "variable"
      ) %>%
      mutate(base_var = factor(gsub(" - Adjusted", "", long_name),
        levels = c(
          paste0("Ferritin", " (", as.character(input$ftU), ")"),
          paste0("Soluble Transferrin Receptor", " (", as.character(input$stfrU), ")"),
          paste0("Retinol", " (", as.character(input$rtU), ")"),
          paste0("Retinol Binding Protein", " (", as.character(input$rbpU), ")"),
          paste0("Zinc", " (", as.character(input$znU), ")"),
          paste0("AGP", " (", as.character(input$agpU), ")"),
          paste0("CRP", " (", as.character(input$crpU), ")")
        )
      ))

    ggplot(plottingDF %>%
      mutate(Reference = ifelse(
        grepl("agp|crp", base_var, ignore.case = TRUE),
        "Reference Biomarker",
        "Biomarker"
      )), aes(x = base_var, y = geom_mean, fill = Adjusted)) +
      geom_bar(stat = "identity", width = .5, position = "dodge", alpha = 0.8) +
      theme_bw() +
      scale_fill_manual(values = cols[c(2, 6)]) +
      scale_y_log10() +
      labs(
        x = "",
        y = "Geometric Mean",
        fill = "",
        title = "BRINDA Adjustment Bar Plot"
      ) +
      theme(
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14)
      ) +
      facet_grid(~Reference, scales = "free_x", space = "free")
  }

  output$adjBarPlot <- renderPlot({
    print(adjBarPlot())
  })

  defBarPlot <- function() {
    df <- brinda()
    adj_names <- names(df)[grepl("adj|AGP|agp|crp|CRP", names(df))]
    non_adj_names <- gsub("_adj", "", adj_names)
    both_names <- c(non_adj_names, adj_names)
    adj_v_non_adj <- df %>%
      select(all_of(both_names))
    cutoff_map <- data.frame(
      variable = c(
        variable = c(
          "sf",
          "stfr",
          "sr",
          "rbp",
          "zn",
          "agp",
          "crp",
          "sf_adj",
          "stfr_adj",
          "sr_adj",
          "rbp_adj",
          "zn_adj"
        )
      ),
      cutoff = c(
        as.numeric(input$ftC),
        as.numeric(input$stfrC),
        as.numeric(input$rtC),
        as.numeric(input$rbpC),
        as.numeric(input$znC),
        1,
        5,
        as.numeric(input$ftC),
        as.numeric(input$stfrC),
        as.numeric(input$rtC),
        as.numeric(input$rbpC),
        as.numeric(input$znC)
      ),
      long_name = c(
        paste0("Ferritin", " (", as.character(input$ftU), ")"),
        paste0("Soluble Transferrin Receptor", " (", as.character(input$stfrU), ")"),
        paste0("Retinol", " (", as.character(input$rtU), ")"),
        paste0("Retinol Binding Protein", " (", as.character(input$rbpU), ")"),
        paste0("Zinc", " (", as.character(input$znU), ")"),
        paste0("AGP", " (", as.character(input$agpU), ")"),
        paste0("CRP", " (", as.character(input$crpU), ")"),
        paste0("Ferritin - Adjusted", " (", as.character(input$ftU), ")"),
        paste0("Soluble Transferrin Receptor - Adjusted", " (", as.character(input$stfrU), ")"),
        paste0("Retinol - Adjusted", " (", as.character(input$rtU), ")"),
        paste0("Retinol Binding Protein - Adjusted", " (", as.character(input$rbpU), ")"),
        paste0("Zinc - Adjusted", " (", as.character(input$znU), ")")
      )
    )

    def_df <- adj_v_non_adj %>%
      reshape2::melt() %>%
      inner_join(.,
        cutoff_map,
        by = "variable"
      ) %>%
      group_by(variable, long_name) %>%
      mutate(
        deficient = ifelse(value < cutoff, "deficient", "normal"),
        stfr_deficient = ifelse(value > cutoff, "deficient", "normal")
      ) %>%
      summarise(
        percent_def = length(deficient[deficient == "deficient"]) /
          length(deficient) * 100,
        stfr_percent_def = length(stfr_deficient[stfr_deficient == "deficient"]) /
          length(stfr_deficient) * 100
      )

    # implementing fix for Soluble Transferrin Receptor deficiency
    def_df$percent_def[def_df$long_name == "Soluble Transferrin Receptor"] <- def_df$stfr_percent_def[def_df$long_name == "Soluble Transferrin Receptor"]

    # add in the biomarker column
    def_df$base_var <- factor(gsub(" - Adjusted", "", def_df$long_name),
      levels = c(
        paste0("Ferritin", " (", as.character(input$ftU), ")"),
        paste0("Soluble Transferrin Receptor", " (", as.character(input$stfrU), ")"),
        paste0("Retinol", " (", as.character(input$rtU), ")"),
        paste0("Retinol Binding Protein", " (", as.character(input$rbpU), ")"),
        paste0("Zinc", " (", as.character(input$znU), ")"),
        paste0("AGP", " (", as.character(input$agpU), ")"),
        paste0("CRP", " (", as.character(input$crpU), ")")
      )
    )
    def_df$adj <- ifelse(grepl("Adjusted", def_df$long_name),
      "BRINDA Adjusted",
      "Not Adjusted"
    )

    # plot the deficiency for biomarkers
    ggplot(def_df %>%
      mutate(Reference = ifelse(
        grepl("agp|crp", base_var, ignore.case = TRUE),
        "Reference Biomarker",
        "Biomarker"
      )), aes(x = base_var, y = percent_def, fill = adj)) +
      geom_bar(position = "dodge", stat = "identity", width = 0.5) +
      theme_bw() +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = cols[c(2, 6)]) +
      ylim(c(0, 100)) +
      labs(
        x = "",
        y = "Deficiency (%)",
        title = "Biomarker Deficiency",
        fill = ""
      ) +
      theme(
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14)
      ) +
      facet_grid(~Reference, scales = "free_x", space = "free")
  }


  output$defBarPlot <- renderPlot({
    print(defBarPlot())
  })

  density <- function() {
    df <- brinda()
    adj_names <- names(df)[grepl("adj|AGP|agp|crp|CRP", names(df))]
    non_adj_names <- gsub("_adj", "", adj_names)
    both_names <- c(non_adj_names, adj_names)

    to_map <- data.frame(
      variable = c(
        variable = c(
          "sf",
          "stfr",
          "sr",
          "rbp",
          "zn",
          "agp",
          "crp",
          "sf_adj",
          "stfr_adj",
          "sr_adj",
          "rbp_adj",
          "zn_adj"
        )
      ),
      long_name = c(
        paste0("Ferritin", " (", as.character(input$ftU), ")"),
        paste0("Soluble Transferrin Receptor", " (", as.character(input$stfrU), ")"),
        paste0("Retinol", " (", as.character(input$rtU), ")"),
        paste0("Retinol Binding Protein", " (", as.character(input$rbpU), ")"),
        paste0("Zinc", " (", as.character(input$znU), ")"),
        paste0("AGP", " (", as.character(input$agpU), ")"),
        paste0("CRP", " (", as.character(input$crpU), ")"),
        paste0("Ferritin - Adjusted", " (", as.character(input$ftU), ")"),
        paste0("Soluble Transferrin Receptor - Adjusted", " (", as.character(input$stfrU), ")"),
        paste0("Retinol - Adjusted", " (", as.character(input$rtU), ")"),
        paste0("Retinol Binding Protein - Adjusted", " (", as.character(input$rbpU), ")"),
        paste0("Zinc - Adjusted", " (", as.character(input$znU), ")")
      )
    )

    tmp <- reshape2::melt(
      df %>%
        select(all_of(both_names))
    ) %>%
      mutate(Adjusted = ifelse(grepl("adj", variable), "BRINDA Adjusted", "Not Adjusted")) %>%
      inner_join(
        .,
        to_map,
        by = c("variable")
      ) %>%
      mutate(base_var = factor(gsub(" - Adjusted", "", long_name),
        levels = c(
          paste0("CRP", " (", as.character(input$crpU), ")"),
          paste0("AGP", " (", as.character(input$agpU), ")"),
          paste0("Zinc", " (", as.character(input$znU), ")"),
          paste0("Retinol Binding Protein", " (", as.character(input$rbpU), ")"),
          paste0("Retinol", " (", as.character(input$rtU), ")"),
          paste0("Soluble Transferrin Receptor", " (", as.character(input$stfrU), ")"),
          paste0("Ferritin", " (", as.character(input$ftU), ")")
        )
      ))

    ggplot(tmp %>%
      mutate(Reference = ifelse(
        grepl("agp|crp", base_var, ignore.case = TRUE),
        "Reference Biomarker",
        "Biomarker"
      )), aes(y = base_var, x = value, fill = Adjusted)) +
      geom_density_ridges(color = "darkslategrey", alpha = 0.8) +
      scale_fill_manual(values = cols[c(2, 6)]) +
      theme_bw() +
      scale_x_log10() +
      labs(
        x = "",
        y = "",
        fill = "",
        title = "BRINDA Adjustment Density Plot"
      ) +
      theme(text = element_text(size = 14)) +
      facet_grid(Reference ~ ., scales = "free_y", space = "free")
  }

  output$densityplot <- renderPlot({
    print(density())
  })
  # --- Download BRINDA adjusted table -----------------------------------------
  output$downloadRes <- downloadHandler(
    filename = function() {
      paste("BRINDA_Results", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(brinda(), file, row.names = FALSE)
    }
  )
  # --- Download BRINDA adjusted barplot ---------------------------------------
  output$downloadAdjBar <- downloadHandler(
    filename = "BRINDA_adjustment_barplot.png",
    content = function(file) {
      png(file)
      print(adjBarPlot())
      dev.off()
    }
  )
  # --- Download BRINDA adjusted barplot ---------------------------------------
  output$downloadDefBar <- downloadHandler(
    filename = "BRINDA_deficiency_barplot.png",
    content = function(file) {
      png(file)
      print(defBarPlot())
      dev.off()
    }
  )
  # --- Download BRINDA adjusted density plot ----------------------------------
  output$downloadDensity <- downloadHandler(
    filename = "BRINDA_densityplot.png",
    content = function(file) {
      png(file)
      print(density())
      dev.off()
    }
  )
  # --- Download BRINDA adjusted table -----------------------------------------
  output$downloadRes2 <- downloadHandler(
    filename = function() {
      paste("BRINDA_Results", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(brinda(), file, row.names = FALSE)
    }
  )
  # --- Create a stats object --------------------------------------------------
  stats <- function() {
    to_map <- data.frame(
      variable = c(
        as.character(input$ft),
        as.character(input$stfr),
        as.character(input$rt),
        as.character(input$rbp),
        as.character(input$zn),
        as.character(input$agp),
        as.character(input$crp)
      ),
      Biomarker = c(
        "Ferritin",
        "Soluble Transferrin Receptor",
        "Retinol",
        "Retinol Binding Protein",
        "Zinc",
        "AGP",
        "CRP"
      ),
      Unit = c(
        as.character(input$ftU),
        as.character(input$stfrU),
        as.character(input$rtU),
        as.character(input$rbpU),
        as.character(input$znU),
        as.character(input$agpU),
        as.character(input$crpU)
      )
    )

    stats <- imported$data() %>%
      reshape2::melt() %>%
      group_by(variable) %>%
      summarise(
        N = length(value[!is.na(value)]),
        Minimum = min(value[!is.na(value)], na.rm = T),
        Mean = mean(value[!is.na(value)], na.rm = T),
        `Geometric Mean` = exp(mean(log(value[!is.na(value)]), na.rm = T)),
        Median = median(value[!is.na(value)], na.rm = T),
        Max = max(value[!is.na(value)], na.rm = T)
      )

    stats_w_biomarker <- to_map %>%
      inner_join(.,
        stats,
        by = "variable"
      ) %>%
      rename("Variable" = "variable")
  }

  init_def <- function() {
    cutoff_map <- data.frame(
      variable = c(
        as.character(input$ft),
        as.character(input$stfr),
        as.character(input$rt),
        as.character(input$rbp),
        as.character(input$zn),
        as.character(input$agp),
        as.character(input$crp)
      ),
      cutoff = c(
        as.numeric(input$ftC),
        as.numeric(input$stfrC),
        as.numeric(input$rtC),
        as.numeric(input$rbpC),
        as.numeric(input$znC),
        1,
        5
      ),
      unit = c(
        as.character(input$ftU),
        as.character(input$stfrU),
        as.character(input$rtU),
        as.character(input$rbpU),
        as.character(input$znU),
        as.character(input$agpU),
        as.character(input$crpU)
      ),
      Biomarker = c(
        "Ferritin",
        "Soluble Transferrin Receptor",
        "Retinol",
        "Retinol Binding Protein",
        "Zinc",
        "AGP",
        "CRP"
      )
    )

    def_df <- imported$data() %>%
      reshape2::melt() %>%
      inner_join(.,
        cutoff_map,
        by = "variable"
      ) %>%
      group_by(variable, Biomarker, unit, cutoff) %>%
      mutate(
        deficient = ifelse(value < cutoff, "deficient", "normal"),
        stfr_deficient = ifelse(value > cutoff, "deficient", "normal")
      ) %>%
      summarise(
        `Percent Deficiency` = length(deficient[deficient == "deficient"]) /
          length(deficient) * 100,
        stfr_percent_def = length(stfr_deficient[stfr_deficient == "deficient"]) /
          length(stfr_deficient) * 100
      ) %>%
      ungroup()

    # implementing fix for Soluble Transferrin Receptor deficiency
    if ("Soluble Transferrin Receptor" %in% def_df$Biomarker) {
      def_df$`Percent Deficiency` <- ifelse(def_df$Biomarker == "Soluble Transferrin Receptor",
        def_df$stfr_percent_def,
        def_df$`Percent Deficiency`
      )
      def_df <- def_df %>%
        filter(!is.na(cutoff)) %>%
        mutate(`Value Cutoff` = ifelse(Biomarker == "Soluble Transferrin Receptor",
          paste(">", as.character(cutoff), unit),
          paste("<", as.character(cutoff), unit)
        )) %>%
        select(-c(stfr_percent_def, cutoff, unit)) %>%
        rename("Variable" = "variable")
    } else {
      def_df <- def_df %>%
        filter(!is.na(cutoff)) %>%
        mutate(`Value Cutoff` = ifelse(Biomarker == "Soluble Transferrin Receptor",
          paste(">", as.character(cutoff), unit),
          paste("<", as.character(cutoff), unit)
        )) %>%
        select(-c(stfr_percent_def, cutoff, unit)) %>%
        rename("Variable" = "variable")
    }
    return(def_df)
  }

  adj_def_df <- function() {
    cutoff_map <- data.frame(
      variable = c(
        "sf",
        "stfr",
        "sr",
        "rbp",
        "zn",
        "agp",
        "crp",
        "sf_adj",
        "stfr_adj",
        "sr_adj",
        "rbp_adj",
        "zn_adj"
      ),
      Biomarker = c(
        "Ferritin",
        "Soluble Transferrin Receptor",
        "Retinol",
        "Retinol Binding Protein",
        "Zinc",
        "AGP",
        "CRP",
        "Ferritin - Adjusted",
        "Soluble Transferrin Receptor - Adjusted",
        "Retinol - Adjusted",
        "Retinol Binding Protein - Adjusted",
        "Zinc - Adjusted"
      ),
      cutoff = c(
        as.numeric(input$ftC),
        as.numeric(input$stfrC),
        as.numeric(input$rtC),
        as.numeric(input$rbpC),
        as.numeric(input$znC),
        1,
        5,
        as.numeric(input$ftC),
        as.numeric(input$stfrC),
        as.numeric(input$rtC),
        as.numeric(input$rbpC),
        as.numeric(input$znC)
      ),
      unit = c(
        as.character(input$ftU),
        as.character(input$stfrU),
        as.character(input$rtU),
        as.character(input$rbpU),
        as.character(input$znU),
        as.character(input$agpU),
        as.character(input$crpU),
        as.character(input$ftU),
        as.character(input$stfrU),
        as.character(input$rtU),
        as.character(input$rbpU),
        as.character(input$znU)
      )
    )

    def_df <- brinda() %>%
      reshape2::melt() %>%
      inner_join(.,
        cutoff_map,
        by = "variable"
      ) %>%
      group_by(variable, Biomarker, unit, cutoff) %>%
      mutate(
        deficient = ifelse(value < cutoff, "deficient", "normal"),
        stfr_deficient = ifelse(value > cutoff, "deficient", "normal")
      ) %>%
      summarise(
        `Percent Deficiency` = length(deficient[deficient == "deficient"]) /
          length(deficient) * 100,
        stfr_percent_def = length(stfr_deficient[stfr_deficient == "deficient"]) /
          length(stfr_deficient) * 100
      ) %>%
      ungroup()

    # implementing fix for Soluble Transferrin Receptor deficiency
    if ("Soluble Transferrin Receptor" %in% def_df$Biomarker) {
      def_df$`Percent Deficiency` <- ifelse(def_df$Biomarker == "Soluble Transferrin Receptor",
        def_df$stfr_percent_def,
        def_df$`Percent Deficiency`
      )
      def_df <- def_df %>%
        filter(!is.na(cutoff)) %>%
        mutate(`Value Cutoff` = ifelse(Biomarker == "Soluble Transferrin Receptor",
          paste(">", as.character(cutoff), unit),
          paste("<", as.character(cutoff), unit)
        )) %>%
        select(-c(stfr_percent_def, cutoff, unit)) %>%
        rename("Variable" = "variable")
    } else {
      def_df <- def_df %>%
        filter(!is.na(cutoff)) %>%
        mutate(`Value Cutoff` = ifelse(grepl("Soluble Transferrin Receptor", Biomarker),
          paste(">", as.character(cutoff), unit),
          paste("<", as.character(cutoff), unit)
        )) %>%
        select(-c(stfr_percent_def, cutoff, unit)) %>%
        rename("Variable" = "variable")
    }
    return(def_df)
  }

  # --- Download BRINDA adjustment Report --------------------------------------
  output$report <- downloadHandler(
    filename = function() {
      paste("BRINDA_Adjustment_Report", sep = ".", switch(input$format,
        PDF = "pdf",
        HTML = "html",
        Word = "docx"
      ))
    },
    content = function(file) {
      shinyalert("DONE",
        paste("BRINDA adjustment report was generated successfully."),
        type = "success"
      )
      shiny::withProgress(
        message = paste0("Rendering", input$report, " Report"),
        value = 0,
        {
          shiny::incProgress(1 / 10)
          Sys.sleep(1)
          shiny::incProgress(5 / 10)
          src <- normalizePath("./reports/report.Rmd")

          # temporarily switch to the temp dir, in case you do not have write
          # permission to the current working directory
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, "report.Rmd", overwrite = TRUE)

          library(rmarkdown)
          out <- render("report.Rmd",
            switch(input$format,
              PDF = pdf_document(),
              HTML = html_document(
                toc = TRUE,
                toc_depth = 3,
                toc_float = TRUE,
                theme = "simplex"
              ),
              Word = word_document()
            ),
            params = list(
              data = imported$data(),
              rbp = input$rbp,
              rt = input$rt,
              ft = input$ft,
              stfr = input$stfr,
              zn = input$zn,
              agp = input$agp,
              crp = input$crp,
              pop = input$pop,
              rbpC = input$rbpC,
              rtC = input$rtC,
              ftC = input$ftC,
              stfrC = input$stfrC,
              znC = input$znC,
              initCut = cutoffBarPlot(),
              refAgp = input$refAgp,
              refCrp = input$refCrp,
              stats = stats(),
              init_df = init_def(),
              adj_def_df = adj_def_df(),
              adjBar = adjBarPlot(),
              defBar = defBarPlot(),
              density = density()
            )
          )
          file.rename(out, file)
          shiny::incProgress(10 / 10)
        }
      )
    }
  )
}
# --- Shiny Server and UI Call -------------------------------------------------
options(shiny.autoreload = TRUE)
options(shiny.maxRequestSize = 300 * 1024^2) # 300 MB
shinyApp(ui, server)
