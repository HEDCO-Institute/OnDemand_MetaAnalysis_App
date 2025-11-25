# app.R

# 0) SET-UP ----------------
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(tidyverse)
library(rio)
library(here)
library(janitor)

# Import Depression MA data

# Get sheet names
sheets <- readxl::excel_sheets(here("data","Depression_Overview_Meta_Analysis_Data.xlsx"))

# Import all sheets into one df
dpo_ma_raw <- rio::import_list(here::here("data","Depression_Overview_Meta_Analysis_Data.xlsx"), which = sheets[-1], rbind=TRUE) %>%
  clean_names()

dpo_raw <- import(here("data", "Depression_Overview_Study_Level_Distiller.xlsx")) %>% 
  clean_names()
  


# 1) THEME & DUMMY DATA --------------------------------------------------

my_teal    <- "#489D46"
my_dark    <- "#4D5859"
my_lightbg <- "#D8DCDA"

my_theme <- bs_theme(
  version      = 4,
  bootswatch    = "flatly",
  primary      = my_teal,
  secondary    = my_dark,
  bg           = my_lightbg,
  fg           = my_dark,
  base_font    = font_google("Source Sans Pro"),
  heading_font = font_google("Source Sans Pro"),
  font_scale   = 1.2
)

set.seed(42)
df_demo <- data.frame(
  study_name   = paste("Study", 1:12),
  problem      = rep(c("Anxiety","Depression","ADHD"), each = 4),
  intervention = rep(c("CBT","Mindfulness","SEL","Family","MTSS"), length.out = 12),
  age_group    = rep(c("Elementary","Middle School","High School","All Ages"), times = 3),
  mean_effect  = runif(12, -0.5, 0.8)
) %>%
  rowwise() %>%
  mutate(
    half_width  = runif(1, 0.1, 0.3),
    x95_lower   = mean_effect - half_width,
    x95_upper   = mean_effect + half_width,
    sample_size = sample(50:200, 1)
  ) %>%
  ungroup()

# Clean Depression Data

# Critical value from standard Z distribution to make a 95% CI from vi
z <- qnorm(1 - .05/2)

# Re-format meta-analytic data
dpo_ma_td <- dpo_ma_raw %>% 
  mutate(se = sqrt(vi),
         ci_lower = yi - z*se,
         ci_upper = yi + z*se,
         total_n = baseline_intervention_n + baseline_comparison_n) %>% 
  select(refid, study, yi, vi,  total_n, se, ci_lower, ci_upper, 
         intervention, comparison, starts_with("outcome"),
         ends_with("_n"), ends_with("_mean"), ends_with("_sd"),
         file, intervention_distiller, 52:61)

# Re-format study level data

## Helper functions
# A value means selected; NA or "" means not selected
is_on <- function(x) {
  if (is.logical(x)) !is.na(x) & x
  else if (is.character(x)) !is.na(x) & trimws(x) != ""
  else !is.na(x)
}

dpo_td <- dpo_raw %>%
  mutate(
    # Grades (value/NA style). Include K if you have it (see note below).
    k8_present = if_any(all_of(paste0("study_grade_level_", 1:8)),  ~ is_on(.x)),
    hs_present = if_any(all_of(paste0("study_grade_level_", 9:12)), ~ is_on(.x)),
    grades_known = k8_present | hs_present,
    
    # School level flags
    elem_school = is_on(study_school_level_elementary),
    mid_school  = is_on(study_school_level_middle),
    high_school = is_on(study_school_level_high),
    
    # Only-reported + cannot tell
    primary_only     = is_on(study_school_level_only_reported_primary),
    secondary_only   = is_on(study_school_level_only_reported_secondary),
    school_cant_tell = is_on(study_school_level_cannot_tell),
    
    # FINAL band (K-8 / 9-12 / K-12 / Not Reported)
    age_group = case_when(
      # 1) Prefer explicit grades
      grades_known & k8_present & hs_present ~ "K-12",
      grades_known & hs_present              ~ "9-12",
      grades_known & k8_present              ~ "K-8",
      
      # 2) Only-reported flags (now BEFORE Not Reported)
      !grades_known & primary_only & secondary_only ~ "K-12",
      !grades_known & primary_only                  ~ "K-8",
      !grades_known & secondary_only                ~ "9-12",
      
      # 3) Regular school-level flags
      !grades_known & high_school & (elem_school | mid_school) ~ "K-12",
      !grades_known & (elem_school | mid_school) & !high_school ~ "K-8",
      !grades_known & high_school & !(elem_school | mid_school) ~ "9-12",
      
      # 4) Explicit "cannot tell"
      school_cant_tell ~ "Not Reported",
      
      TRUE ~ "Not Reported"
    )
  )


# Merge dpo
dpo_full <- dpo_ma_td %>% 
  left_join(dpo_td, by = "refid") %>% 
  rename(study_name = study)

# Format for app
dpo_app <- dpo_full %>%
  mutate(
    mean_effect = yi,
    x95_lower   = ci_lower,
    x95_upper   = ci_upper,
    sample_size = total_n,          # use computed total_n from MA file
    problem      = file,            # file = actual outcome bucket
    intervention = intervention,    # pass through as-is
    study_name   = study_name       # already renamed upstream
  )

# 2a) Jitter‐only plot (studies only) -------------------------------------

# plot_jitter <- function(df, effect_better = "positive") {
#   pos_col <- if (effect_better=="positive") "#007030" else "#964B00"
#   neg_col <- if (effect_better=="positive") "#964B00" else "#007030"
#   
#   df2 <- df %>%
#     mutate(
#       sd_i      = (x95_upper - x95_lower)/(2*1.96),
#       precision = 1/sd_i
#     )
#   levels_y <- df2$study_name
#   
#   set.seed(2025)
#   cloud <- df2 %>%
#     rowwise() %>%
#     mutate(draws = list(rnorm(200, mean_effect, sd = sd_i))) %>%
#     unnest(draws) %>%
#     ungroup() %>%
#     mutate(
#       y_label  = factor(study_name, levels = levels_y),
#       draw_dir = draws > 0
#     )
#   
#   pts <- df2 %>%
#     mutate(
#       y_label = factor(study_name, levels = levels_y),
#       dir     = mean_effect > 0
#     )
#   
#   # candidate breaks at 0%, 50%, 100% quantiles
#   precs      <- df2$precision
#   candidate  <- as.numeric(quantile(precs, c(0, 0.5, 1)))
#   labs       <- c("Least precise", "Medium precise", "Most precise")
#   # dedupe to avoid ggplot error
#   brks       <- unique(candidate)
#   labs_final <- labs[match(brks, candidate)]
#   
#   ggplot() +
#     annotate("rect", xmin=-Inf, xmax=0, ymin=-Inf, ymax=Inf, fill=neg_col, alpha=0.05) +
#     annotate("rect", xmin=0,    xmax=Inf, ymin=-Inf, ymax=Inf, fill=pos_col, alpha=0.05) +
#     geom_vline(xintercept=0, linetype="dashed", color="gray70") +
#     
#     geom_jitter(data=cloud,
#                 aes(x=draws, y=y_label, fill=draw_dir),
#                 shape=21, color="black", alpha=0.04,
#                 size=1.2, height=0.1, show.legend=FALSE) +
#     
#     geom_point(data=pts,
#                aes(x=mean_effect, y=y_label,
#                    size=precision, color=dir),
#                shape=19, show.legend=FALSE) +
#     
#     scale_fill_manual(values = c(`TRUE`=pos_col, `FALSE`=neg_col), guide="none") +
#     scale_color_manual(values = c(`TRUE`=pos_col, `FALSE`=neg_col), guide="none") +
#     scale_size_continuous(
#       name   = "Precision",
#       range  = c(2, 6),
#       breaks = brks,
#       labels = labs_final
#     ) +
#     
#     labs(x="Effect Size", y=NULL) +
#     theme_minimal(base_family="Source Sans Pro") +
#     theme(axis.text.y = element_text(color = my_dark))
# }

plot_jitter <- function(df, effect_better = "positive") {
  pos_col <- if (effect_better=="positive") "#007030" else "#964B00"
  neg_col <- if (effect_better=="positive") "#964B00" else "#007030"
  
  # Detect which field varies within a study
  df2 <- df %>%
    mutate(
      study_name = dplyr::if_else(is.na(study_name) | study_name=="", paste0("Ref ", refid), study_name),
      sd_i      = (x95_upper - x95_lower)/(2*1.96),
      precision = 1/sd_i
    ) %>%
    group_by(study_name) %>%
    mutate(
      varies_tp   = dplyr::n_distinct(outcome_timepoint,  na.rm = TRUE) > 1,
      varies_meas = dplyr::n_distinct(outcome_measure,    na.rm = TRUE) > 1,
      varies_int  = dplyr::n_distinct(intervention,       na.rm = TRUE) > 1,
      varies_comp = dplyr::n_distinct(comparison,         na.rm = TRUE) > 1,
      sub_label = dplyr::case_when(
        varies_tp   ~ paste0("TP=", outcome_timepoint),
        varies_meas ~ outcome_measure,
        varies_int  ~ paste0("Int=", intervention),
        varies_comp ~ paste0("Comp=", comparison),
        TRUE ~ ""
      ),
      y_label_raw = ifelse(sub_label == "", study_name, paste0(study_name, " — ", sub_label))
    ) %>%
    ungroup()
  
  # unique, non-NA factor levels
  levels_y <- unique(as.character(df2$y_label_raw))
  levels_y <- levels_y[!is.na(levels_y)]
  
  set.seed(2025)
  cloud <- df2 %>%
    rowwise() %>%
    mutate(draws = list(rnorm(200, mean_effect, sd = sd_i))) %>%
    tidyr::unnest(draws) %>%
    ungroup() %>%
    mutate(
      y_label  = factor(y_label_raw, levels = levels_y),
      draw_dir = draws > 0
    )
  
  pts <- df2 %>%
    mutate(
      y_label = factor(y_label_raw, levels = levels_y),
      dir     = mean_effect > 0
    )
  
  precs      <- pts$precision
  candidate  <- as.numeric(quantile(precs, c(0, 0.5, 1), na.rm = TRUE))
  labs       <- c("Least precise", "Medium precise", "Most precise")
  brks       <- unique(candidate)
  labs_final <- labs[match(brks, candidate)]
  
  ggplot() +
    annotate("rect", xmin=-Inf, xmax=0, ymin=-Inf, ymax=Inf, fill=neg_col, alpha=0.05) +
    annotate("rect", xmin=0,    xmax=Inf, ymin=-Inf, ymax=Inf, fill=pos_col, alpha=0.05) +
    geom_vline(xintercept=0, linetype="dashed", color="gray70") +
    geom_jitter(data=cloud,
                aes(x=draws, y=y_label, fill=draw_dir),
                shape=21, color="black", alpha=0.04,
                size=1.2, height=0.1, show.legend=FALSE) +
    geom_point(data=pts,
               aes(x=mean_effect, y=y_label, size=precision, color=dir),
               shape=19, show.legend=FALSE) +
    scale_fill_manual(values = c(`TRUE`=pos_col, `FALSE`=neg_col), guide="none") +
    scale_color_manual(values = c(`TRUE`=pos_col, `FALSE`=neg_col), guide="none") +
    scale_size_continuous(
      name   = "Precision",
      range  = c(2, 6),
      breaks = brks,
      labels = labs_final
    ) +
    labs(x="Effect Size", y=NULL) +
    theme_minimal(base_family="Source Sans Pro") +
    theme(axis.text.y = element_text(color = my_dark))
}

#different by timepoint, intervention (what else?)

# 2b) Density of overall --------------------------------------------------

plot_density <- function(df, effect_better = "positive") {
  pos_col <- if (effect_better=="positive") "#007030" else "#964B00"
  neg_col <- if (effect_better=="positive") "#964B00" else "#007030"
  
  total_n    <- sum(df$sample_size)
  overall_m  <- sum(df$mean_effect * df$sample_size) / total_n
  sd_overall <- sd(df$mean_effect) / sqrt(nrow(df))
  
  # only sensible when at least two studies
  if (nrow(df) < 2 || is.na(sd_overall) || sd_overall == 0) {
    return(NULL)
  }
  
  dens_df <- tibble(draws = rnorm(500, overall_m, sd_overall))
  fill_col <- if (overall_m > 0) pos_col else neg_col
  
  ggplot(dens_df, aes(x = draws)) +
    geom_density(fill = fill_col, alpha = 0.4, color = NA) +
    labs(x="Overall Effect Size", y="Density") +
    theme_minimal(base_family="Source Sans Pro") +
    theme(
      axis.title  = element_text(color = my_dark),
      axis.text   = element_text(color = my_dark),
      plot.margin = margin(5,5,20,5)
    )
}

plot_overall_jitter <- function(df, effect_better = "positive") {
  pos_col <- if (effect_better=="positive") "#007030" else "#964B00"
  neg_col <- if (effect_better=="positive") "#964B00" else "#007030"
  z <- qnorm(0.975)
  
  # per-study sd from CI and precision (same as your other plot)
  df2 <- df %>%
    mutate(
      sd_i      = (x95_upper - x95_lower) / (2*1.96),
      precision = 1 / pmax(sd_i, .Machine$double.eps),
      dir       = mean_effect > 0,
      y_label   = factor("All studies")     # single row
    )
  
  # weights (sample-size weighted — matches your summary text)
  w <- df2$sample_size
  w[is.na(w) | w <= 0] <- 1                 # small guard so NA/0 doesn't blow up
  
  overall_m  <- sum(df2$mean_effect * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
  se_overall <- sqrt(sum((w^2) * (df2$sd_i^2), na.rm = TRUE)) / sum(w, na.rm = TRUE)
  overall_lo <- overall_m - z * se_overall
  overall_hi <- overall_m + z * se_overall
  overall_df <- data.frame(
    y_label = factor("All studies"),
    m = overall_m, lo = overall_lo, hi = overall_hi,
    dir = overall_m > 0
  )
  
  ggplot() +
    annotate("rect", xmin=-Inf, xmax=0, ymin=-Inf, ymax=Inf, fill=neg_col, alpha=0.05) +
    annotate("rect", xmin=0,    xmax=Inf, ymin=-Inf, ymax=Inf, fill=pos_col, alpha=0.05) +
    geom_vline(xintercept=0, linetype="dashed", color="gray70") +
    
    # jittered per-study points
    geom_jitter(
      data = df2,
      aes(x = mean_effect, y = y_label, fill = dir),
      shape = 21, color = "black", alpha = 0.25, size = 2,
      height = 0.12, width = 0, show.legend = FALSE
    ) +
    
    # overall CI + point
    geom_errorbarh(
      data = overall_df,
      aes(y = y_label, xmin = lo, xmax = hi),
      height = 0.14, linewidth = 0.9, color = "black"
    ) +
    geom_point(
      data = overall_df,
      aes(x = m, y = y_label),
      shape = 23, size = 5,
      fill = if (overall_m > 0) pos_col else neg_col,
      color = "black"
    ) +
    
    scale_fill_manual(values = c(`TRUE`=pos_col, `FALSE`=neg_col), guide="none") +
    labs(x = "Effect Size", y = NULL) +
    theme_minimal(base_family = "Source Sans Pro") +
    theme(
      axis.text.y = element_text(color = my_dark),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(5,5,20,5)
    )
}

# 3) UI -------------------------------------------------------------------

ui <- fluidPage(
  theme = my_theme,
  tags$head(tags$style(HTML("
    body { background-color: #F7F7F7; }
    .wizard-container {
      max-width: 900px;
      margin: 50px auto;
      display: flex;
      gap: 30px;
      align-items: flex-start;
    }
    .sidebar {
      width: 140px;
      background: white;
      border: 1px solid #DDD;
      border-radius: 4px;
      padding: 20px 0;
      box-shadow: 0 2px 8px rgba(0,0,0,0.05);
      display: flex;
      flex-direction: column;
      align-items: center;
      flex-shrink: 0;
    }
    .step-item {
      display: flex;
      flex-direction: column;
      align-items: center;
      margin-bottom: 12px;
    }
    .step-icon {
      font-size: 24px;
      color: #AAA;
    }
    .step-icon.completed { color: #4D5859; }
    .step-icon.active    { color: #489D46; }
    .step-label {
      margin-top: 6px;
      color: #489D46;
      font-weight: bold;
      opacity: 0;
      transition: opacity 0.2s;
    }
    .step-icon.active + .step-label { opacity: 1; }
    .wizard-panel {
      flex: 1;
      background: white;
      border: 1px solid #DDD;
      border-radius: 4px;
      padding: 25px 30px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.05);
    }
    .plot-panel {
      max-width: 700px;
      margin: 20px auto;
    }
    .table-container {
      overflow-x: auto;
      margin-top: 10px;
    }
    .wizard-footer {
      margin-top: 25px;
      text-align: right;
    }
    .wizard-footer .btn {
      min-width: 100px;
    }
    h2 { color: #489D46; margin-bottom: 20px; }
    hr { border-top: 1px solid #EEE; }
  "))),
  
  div(class="wizard-container",
      div(class="sidebar", uiOutput("sidebar_steps")),
      div(class="wizard-panel", uiOutput("wizard_ui"))
  )
)

# 4) SERVER ---------------------------------------------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(
    page        = 1,
    chosen_prob = "Anxiety",
    chosen_int  = "All",
    chosen_age  = "All Ages"
  )
  
  # Sync filters
  observeEvent(input$problem,     rv$chosen_prob <- input$problem,     ignoreNULL=TRUE)
  observeEvent(input$intervention,rv$chosen_int  <- input$intervention,ignoreNULL=TRUE)
  observeEvent(input$age_group,   rv$chosen_age  <- input$age_group,   ignoreNULL=TRUE)
  
  # Sidebar
  output$sidebar_steps <- renderUI({
    steps <- list(
      list(icon="rocket",    label="Start",        idx=1),
      list(icon="lightbulb", label="Problem",      idx=2),
      list(icon="clipboard", label="Intervention", idx=3),
      list(icon="users",     label="Age Group",    idx=4),
      list(icon="magic",     label="Results",      idx=5)
    )
    tags$div(lapply(steps, function(st) {
      cls <- if      (st$idx < rv$page) "step-icon completed"
      else if (st$idx == rv$page) "step-icon active"
      else                         "step-icon disabled"
      btn <- actionButton(paste0("nav", st$idx), NULL,
                          icon = icon(st$icon, verify_fa=FALSE),
                          class=cls, style="background:none;border:none;padding:0;")
      lbl <- tags$span(class="step-label", st$label)
      tags$div(class="step-item", btn, lbl)
    }))
  })
  
  # Wizard UI pages
  output$wizard_ui <- renderUI({
    switch(as.character(rv$page),
           "1" = div(class="wizard-panel",
                     h2("Welcome"),
                     p("Explore K–12 school‐based mental health prevention evidence."),
                     div(class="wizard-footer",
                         actionButton("to_page2", "Get Started →", class="btn btn-primary")
                     )
           ),
           "2" = div(class="wizard-panel",
                     h2("Step 1: Select Problem Domain(s)"),
                     checkboxGroupInput("problem", NULL,
                                        choices = c("Anxiety","Depression","Suicide","Well-Being","Educational Attainment", "project or outcome level?"),
                                        selected = rv$chosen_prob),
                     div(class="wizard-footer",
                         actionButton("back_to1","← Back", class="btn btn-secondary"),
                         actionButton("to_page3","Proceed →", class="btn btn-primary")
                     )
           ),
           "3" = div(class="wizard-panel",
                     h2("Step 2: Select Intervention Type(s)"),
                     checkboxGroupInput("intervention", NULL,
                                        choices = c("All Interventions"="All","CBT","Mindfulness","SEL","Family","groupings or specific names?"),
                                        selected = rv$chosen_int),
                     div(class="wizard-footer",
                         actionButton("back_to2","← Back", class="btn btn-secondary"),
                         actionButton("to_page4","Proceed →", class="btn btn-primary")
                     )
           ),
           "4" = div(class="wizard-panel",
                     h2("Step 3: Select Age Group(s)"),
                     checkboxGroupInput("age_group", NULL,
                                        choices = c("K-8","9-12","K-12", "All Ages"),
                                        selected = rv$chosen_age),
                     div(class="wizard-footer",
                         actionButton("back_to3","← Back", class="btn btn-secondary"),
                         actionButton("to_page5","Proceed →", class="btn btn-primary")
                     )
           ),
           "5" = div(class="wizard-panel",
                     h2("Results"),
                     p("You selected:"),
                     tags$ul(
                       tags$li(strong("Problem(s): "), paste(rv$chosen_prob, collapse=", "),
                               actionLink("edit_prob", "(Edit)")),
                       tags$li(strong("Intervention(s): "), paste(rv$chosen_int, collapse=", "),
                               actionLink("edit_int", "(Edit)")),
                       tags$li(strong("Age Group(s): "), paste(rv$chosen_age, collapse=", "),
                               actionLink("edit_age", "(Edit)"))
                     ), hr(),
                     uiOutput("results_text"),
                     div(class="plot-panel", plotOutput("results_jitter", height="300px")),
                     div(class="plot-panel", plotOutput("results_density", height="150px")), hr(),
                     h4("Filtered Data"),
                     div(class="table-container", tableOutput("filtered_table")),
                     div(class="wizard-footer",
                         actionButton("back_to4","← Back", class="btn btn-secondary"),
                         actionButton("start_over","Start Over", class="btn",
                                      style="background-color:#8D1D58;color:white;")
                     )
           )
    )
  })
  
  # Navigation observers
  observeEvent(input$to_page2,    rv$page <- 2)
  observeEvent(input$back_to1,    rv$page <- 1)
  observeEvent(input$to_page3,    rv$page <- 3)
  observeEvent(input$back_to2,    rv$page <- 2)
  observeEvent(input$to_page4,    rv$page <- 4)
  observeEvent(input$back_to3,    rv$page <- 3)
  observeEvent(input$to_page5,    rv$page <- 5)
  observeEvent(input$back_to4,    rv$page <- 4)
  observeEvent(input$start_over, {
    rv$page        <- 1
    rv$chosen_prob <- "Anxiety"
    rv$chosen_int  <- "All"
    rv$chosen_age  <- "All Ages"
  })
  
  # Sidebar clicks
  observeEvent(input$nav1, rv$page <- 1)
  observeEvent(input$nav2, rv$page <- 2)
  observeEvent(input$nav3, rv$page <- 3)
  observeEvent(input$nav4, rv$page <- 4)
  observeEvent(input$nav5, rv$page <- 5)
  
  # Edit links
  observeEvent(input$edit_prob, rv$page <- 2)
  observeEvent(input$edit_int,  rv$page <- 3)
  observeEvent(input$edit_age,  rv$page <- 4)
  
  # Filtered data
  filtered_df <- reactive({
    df <- dpo_app[dpo_app$problem %in% rv$chosen_prob, ]
    if (!("All" %in% rv$chosen_int))    df <- df[df$intervention %in% rv$chosen_int, ]
    if (!("All Ages" %in% rv$chosen_age)) df <- df[df$age_group %in% rv$chosen_age, ]
    df
  })
  
  output$filtered_table <- renderTable({
    df <- filtered_df()
    if (nrow(df)==0) return(data.frame(Note="No matching rows"))
    df %>% select(study_name, problem, intervention, outcome_measure, outcome_timepoint, #age_group,
                  mean_effect, x95_lower, x95_upper, sample_size) %>% 
      arrange(study_name, intervention, outcome_measure, outcome_timepoint)
  })
  
  output$results_text <- renderUI({
    df <- filtered_df()
    
    # counts
    n_eff     <- nrow(df)                        # effect sizes
    n_studies <- dplyr::n_distinct(df$refid)     # unique studies (use refid)
    
    # study-level participants (avoid double counting across multiple effects)
    by_study <- df %>%
      dplyr::group_by(refid) %>%
      dplyr::summarise(study_n = suppressWarnings(max(sample_size, na.rm = TRUE)),
                       .groups = "drop")
    by_study$study_n[!is.finite(by_study$study_n)] <- NA
    tot_study_n <- sum(by_study$study_n, na.rm = TRUE)
    
    # weighted mean (effect-size level, as before)
    tot_w <- sum(df$sample_size, na.rm = TRUE)
    avg <- if (n_eff > 0 && tot_w > 0) {
      round(sum(df$mean_effect * df$sample_size, na.rm = TRUE) / tot_w, 2)
    } else if (n_eff > 0) {
      round(mean(df$mean_effect, na.rm = TRUE), 2)
    } else NA_real_
    
    HTML(sprintf(
      "<p><em>Across <strong>%d</strong> effect sizes from <strong>%d</strong> unique studies (N = <strong>%s</strong> participants), weighted mean effect size = <strong>%s</strong>.</em></p>",
      n_eff,
      n_studies,
      ifelse(tot_study_n == 0, "—", format(tot_study_n, big.mark=",")),
      ifelse(is.na(avg), "—", sprintf("%.2f", avg))
    ))
  })
  
  output$results_jitter <- renderPlot({
    df <- filtered_df()
    if (nrow(df)==0) return(NULL)
    plot_jitter(df)
  })
  
  output$results_density <- renderPlot({
    df <- filtered_df()
    if (nrow(df) < 2) return(NULL)
    plot_overall_jitter(df)
  }, height = 180)
  
}

# 5) RUN APP --------------------------------------------------------------

shinyApp(ui, server)
