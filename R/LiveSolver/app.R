#####################################################
### OptimalEcospace Live Solver (Shiny app)       ###
###                                               ###
### Purpose: Pick an existing Ecospace model,     ###
###  configure an artificial-structure            ###
###  installation and/or decommissioning          ###
###  strategy, and watch the solver iterate       ###
###  Ecospace scenarios live.                     ###
###                                               ###
### Launch from the repository root:              ###
###   shiny::runApp("R/LiveSolver")               ###
#####################################################

# Load libraries and dependencies ----
library(shiny)
library(bslib)
library(DT)
library(processx)
source("solver_engine.R", local = TRUE)
options(scipen = 10)

repo_root <- normalizePath(Sys.getenv("OPTIMAL_ECOSPACE_ROOT", "../.."), winslash = "/", mustWork = TRUE)
models    <- ls_discover_models(repo_root)
models$key <- paste(models$region, models$model_name, sep = " / ")
defaults  <- ls_default_config(repo_root)

# UI helpers ----

num <- function(id, label, value, ...) numericInput(id, label, if (is.null(value) || is.na(value)) NA else value, ...)
lim_note <- function() helpText("Leave a limit blank for no limit.")

layout_choices <- c(
  "Random"                       = "random",
  "Clustered around random hubs" = "clustered",
  "Prefer cheaper cells"         = "cost_weighted",
  "Prefer deeper cells"          = "depth_weighted"
)

ui <- page_navbar(
  title = "OptimalEcospace Live Solver",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  id = "nav",
  fillable = FALSE,

  # Start tab ----
  nav_panel("Start here",
    layout_columns(col_widths = c(7, 5),
      card(
        card_header("What this tool does"),
        p("The Live Solver tests where artificial structures (such as artificial reefs) could be installed,",
          "or which existing structures could be removed, and how each choice changes the ecosystem",
          "simulated by Ecospace."),
        p("You set the rules (budget, how many structures, where they are allowed, what it costs).",
          "The solver then builds many scenarios that follow those rules, runs Ecospace for each one,",
          "and ranks them by the outcomes you care about (for example red snapper biomass and catch)."),
        h5("How to play"),
        tags$ol(
          tags$li(strong("Setup tab → Strategy:"), " install new structures, remove existing ones, or both."),
          tags$li(strong("Adjust the constraints."), " The map on the right updates to show where structures can go.",
                  " Press ", em("Sample a candidate"), " to see one example scenario."),
          tags$li(strong("Choose how many scenarios to run"), " and press ", em("Start solver"), "."),
          tags$li(strong("Live solver tab:"), " watch scores and maps update after every scenario."),
          tags$li(strong("Results tab:"), " compare scenarios with the status quo, click any scenario to see its map,",
                  " and download everything as a spreadsheet.")
        ),
        p(class = "text-muted small",
          "Every scenario is saved in the Outputs folder, so nothing is lost when you close the app.")
      ),
      card(
        card_header("Ecospace console"),
        textInput("console_path", "Location of EwEClientConsole.exe", defaults$model$console_path, width = "100%"),
        uiOutput("console_status"),
        p(class = "small",
          "Real scenarios need the Ecospace console installed on this Windows computer. Without it you can",
          "still try everything in ", strong("test mode"), ", which uses made-up scores so you can learn the tool."),
        div(class = "d-flex gap-2 flex-wrap",
          actionButton("goto_setup", "Set up scenarios", class = "btn-primary"),
          actionButton("quick_test", "Quick test (20 scenarios, test mode)", class = "btn-outline-secondary"))
      )
    )
  ),

  # Setup tab ----
  nav_panel("Setup",
    layout_columns(col_widths = c(5, 7),
      accordion(id = "setup_acc", open = c("Model & run"),
        accordion_panel("Model & run",
          selectInput("model", "Ecospace model", choices = models$key, selected = models$key[1]),
          layout_columns(
            num("ecosim_scenario", "Ecosim scenario index", defaults$model$ecosim_scenario, min = 1, step = 1),
            num("ecospace_scenario", "Ecospace scenario index", defaults$model$ecospace_scenario, min = 1, step = 1)
          ),
          layout_columns(
            num("n_years", "Run length (years)", defaults$model$n_years, min = 1, step = 1),
            num("timesteps_year", "Time steps / year", defaults$model$timesteps_year, min = 1, step = 1)
          ),
          layout_columns(
            checkboxInput("use_spinup", "Use spin-up", defaults$model$use_spinup),
            num("spinup_years", "Spin-up (years)", defaults$model$spinup_years, min = 0, step = 1)
          ),
          layout_columns(
            num("n_threads", "Threads", defaults$model$n_threads, min = 1, step = 1),
            num("timeout_sec", "Run timeout (s, 0 = none)", defaults$model$timeout_sec, min = 0, step = 60)
          ),
          selectInput("structure_layer", "Artificial structure layer (from STConfig)", choices = NULL),
          selectInput("natural_habitats", "Natural habitat layers displaced / restored", choices = NULL, multiple = TRUE),
          selectInput("basemap_file", "Basemap (depth)", choices = NULL),
          selectInput("existing_file", "Existing structures map", choices = NULL),
          radioButtons("pristine_source", "Pristine habitat maps", inline = TRUE,
                       c("Current Inputs files" = "inputs", "Backup_Base_Maps folder" = "backup")),
          textAreaInput("extra_tags", "Extra command-file lines (advanced)", defaults$model$extra_tags, rows = 3,
                        placeholder = "<ECOSIM_VULNERABILITIES_INDEXED>(1),2.5, Indexed.Single")
        ),

        accordion_panel("Strategy",
          radioButtons("strategy_type", "Strategy", choices = c(
            "Install new structures" = "install",
            "Decommission existing structures" = "decommission",
            "Mixed: decommission and install" = "mixed")),
          conditionalPanel("input.strategy_type == 'mixed'",
            checkboxInput("shared_budget", "Shared budget (removals are paid from the installation budget)", FALSE))
        ),

        accordion_panel("Installation constraints",
          conditionalPanel("input.strategy_type == 'decommission'", helpText("Not used by the selected strategy.")),
          h6("Budget & quantity"),
          num("inst_budget", "Budget (USD)", defaults$install$budget, min = 0, step = 50000),
          checkboxInput("inst_fill_budget", "Fill the budget (otherwise a random count between min and max)", defaults$install$fill_budget),
          layout_columns(
            num("inst_min_n", "Min new structures", defaults$install$min_n, min = 0, step = 1),
            num("inst_max_n", "Max new structures", defaults$install$max_n, min = 0, step = 1)
          ),
          lim_note(),
          sliderInput("inst_coverage", "Cell coverage of one structure (fraction)", 0.05, 1, defaults$install$coverage_fraction, 0.05),
          h6("Cost"),
          radioButtons("inst_cost_source", NULL, inline = TRUE,
                       c("Cost map" = "map", "Depth-based" = "depth", "Fixed only" = "fixed")),
          conditionalPanel("input.inst_cost_source == 'map'",
            layout_columns(selectInput("inst_cost_map", "Cost map", choices = NULL),
                           num("inst_cost_mult", "Multiplier", defaults$install$cost_multiplier, min = 0, step = 0.1))),
          conditionalPanel("input.inst_cost_source == 'depth'",
            num("inst_depth_rate", "USD per metre depth", defaults$install$depth_cost_rate, min = 0)),
          num("inst_fixed_cost", "Fixed cost per structure (USD, added)", defaults$install$fixed_cost, min = 0),
          checkboxInput("inst_scale_cov", "Scale cost by coverage fraction", FALSE),
          h6("Where structures may go"),
          checkboxInput("inst_use_excl", "Respect exclusions map", TRUE),
          conditionalPanel("input.inst_use_excl", selectInput("inst_excl_file", "Exclusions map", choices = NULL)),
          selectInput("inst_mask_file", "Additional allowed-area mask (cells > 0)", choices = NULL),
          layout_columns(
            num("inst_depth_min", "Min depth (m)", defaults$install$depth_min, min = 0),
            num("inst_depth_max", "Max depth (m)", defaults$install$depth_max, min = 0)
          ),
          layout_columns(
            num("inst_xmin", "West lon", NA), num("inst_xmax", "East lon", NA),
            num("inst_ymin", "South lat", NA), num("inst_ymax", "North lat", NA)
          ),
          layout_columns(
            num("inst_spacing", "Min spacing between new (cells)", 0, min = 0, step = 1),
            num("inst_buffer", "Buffer from existing (cells)", 0, min = 0, step = 1)
          ),
          selectInput("inst_avoid", "Avoid cells rich in these natural habitats", choices = NULL, multiple = TRUE),
          conditionalPanel("input.inst_avoid && input.inst_avoid.length > 0",
            sliderInput("inst_avoid_thr", "...when their combined fraction exceeds", 0, 1, 0.5, 0.05)),
          h6("Placement & habitat effects"),
          selectInput("inst_placement", "Placement pattern", layout_choices),
          conditionalPanel("input.inst_placement == 'clustered'",
            layout_columns(num("inst_n_clusters", "Hubs", 3, min = 1, step = 1),
                           num("inst_cluster_radius", "Hub radius (cells)", 3, min = 0, step = 1))),
          selectInput("inst_displacement", "Natural habitat under new structures", c(
            "Reduced proportionally" = "proportional",
            "Each reduced by the added coverage (legacy)" = "subtract",
            "Left unchanged" = "none"))
        ),

        accordion_panel("Decommissioning constraints",
          conditionalPanel("input.strategy_type == 'install'", helpText("Not used by the selected strategy.")),
          h6("Budget & quantity"),
          num("dec_budget", "Removal budget (USD)", defaults$decom$budget, min = 0, step = 50000),
          layout_columns(
            num("dec_min_n", "Min removals", defaults$decom$min_n, min = 0, step = 1),
            num("dec_max_n", "Max removals", defaults$decom$max_n, min = 0, step = 1)
          ),
          num("dec_min_retained", "Min structures retained", defaults$decom$min_retained, min = 0, step = 1),
          lim_note(),
          sliderInput("dec_removal_fraction", "Fraction of each structure removed (1 = full, < 1 = partial / toppling)",
                      0.05, 1, defaults$decom$removal_fraction, 0.05),
          num("dec_threshold", "Only structures with coverage above", defaults$decom$eligible_threshold, min = 0, max = 1, step = 0.05),
          h6("Cost"),
          radioButtons("dec_cost_source", NULL, inline = TRUE,
                       c("Fixed only" = "fixed", "Depth-based" = "depth", "Cost map" = "map")),
          conditionalPanel("input.dec_cost_source == 'map'",
            layout_columns(selectInput("dec_cost_map", "Cost map", choices = NULL),
                           num("dec_cost_mult", "Multiplier", 1, min = 0, step = 0.1))),
          conditionalPanel("input.dec_cost_source == 'depth'",
            num("dec_depth_rate", "USD per metre depth", defaults$decom$depth_cost_rate, min = 0)),
          num("dec_fixed_cost", "Fixed cost per removal (USD, added)", defaults$decom$fixed_cost, min = 0),
          h6("Which structures"),
          layout_columns(
            num("dec_depth_min", "Min depth (m)", NA, min = 0),
            num("dec_depth_max", "Max depth (m)", NA, min = 0)
          ),
          layout_columns(
            num("dec_xmin", "West lon", NA), num("dec_xmax", "East lon", NA),
            num("dec_ymin", "South lat", NA), num("dec_ymax", "North lat", NA)
          ),
          selectInput("dec_selection", "Selection preference", c(
            "Random" = "random", "Prefer cheapest removal" = "cheapest", "Prefer costliest removal" = "costliest",
            "Prefer shallowest" = "shallowest", "Prefer deepest" = "deepest")),
          selectInput("dec_restore", "Freed area becomes", choices = c("Natural habitats, proportionally" = "proportional",
                                                                        "Nothing (left empty)" = "none"))
        ),

        accordion_panel("Objectives",
          helpText("Double-click a cell to edit. type: species | sum | biodiversity; direction: max | min.",
                   "file_pattern matches the Ecospace output CSV name."),
          DTOutput("obj_table"),
          div(class = "mt-2",
            actionButton("obj_add", "Add objective", class = "btn-sm"),
            actionButton("obj_del", "Delete selected", class = "btn-sm"),
            actionButton("obj_reset", "Reset defaults", class = "btn-sm")),
          layout_columns(
            num("eval_last_n", "Average over last N output rows", 1, min = 1, step = 1),
            selectInput("roi_objective", "ROI objective (per USD)", choices = defaults$objectives$name,
                        selected = defaults$search$roi_objective)
          )
        ),

        accordion_panel("Search",
          layout_columns(
            num("seed", "Random seed (blank = random)", NA, step = 1)
          ),
          checkboxInput("include_baselines", "Add the two baseline runs (no change / everything eligible)", TRUE),
          radioButtons("algorithm", "Search algorithm", c(
            "Random search" = "random",
            "Evolutionary (mutate the best run)" = "evolutionary")),
          conditionalPanel("input.algorithm == 'evolutionary'",
            layout_columns(
              num("warmup", "Random warm-up runs", defaults$search$warmup, min = 0, step = 1),
              sliderInput("mutation_rate", "Mutation rate", 0.01, 0.9, defaults$search$mutation_rate, 0.01))),
          radioButtons("map_mode", "How Ecospace receives the maps", c(
            "Overwrite Inputs maps each run (restored afterwards)" = "overwrite",
            "Per-run STConfig pointing at the run's maps" = "isolated")),
        ),

        accordion_panel("Save / load configuration",
          downloadButton("cfg_download", "Download configuration (.json)", class = "btn-sm"),
          fileInput("cfg_upload", "Load configuration", accept = ".json")
        )
      ),

      card(
        card_header(class = "d-flex justify-content-between align-items-center",
          "Constraint preview",
          div(actionButton("sample_btn", "Sample a candidate", class = "btn-sm btn-outline-primary"),
              actionButton("start_btn", "Start solver", class = "btn-sm btn-success"))),
        card_body(fillable = FALSE,
          div(class = "d-flex gap-4 align-items-end flex-wrap",
            num("iterations", "Number of scenarios to run", defaults$search$iterations, min = 1, step = 1, width = "220px"),
            checkboxInput("dry_run", "Test mode (no Ecospace; made-up scores)", defaults$search$dry_run)),
          uiOutput("preview_error"),
          plotOutput("preview_plot", height = "430px"),
          tableOutput("preview_summary"),
          uiOutput("sample_summary")
        )
      )
    )
  ),

  # Live tab ----
  nav_panel("Live solver",
    layout_columns(col_widths = c(3, 3, 3, 3),
      value_box("Status", textOutput("vb_status"), textOutput("vb_mode"), theme = "primary"),
      value_box("Progress", textOutput("vb_progress"), textOutput("vb_elapsed"), theme = "secondary"),
      value_box("Best composite score", textOutput("vb_best"), textOutput("vb_best_run"), theme = "success"),
      value_box("Best run cost", textOutput("vb_cost"), textOutput("vb_best_n"), theme = "info")
    ),
    div(class = "mb-2",
      actionButton("pause_btn", "Pause after this run", class = "btn-warning btn-sm"),
      actionButton("resume_btn", "Resume", class = "btn-success btn-sm"),
      actionButton("stop_btn", "Stop & restore", class = "btn-danger btn-sm"),
      span(class = "ms-3", numericInput("extend_n", NULL, 25, min = 1, width = "90px") |> tagAppendAttributes(style = "display:inline-block")),
      actionButton("extend_btn", "Add iterations", class = "btn-sm"),
      actionButton("rescore_btn", "Re-score with current objective weights", class = "btn-sm")
    ),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Composite score by run"), plotOutput("trace_plot", height = "300px")),
      card(card_header("Cost vs. score (frontier highlighted)"), plotOutput("frontier_plot", height = "300px")),
      card(card_header(textOutput("best_title")), plotOutput("best_map", height = "360px")),
      card(card_header(textOutput("current_title")), plotOutput("current_map", height = "360px"))
    ),
    card(card_header("Solver log"), verbatimTextOutput("log_text"), max_height = "260px")
  ),

  # Results tab ----
  nav_panel("Results",
    div(class = "mb-2",
      downloadButton("res_download", "Download results (.csv)", class = "btn-sm"),
      textOutput("session_path", inline = TRUE)),
    layout_columns(col_widths = c(7, 5),
      card(
        card_header(class = "d-flex justify-content-between align-items-center",
          "Top scenarios: change from the status quo",
          sliderInput("top_n", NULL, 3, 15, 8, step = 1, width = "200px")),
        plotOutput("compare_plot", height = "520px"),
        p(class = "small text-muted px-3",
          "Bars show the percent change of each outcome compared with the no-change baseline.",
          "Blue = better (in the direction you chose for that outcome), orange = worse.")
      ),
      card(
        card_header(textOutput("detail_title")),
        plotOutput("detail_map", height = "320px"),
        tableOutput("detail_table")
      )
    ),
    card(card_header("All scenarios (click a row to inspect it)"), DTOutput("results_table"))
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(S = NULL, job = NULL, state = "idle", tick = 0, sample = NULL, pending = NULL,
                       selected_run = NULL, autostart = FALSE)
  objectives <- reactiveVal(defaults$objectives)

  model_row <- reactive({
    req(input$model)
    models[models$key == input$model, ]
  })
  region <- reactive(ls_region_info(model_row()$inputs_dir))

  output$console_status <- renderUI({
    if (file.exists(input$console_path)) {
      div(class = "alert alert-success py-2", icon("check"), "Console found. Real Ecospace scenarios are available.")
    } else {
      div(class = "alert alert-warning py-2", icon("triangle-exclamation"),
          "Console not found at this location. Only test mode is available until you enter the correct path.")
    }
  })

  # Model change -> refresh layer and map choices ----
  observeEvent(region(), {
    info <- region()
    pc   <- rv$pending
    lyr  <- info$layers$name
    asc  <- info$asc_files
    pick <- function(val, choices, fallback) if (!is.null(val) && length(val) && all(val %in% choices)) val else fallback
    struct <- pick(pc$layers$structure_layer, lyr, {
      h <- grep("struct|reef", lyr, ignore.case = TRUE, value = TRUE); if (length(h)) h[1] else lyr[1]
    })
    natural <- setdiff(lyr, struct)
    updateSelectInput(session, "structure_layer", choices = lyr, selected = struct)
    updateSelectInput(session, "natural_habitats", choices = natural, selected = pick(pc$layers$natural_habitats, natural, natural))
    updateSelectInput(session, "basemap_file", choices = asc, selected = pick(pc$layers$basemap_file, asc, if ("basemap.asc" %in% asc) "basemap.asc" else asc[1]))
    updateSelectInput(session, "existing_file", choices = c("Same as structure layer" = "", asc), selected = pick(pc$layers$existing_file, c("", asc), ""))
    cm <- if ("cost_map.asc" %in% asc) "cost_map.asc" else asc[1]
    updateSelectInput(session, "inst_cost_map", choices = asc, selected = pick(pc$install$cost_map_file, asc, cm))
    updateSelectInput(session, "dec_cost_map", choices = asc, selected = pick(pc$decom$cost_map_file, asc, cm))
    updateSelectInput(session, "inst_excl_file", choices = asc, selected = pick(pc$install$exclusions_file, asc, if ("exclusions.asc" %in% asc) "exclusions.asc" else asc[1]))
    updateSelectInput(session, "inst_mask_file", choices = c("None" = "", asc), selected = pick(pc$install$extra_mask_file, c("", asc), ""))
    updateSelectInput(session, "inst_avoid", choices = natural, selected = pick(pc$install$avoid_habitats, natural, character()))
    updateSelectInput(session, "dec_restore", choices = c("Natural habitats, proportionally" = "proportional",
                                                           "Nothing (left empty)" = "none",
                                                           setNames(natural, paste("All to", natural))),
                      selected = pick(pc$decom$restore, c("proportional", "none", natural), "proportional"))
    if (!is.null(pc)) {
      push_scalar_inputs(pc)
      rv$pending <- NULL
    }
    if (info$locked) showNotification("A previous solver session did not finish cleanly. Its pristine maps will be restored when the next session starts.", type = "warning", duration = 12)
  })

  observeEvent(input$structure_layer, {
    lyr <- region()$layers$name
    natural <- setdiff(lyr, input$structure_layer)
    updateSelectInput(session, "natural_habitats", choices = natural, selected = {
      keep <- intersect(isolate(input$natural_habitats), natural)
      if (length(keep)) keep else natural
    })
  }, ignoreInit = TRUE)

  # Objectives editor ----
  output$obj_table <- renderDT({
    datatable(objectives(), editable = "cell", rownames = FALSE, selection = "single",
              options = list(dom = "t", ordering = FALSE, pageLength = 50, scrollX = TRUE))
  })
  observeEvent(input$obj_table_cell_edit, {
    info <- input$obj_table_cell_edit
    df <- objectives()
    col <- names(df)[info$col + 1]
    val <- info$value
    if (col == "weight") val <- ls_num(val, 1)
    if (col == "type" && !val %in% c("species", "sum", "biodiversity")) { showNotification("type must be species, sum or biodiversity", type = "error"); val <- df[info$row, col] }
    if (col == "direction" && !val %in% c("max", "min")) { showNotification("direction must be max or min", type = "error"); val <- df[info$row, col] }
    if (col == "target" && !nzchar(trimws(val))) val <- NA
    df[info$row, col] <- val
    objectives(df)
  })
  observeEvent(input$obj_add, {
    df <- objectives()
    objectives(rbind(df, data.frame(name = paste0("Objective_", nrow(df) + 1), file_pattern = "Annual_Average_Biomass",
                                    target = NA, type = "sum", direction = "max", weight = 1)))
  })
  observeEvent(input$obj_del, {
    s <- input$obj_table_rows_selected
    if (length(s)) objectives(objectives()[-s, , drop = FALSE])
  })
  observeEvent(input$obj_reset, objectives(ls_default_objectives()))
  observeEvent(objectives(), {
    updateSelectInput(session, "roi_objective", choices = objectives()$name,
                      selected = if (isTRUE(isolate(input$roi_objective) %in% objectives()$name)) isolate(input$roi_objective) else objectives()$name[1])
  })

  # Inputs -> configuration ----
  config <- reactive({
    m <- model_row(); info <- region()
    list(
      model = list(
        region = m$region, model_file = m$model_file, inputs_dir = m$inputs_dir,
        stconfig = info$stconfig, cmd_template = info$cmd_template, console_path = input$console_path,
        ecosim_scenario = input$ecosim_scenario, ecospace_scenario = input$ecospace_scenario,
        n_years = input$n_years, use_spinup = input$use_spinup, spinup_years = input$spinup_years,
        timesteps_year = input$timesteps_year, n_threads = input$n_threads, timeout_sec = input$timeout_sec,
        extra_tags = input$extra_tags
      ),
      layers = list(
        structure_layer = input$structure_layer, existing_file = input$existing_file,
        natural_habitats = input$natural_habitats %||% character(), basemap_file = input$basemap_file,
        pristine_source = input$pristine_source
      ),
      strategy = list(type = input$strategy_type, shared_budget = input$shared_budget),
      install = list(
        budget = input$inst_budget, fill_budget = input$inst_fill_budget,
        min_n = input$inst_min_n, max_n = input$inst_max_n, coverage_fraction = input$inst_coverage,
        cost_source = input$inst_cost_source, cost_map_file = input$inst_cost_map, cost_multiplier = input$inst_cost_mult,
        fixed_cost = input$inst_fixed_cost, depth_cost_rate = input$inst_depth_rate,
        scale_cost_by_coverage = input$inst_scale_cov,
        depth_min = input$inst_depth_min, depth_max = input$inst_depth_max,
        use_exclusions = input$inst_use_excl, exclusions_file = input$inst_excl_file,
        extra_mask_file = input$inst_mask_file,
        xmin = input$inst_xmin, xmax = input$inst_xmax, ymin = input$inst_ymin, ymax = input$inst_ymax,
        min_spacing = input$inst_spacing, buffer_existing = input$inst_buffer,
        avoid_habitats = input$inst_avoid %||% character(), avoid_threshold = input$inst_avoid_thr,
        placement = input$inst_placement, n_clusters = input$inst_n_clusters, cluster_radius = input$inst_cluster_radius,
        displacement = input$inst_displacement
      ),
      decom = list(
        budget = input$dec_budget, min_n = input$dec_min_n, max_n = input$dec_max_n,
        min_retained = input$dec_min_retained, eligible_threshold = input$dec_threshold,
        removal_fraction = input$dec_removal_fraction,
        cost_source = input$dec_cost_source, cost_map_file = input$dec_cost_map, cost_multiplier = input$dec_cost_mult,
        fixed_cost = input$dec_fixed_cost, depth_cost_rate = input$dec_depth_rate,
        depth_min = input$dec_depth_min, depth_max = input$dec_depth_max,
        xmin = input$dec_xmin, xmax = input$dec_xmax, ymin = input$dec_ymin, ymax = input$dec_ymax,
        selection = input$dec_selection, restore = input$dec_restore
      ),
      search = list(
        iterations = input$iterations, algorithm = input$algorithm, warmup = input$warmup,
        mutation_rate = input$mutation_rate, include_baselines = input$include_baselines,
        seed = input$seed, map_mode = input$map_mode, dry_run = input$dry_run,
        eval_last_n = input$eval_last_n, roi_objective = input$roi_objective, max_retries = 50
      ),
      objectives = objectives()
    )
  })
  config_d <- debounce(config, 600)

  # Configuration -> inputs (for loading a saved .json) ----
  push_scalar_inputs <- function(cfg) {
    M <- cfg$model; I <- cfg$install; D <- cfg$decom; Se <- cfg$search
    nums <- c(ecosim_scenario = M$ecosim_scenario, ecospace_scenario = M$ecospace_scenario, n_years = M$n_years,
              spinup_years = M$spinup_years, timesteps_year = M$timesteps_year, n_threads = M$n_threads,
              timeout_sec = M$timeout_sec,
              inst_budget = I$budget, inst_min_n = I$min_n, inst_max_n = I$max_n, inst_cost_mult = I$cost_multiplier,
              inst_depth_rate = I$depth_cost_rate, inst_fixed_cost = I$fixed_cost, inst_depth_min = I$depth_min,
              inst_depth_max = I$depth_max, inst_xmin = I$xmin, inst_xmax = I$xmax, inst_ymin = I$ymin, inst_ymax = I$ymax,
              inst_spacing = I$min_spacing, inst_buffer = I$buffer_existing, inst_n_clusters = I$n_clusters,
              inst_cluster_radius = I$cluster_radius,
              dec_budget = D$budget, dec_min_n = D$min_n, dec_max_n = D$max_n, dec_min_retained = D$min_retained,
              dec_threshold = D$eligible_threshold, dec_cost_mult = D$cost_multiplier, dec_depth_rate = D$depth_cost_rate,
              dec_fixed_cost = D$fixed_cost, dec_depth_min = D$depth_min, dec_depth_max = D$depth_max,
              dec_xmin = D$xmin, dec_xmax = D$xmax, dec_ymin = D$ymin, dec_ymax = D$ymax,
              iterations = Se$iterations, seed = Se$seed, warmup = Se$warmup, eval_last_n = Se$eval_last_n)
    for (id in names(nums)) updateNumericInput(session, id, value = ls_num(nums[[id]]))
    sliders <- c(inst_coverage = I$coverage_fraction, inst_avoid_thr = I$avoid_threshold,
                 dec_removal_fraction = D$removal_fraction, mutation_rate = Se$mutation_rate)
    for (id in names(sliders)) updateSliderInput(session, id, value = ls_num(sliders[[id]]))
    checks <- c(use_spinup = M$use_spinup, shared_budget = cfg$strategy$shared_budget, inst_fill_budget = I$fill_budget,
                inst_scale_cov = I$scale_cost_by_coverage, inst_use_excl = I$use_exclusions,
                include_baselines = Se$include_baselines, dry_run = Se$dry_run)
    for (id in names(checks)) updateCheckboxInput(session, id, value = isTRUE(checks[[id]]))
    radios <- c(pristine_source = cfg$layers$pristine_source, strategy_type = cfg$strategy$type,
                inst_cost_source = I$cost_source, dec_cost_source = D$cost_source,
                algorithm = Se$algorithm, map_mode = Se$map_mode)
    for (id in names(radios)) updateRadioButtons(session, id, selected = radios[[id]])
    updateSelectInput(session, "inst_placement", selected = I$placement)
    updateSelectInput(session, "inst_displacement", selected = I$displacement)
    updateSelectInput(session, "dec_selection", selected = D$selection)
    updateTextInput(session, "console_path", value = M$console_path)
    updateTextAreaInput(session, "extra_tags", value = M$extra_tags %||% "")
    objectives(cfg$objectives)
    updateSelectInput(session, "roi_objective", choices = cfg$objectives$name, selected = Se$roi_objective)
  }

  observeEvent(input$cfg_upload, {
    cfg <- tryCatch(ls_load_config(input$cfg_upload$datapath, repo_root), error = function(e) {
      showNotification(paste("Could not read configuration:", conditionMessage(e)), type = "error"); NULL
    })
    req(cfg)
    key <- paste(cfg$model$region, sub("\\.eweaccdb$", "", basename(cfg$model$model_file)), sep = " / ")
    if (!key %in% models$key) {
      showNotification(paste("Model", key, "is not in this repository; keeping the current model."), type = "warning")
      key <- input$model
    }
    rv$pending <- cfg
    if (identical(key, input$model)) {
      # region() will not change, so re-fire its observer manually via the pending config
      info <- region()
      updateSelectInput(session, "structure_layer", selected = cfg$layers$structure_layer)
      updateSelectInput(session, "natural_habitats", selected = cfg$layers$natural_habitats)
      updateSelectInput(session, "basemap_file", selected = cfg$layers$basemap_file)
      updateSelectInput(session, "existing_file", selected = cfg$layers$existing_file)
      updateSelectInput(session, "inst_cost_map", selected = cfg$install$cost_map_file)
      updateSelectInput(session, "dec_cost_map", selected = cfg$decom$cost_map_file)
      updateSelectInput(session, "inst_excl_file", selected = cfg$install$exclusions_file)
      updateSelectInput(session, "inst_mask_file", selected = cfg$install$extra_mask_file)
      updateSelectInput(session, "inst_avoid", selected = cfg$install$avoid_habitats)
      updateSelectInput(session, "dec_restore", selected = cfg$decom$restore)
      push_scalar_inputs(cfg)
      rv$pending <- NULL
    } else {
      updateSelectInput(session, "model", selected = key)
    }
    showNotification("Configuration loaded.", type = "message")
  })

  output$cfg_download <- downloadHandler(
    filename = function() paste0("livesolver_", model_row()$region, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".json"),
    content = function(file) ls_save_config(config(), file)
  )

  # Constraint preview ----
  preview <- reactive({
    cfg <- config_d()
    req(cfg$layers$structure_layer, cfg$layers$basemap_file)
    if (rv$state %in% c("running", "paused") && !isTRUE(cfg$search$dry_run)) return(list(P = rv$S$P, err = NULL))
    tryCatch(list(P = ls_build_problem(cfg, NULL, recover = FALSE), err = NULL),
             error = function(e) list(P = NULL, err = conditionMessage(e)))
  })
  observeEvent(preview(), rv$sample <- NULL)

  output$preview_error <- renderUI({
    e <- preview()$err
    if (!is.null(e)) div(class = "alert alert-danger", e)
  })
  output$preview_plot <- renderPlot({
    P <- preview()$P
    req(P)
    ls_plot_layout(P, rv$sample, if (is.null(rv$sample)) "Eligible cells under current constraints" else "Sampled candidate")
  })
  output$preview_summary <- renderTable(preview()$P$summary, colnames = FALSE, striped = TRUE, spacing = "xs")

  observeEvent(input$sample_btn, {
    P <- preview()$P
    req(P)
    rv$sample <- ls_generate_candidate(P, "random")
  })
  output$sample_summary <- renderUI({
    c <- rv$sample
    req(c)
    div(class = if (c$feasible) "alert alert-success" else "alert alert-warning",
        sprintf("Sample: %d new structures (%s), %d removals (%s). %s",
                length(c$install), ls_usd(c$install_cost),
                length(c$decom), ls_usd(c$decom_cost),
                if (c$feasible) "Feasible." else paste("Infeasible:", c$note)))
  })

  # Live loop: one step per tick so the UI stays responsive ----
  start_session <- function() {
    if (rv$state %in% c("running", "paused", "pausing")) {
      showNotification("A session is already active. Stop it first.", type = "warning"); return()
    }
    cfg <- config()
    if (!isTRUE(cfg$search$dry_run) && !file.exists(cfg$model$console_path)) {
      showNotification("Ecospace console not found. Set its location on the Start here tab, or switch on test mode.", type = "error"); return()
    }
    if (!nrow(cfg$objectives)) { showNotification("Define at least one objective.", type = "error"); return() }
    S <- tryCatch(ls_new_session(cfg, repo_root), error = function(e) {
      showNotification(paste("Could not start:", conditionMessage(e)), type = "error", duration = 15); NULL
    })
    req(S)
    rv$S <- S; rv$job <- NULL; rv$state <- "running"; rv$tick <- rv$tick + 1
    rv$selected_run <- NULL
    nav_select("nav", "Live solver")
  }
  observeEvent(input$start_btn, start_session())

  observeEvent(input$goto_setup, nav_select("nav", "Setup"))
  # Quick test: switch on test mode with 20 scenarios, then start once the inputs have updated
  observeEvent(input$quick_test, {
    updateCheckboxInput(session, "dry_run", value = TRUE)
    updateNumericInput(session, "iterations", value = 20)
    rv$autostart <- TRUE
  })
  observe({
    req(isTRUE(rv$autostart), isTRUE(input$dry_run), identical(as.numeric(input$iterations), 20))
    rv$autostart <- FALSE
    isolate(start_session())
  })

  observe({
    req(rv$state == "running")
    invalidateLater(if (is.null(isolate(rv$job)) || isTRUE(isolate(rv$S$cfg$search$dry_run))) 30 else 1000)
    isolate({
      S <- rv$S
      tryCatch({
        job <- rv$job
        if (is.null(job)) {
          job <- ls_next_job(S)
          if (is.null(job)) {
            ls_close_session(S)
            rv$state <- "finished"
            showNotification("Solver finished.", type = "message")
          } else {
            job <- ls_start_job(S, job)
          }
        } else {
          job <- ls_poll_job(S, job)
        }
        if (!is.null(job) && job$state %in% c("done", "skipped")) {
          ls_finish_job(S, job)
          job <- NULL
        }
        rv$job <- job
        rv$tick <- rv$tick + 1
      }, error = function(e) {
        ls_log(S, paste("ERROR:", conditionMessage(e)))
        ls_close_session(S, rv$job)
        rv$job <- NULL
        rv$state <- "error"
        rv$tick <- rv$tick + 1
        showNotification(paste("Solver stopped:", conditionMessage(e)), type = "error", duration = NULL)
      })
    })
  })

  # Pause: let the current run finish, then hold
  observe({
    req(rv$state == "pausing")
    invalidateLater(1000)
    isolate({
      if (!is.null(rv$job)) {
        rv$job <- ls_poll_job(rv$S, rv$job)
        if (rv$job$state %in% c("done", "skipped")) { ls_finish_job(rv$S, rv$job); rv$job <- NULL }
      }
      if (is.null(rv$job)) rv$state <- "paused"
      rv$tick <- rv$tick + 1
    })
  })
  observeEvent(input$pause_btn, if (rv$state == "running") rv$state <- "pausing")
  observeEvent(input$resume_btn, if (rv$state %in% c("paused", "pausing")) rv$state <- "running")
  observeEvent(input$stop_btn, {
    req(rv$S, rv$state %in% c("running", "paused", "pausing"))
    ls_log(rv$S, "Stopped by user.")
    ls_close_session(rv$S, rv$job)
    rv$job <- NULL; rv$state <- "stopped"; rv$tick <- rv$tick + 1
  })
  observeEvent(input$extend_btn, {
    req(rv$S, !isTRUE(rv$S$closed))
    rv$S$total_runs <- rv$S$total_runs + max(1, round(ls_num(input$extend_n, 1)))
    ls_log(rv$S, sprintf("Extended to %d runs.", rv$S$total_runs))
    rv$tick <- rv$tick + 1
  })
  observeEvent(input$rescore_btn, {
    S <- rv$S
    req(S, !is.null(S$results))
    new <- objectives()
    cur <- S$cfg$objectives
    hit <- match(cur$name, new$name)
    cur$weight[!is.na(hit)]    <- new$weight[hit[!is.na(hit)]]
    cur$direction[!is.na(hit)] <- new$direction[hit[!is.na(hit)]]
    S$cfg$objectives <- cur
    S$P$cfg$objectives <- cur
    S$results <- ls_score(ls_strip_scores(S$results), cur)
    write.csv(S$results, S$results_file, row.names = FALSE)
    ls_log(S, "Re-scored with updated objective weights/directions.")
    rv$tick <- rv$tick + 1
  })

  session$onSessionEnded(function() {
    S <- isolate(rv$S)
    if (!is.null(S) && !isTRUE(S$closed)) ls_close_session(S, isolate(rv$job))
  })

  # Live outputs ----
  results <- reactive({
    rv$tick
    rv$S$results
  })
  # Dry runs finish many runs per second; throttle the heavier displays
  results_view <- throttle(results, 1500)
  best_row <- reactive({
    df <- results()
    if (is.null(df) || !any(is.finite(df$Composite_Score))) return(NULL)
    df[which.max(df$Composite_Score), ]
  })

  output$vb_status <- renderText({
    rv$tick
    c(idle = "Idle", running = "Running", pausing = "Pausing...", paused = "Paused",
      stopped = "Stopped", finished = "Finished", error = "Error")[[rv$state]]
  })
  output$vb_mode <- renderText({
    req(rv$S)
    if (isTRUE(rv$S$cfg$search$dry_run)) "Test mode: made-up scores" else paste("Ecospace:", basename(rv$S$cfg$model$model_file))
  })
  output$vb_progress <- renderText({
    rv$tick
    req(rv$S)
    done <- if (is.null(rv$S$results)) 0 else nrow(rv$S$results)
    sprintf("%d / %d runs", done, rv$S$total_runs)
  })
  output$vb_elapsed <- renderText({
    df <- results()
    req(df)
    per <- mean(df$runtime_sec, na.rm = TRUE)
    left <- max(0, rv$S$total_runs - nrow(df)) * per
    sprintf("~%.0f s per run, ~%.0f min left", per, left / 60)
  })
  output$vb_best <- renderText({ b <- best_row(); if (is.null(b)) "-" else sprintf("%.3f", b$Composite_Score) })
  output$vb_best_run <- renderText({ b <- best_row(); req(b); paste(b$run, if (isTRUE(b$Pareto)) "(on cost frontier)" else "") })
  output$vb_cost <- renderText({ b <- best_row(); if (is.null(b)) "-" else ls_usd(b$total_cost) })
  output$vb_best_n <- renderText({ b <- best_row(); req(b); sprintf("+%d new / -%d removed", b$n_installed, b$n_decommissioned) })

  output$trace_plot <- renderPlot({
    df <- results_view()
    req(df, any(is.finite(df$Composite_Score)))
    par(mar = c(4, 4, 1, 1))
    cols <- c(baseline_none = "#6b6b6b", baseline_all = "#1baf7a", random = "#2a78d6", evolutionary = "#eb6834")
    plot(df$iteration, df$Composite_Score, pch = 19, col = cols[df$kind], xlab = "Run", ylab = "Composite score")
    sc <- df$Composite_Score; sc[is.na(sc)] <- -Inf
    lines(df$iteration, cummax(sc), type = "s", col = "#d62728", lwd = 2)
    legend("bottomright", c(names(cols)[names(cols) %in% df$kind], "best so far"),
           col = c(cols[names(cols) %in% df$kind], "#d62728"), pch = c(rep(19, sum(names(cols) %in% df$kind)), NA),
           lty = c(rep(NA, sum(names(cols) %in% df$kind)), 1), cex = 0.8, bg = "white")
  })
  output$frontier_plot <- renderPlot({
    df <- results_view()
    req(df, any(is.finite(df$Composite_Score)))
    par(mar = c(4, 4, 1, 1))
    plot(df$total_cost / 1e6, df$Composite_Score, pch = 19, col = ifelse(df$Pareto, "#d62728", "#9ecae1"),
         xlab = "Total cost (million USD)", ylab = "Composite score")
    f <- df[df$Pareto, ]
    f <- f[order(f$total_cost), ]
    lines(f$total_cost / 1e6, f$Composite_Score, col = "#d62728", type = "s")
  })
  output$best_title <- renderText({ b <- best_row(); if (is.null(b)) "Best layout" else paste("Best layout:", b$run) })
  output$best_map <- renderPlot({
    b <- best_row()
    req(b)
    ls_plot_layout(rv$S$P, rv$S$cands[[b$run]], show_eligible = FALSE)
  })
  output$current_title <- renderText({
    rv$tick
    if (!is.null(rv$job)) paste("Running:", rv$job$run, sprintf("(%ds)", round(as.numeric(difftime(Sys.time(), rv$job$started, units = "secs")))))
    else if (!is.null(rv$S$results)) paste("Last run:", tail(rv$S$results$run, 1)) else "Current run"
  })
  output$current_map <- renderPlot({
    rv$tick
    S <- rv$S
    req(S, length(S$cands))
    run <- if (!is.null(rv$job)) rv$job$run else tail(names(S$cands), 1)
    ls_plot_layout(S$P, S$cands[[run]], show_eligible = FALSE)
  })
  output$log_text <- renderText({
    rv$tick
    paste(rev(tail(rv$S$log, 200)), collapse = "\n")
  })

  output$session_path <- renderText({ req(rv$S); paste("  Session folder:", rv$S$session_dir) })
  output$results_table <- renderDT({
    df <- results_view()
    req(df)
    df <- df[, setdiff(names(df), c("map_dir", "timestamp")), drop = FALSE]
    num_cols <- names(df)[vapply(df, is.double, logical(1))]
    sel <- if (!is.null(rv$selected_run) && rv$selected_run %in% df$run) which(df$run == rv$selected_run) else NULL
    dt <- datatable(df, rownames = FALSE, filter = "top", selection = list(mode = "single", selected = sel),
                    options = list(pageLength = 25, scrollX = TRUE, order = list(list(which(names(df) == "Composite_Score") - 1, "desc"))))
    round_cols <- intersect(num_cols, c("Composite_Score", "ROI_per_dollar", objectives()$name,
                                        grep("_vs_status_quo_pct$", names(df), value = TRUE)))
    if (length(round_cols)) dt <- formatRound(dt, round_cols, 4)
    dt
  })
  observeEvent(input$results_table_rows_selected, {
    df <- isolate(results_view())
    i <- input$results_table_rows_selected
    if (length(i) && !is.null(df)) rv$selected_run <- df$run[i]
  })

  # Selected scenario (defaults to the best one)
  detail_row <- reactive({
    df <- results_view()
    req(df)
    if (!is.null(rv$selected_run) && rv$selected_run %in% df$run) return(df[df$run == rv$selected_run, ])
    b <- best_row()
    if (is.null(b)) df[nrow(df), ] else b
  })
  output$detail_title <- renderText({
    r <- detail_row()
    sprintf("Scenario %s: +%d new, -%d removed, %s", r$run, r$n_installed, r$n_decommissioned, ls_usd(r$total_cost))
  })
  output$detail_map <- renderPlot({
    r <- detail_row()
    ls_plot_layout(rv$S$P, rv$S$cands[[r$run]], show_eligible = FALSE)
  })
  output$detail_table <- renderTable({
    r <- detail_row()
    obj <- rv$S$cfg$objectives
    data.frame(
      Outcome = obj$name,
      Goal = ifelse(obj$direction == "max", "higher is better", "lower is better"),
      Value = vapply(obj$name, function(n) if (is.null(r[[n]])) NA_real_ else as.numeric(r[[n]]), numeric(1)),
      `Change vs status quo (%)` = vapply(obj$name, function(n) {
        v <- r[[paste0(n, "_vs_status_quo_pct")]]; if (is.null(v)) NA_real_ else as.numeric(v)
      }, numeric(1)),
      check.names = FALSE
    )
  }, digits = 3, na = "-", striped = TRUE, spacing = "xs")

  # Small multiples: one panel per objective, top-N scenarios as horizontal bars
  output$compare_plot <- renderPlot({
    df <- results_view()
    req(df, any(is.finite(df$Composite_Score)))
    obj <- rv$S$cfg$objectives
    pct_cols <- paste0(obj$name, "_vs_status_quo_pct")
    shiny::validate(shiny::need(any(df$kind == "baseline_none" & df$finished %in% TRUE),
                  "The comparison needs the no-change baseline run (Search → add the two baseline runs)."))
    top <- df[df$kind != "baseline_none" & is.finite(df$Composite_Score), ]
    top <- head(top[order(-top$Composite_Score), ], input$top_n)
    req(nrow(top))
    k <- nrow(obj)
    nc <- min(3, k)
    op <- par(mfrow = c(ceiling(k / nc), nc), mar = c(3.2, 6.5, 3.2, 1), mgp = c(1.9, 0.5, 0), las = 1)
    on.exit(par(op))
    par(cex = 1, cex.main = 0.95, cex.axis = 0.85, cex.lab = 0.85)
    for (j in seq_len(k)) {
      v <- as.numeric(top[[pct_cols[j]]])
      v[!is.finite(v)] <- 0
      better <- if (obj$direction[j] == "max") v > 0 else v < 0
      col <- ifelse(v == 0, "#b5b5b5", ifelse(better, "#2a78d6", "#eb6834"))
      lim <- max(abs(v), 1e-9) * c(-1.15, 1.15)
      barplot(rev(v), names.arg = rev(top$run), horiz = TRUE, col = rev(col), border = NA, xlim = lim,
              main = paste0(obj$name[j], "\n", if (obj$direction[j] == "max") "higher is better" else "lower is better"),
              xlab = "% change vs status quo", cex.names = 0.8, space = 0.25)
      abline(v = 0, col = "#6b6b6b")
    }
  })

  output$res_download <- downloadHandler(
    filename = function() "optimization_results.csv",
    content = function(file) write.csv(results(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
