# ---------------------------------------------------------------
# Llibreries
# ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(reactable)
library(shinycssloaders)
library(reticulate)

# ---------------------------------------------------------------
# Reticulate
# ---------------------------------------------------------------
dir.create("reticulate_uv_cache", showWarnings = FALSE, recursive = TRUE)

Sys.setenv(
  RETICULATE_USE_MANAGED_PYTHON = "true",
  RETICULATE_UV_CACHE_DIR = normalizePath("reticulate_uv_cache", mustWork = FALSE)
)

py_require(
  packages = c(
    "pandas==2.3.3",
    "joblib==1.5.2",
    "scikit-learn==1.6.1",
    "category_encoders==2.9.0",
    "lightgbm==4.6.0",
    "imbalanced-learn==0.14.0"
  ),
  python_version = "3.12"
)

pandas <- import("pandas", delay_load = TRUE)
joblib <- import("joblib", delay_load = TRUE)

# ---------------------------------------------------------------
# Models
# ---------------------------------------------------------------
model_files <- list(
  LogisticRegression = "model_lr.pkl",
  LightGBM           = "model_lgbm.pkl",
  LinearSVC          = "model_svc.pkl",
  NeuralNetwork      = "model_nw.pkl"
)

# Cache simple en memòria
.models_cache <- list()
get_model <- function(model_key) {
  if (!is.null(.models_cache[[model_key]])) return(.models_cache[[model_key]])
  .models_cache[[model_key]] <<- joblib$load(model_files[[model_key]])
  .models_cache[[model_key]]
}

# ---------------------------------------------------------------
# Carrega de dades
# ---------------------------------------------------------------
dades <- read.csv("data_final.csv", stringsAsFactors = FALSE)

# ---------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

apply_plot_theme <- function(p) {
  p %>% layout(
    paper_bgcolor = "white",
    plot_bgcolor  = "white",
    font = list(size = 12),
    margin = list(l = 60, r = 20, t = 40, b = 70)
  )
}

# ---------------------------------------------------------------
# MÒDUL REUTILITZABLE: Cascada geogràfica (País -> CA -> Prov -> Mun)
# ---------------------------------------------------------------
geoCascadeUI <- function(id, labels, multiple = FALSE,
                         layout = c("horizontal", "vertical")) {
  layout <- match.arg(layout)
  ns <- NS(id)
  
  if (layout == "horizontal") {
    tagList(
      column(3, selectizeInput(ns("pais"), labels$pais, choices = NULL, multiple = multiple)),
      column(3, selectizeInput(ns("ca"),   labels$ca,   choices = NULL, multiple = multiple)),
      column(3, selectizeInput(ns("prov"), labels$prov, choices = NULL, multiple = multiple)),
      column(3, selectizeInput(ns("mun"),  labels$mun,  choices = NULL, multiple = multiple))
    )
  } else {
    tagList(
      selectizeInput(ns("pais"), labels$pais, choices = NULL, multiple = multiple),
      selectizeInput(ns("ca"),   labels$ca,   choices = NULL, multiple = multiple),
      selectizeInput(ns("prov"), labels$prov, choices = NULL, multiple = multiple),
      selectizeInput(ns("mun"),  labels$mun,  choices = NULL, multiple = multiple)
    )
  }
}

geoCascadeServer <- function(id, data, cols, multiple = FALSE) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(data(), {
      df <- data()
      updateSelectizeInput(session, "pais", choices = sort(unique(df[[cols$pais]])), selected = NULL, server = TRUE)
      updateSelectizeInput(session, "ca",   choices = sort(unique(df[[cols$ca]])),   selected = NULL, server = TRUE)
      updateSelectizeInput(session, "prov", choices = sort(unique(df[[cols$prov]])), selected = NULL, server = TRUE)
      updateSelectizeInput(session, "mun",  choices = sort(unique(df[[cols$mun]])),  selected = NULL, server = TRUE)
    }, ignoreInit = FALSE)
    
    observeEvent(input$pais, {
      df <- data()
      if (!is.null(input$pais) && length(input$pais) > 0) df <- df[df[[cols$pais]] %in% input$pais, , drop = FALSE]
      
      opc_ca   <- sort(unique(df[[cols$ca]]))
      opc_prov <- sort(unique(df[[cols$prov]]))
      opc_mun  <- sort(unique(df[[cols$mun]]))
      
      updateSelectizeInput(session, "ca",   choices = opc_ca,   selected = intersect(input$ca,   opc_ca),   server = TRUE)
      updateSelectizeInput(session, "prov", choices = opc_prov, selected = intersect(input$prov, opc_prov), server = TRUE)
      updateSelectizeInput(session, "mun",  choices = opc_mun,  selected = intersect(input$mun,  opc_mun),  server = TRUE)
    }, ignoreInit = TRUE)
    
    observeEvent(input$ca, {
      df <- data()
      if (!is.null(input$pais) && length(input$pais) > 0) df <- df[df[[cols$pais]] %in% input$pais, , drop = FALSE]
      if (!is.null(input$ca)   && length(input$ca) > 0)   df <- df[df[[cols$ca]]   %in% input$ca,   , drop = FALSE]
      
      opc_prov <- sort(unique(df[[cols$prov]]))
      opc_mun  <- sort(unique(df[[cols$mun]]))
      
      updateSelectizeInput(session, "prov", choices = opc_prov, selected = intersect(input$prov, opc_prov), server = TRUE)
      updateSelectizeInput(session, "mun",  choices = opc_mun,  selected = intersect(input$mun,  opc_mun),  server = TRUE)
    }, ignoreInit = TRUE)
    
    observeEvent(input$prov, {
      df <- data()
      if (!is.null(input$pais) && length(input$pais) > 0) df <- df[df[[cols$pais]] %in% input$pais, , drop = FALSE]
      if (!is.null(input$ca)   && length(input$ca) > 0)   df <- df[df[[cols$ca]]   %in% input$ca,   , drop = FALSE]
      if (!is.null(input$prov) && length(input$prov) > 0) df <- df[df[[cols$prov]] %in% input$prov, , drop = FALSE]
      
      opc_mun <- sort(unique(df[[cols$mun]]))
      updateSelectizeInput(session, "mun", choices = opc_mun, selected = intersect(input$mun, opc_mun), server = TRUE)
    }, ignoreInit = TRUE)
    
    reset_fun <- function() {
      df <- data()
      updateSelectizeInput(session, "pais", choices = sort(unique(df[[cols$pais]])), selected = NULL, server = TRUE)
      updateSelectizeInput(session, "ca",   choices = sort(unique(df[[cols$ca]])),   selected = NULL, server = TRUE)
      updateSelectizeInput(session, "prov", choices = sort(unique(df[[cols$prov]])), selected = NULL, server = TRUE)
      updateSelectizeInput(session, "mun",  choices = sort(unique(df[[cols$mun]])),  selected = NULL, server = TRUE)
    }
    
    filter_fun <- function(df) {
      if (!is.null(input$pais) && length(input$pais) > 0) df <- df[df[[cols$pais]] %in% input$pais, , drop = FALSE]
      if (!is.null(input$ca)   && length(input$ca) > 0)   df <- df[df[[cols$ca]]   %in% input$ca,   , drop = FALSE]
      if (!is.null(input$prov) && length(input$prov) > 0) df <- df[df[[cols$prov]] %in% input$prov, , drop = FALSE]
      if (!is.null(input$mun)  && length(input$mun) > 0)  df <- df[df[[cols$mun]]  %in% input$mun,  , drop = FALSE]
      df
    }
    
    list(
      pais   = reactive(input$pais),
      ca     = reactive(input$ca),
      prov   = reactive(input$prov),
      mun    = reactive(input$mun),
      reset  = reset_fun,
      filter = filter_fun
    )
  })
}

# ---------------------------------------------------------------
# KPI
# ---------------------------------------------------------------
kpi_box <- function(title, value_ui, status) {
  box(
    width = 12, height = "95px",
    title = title, status = status, solidHeader = TRUE,
    div(style = "font-size: 26px; line-height: 1.2; text-align: center;", value_ui)
  )
}

make_ui_afus <- function(df_react) {
  renderUI({
    cnt <- sum(df_react()$afusellades == "executat/da", na.rm = TRUE)
    kpi_box("Afusellats", cnt, "danger")
  })
}

make_ui_total <- function(df_react) {
  renderUI({
    cnt <- nrow(df_react())
    kpi_box("Total", cnt, "info")
  })
}

# ---------------------------------------------------------------
# Funció única per construir gràfics
# ---------------------------------------------------------------
build_plot <- function(ds, plot_type) {
  req(nrow(ds) > 0)
  
  if (plot_type == "edat") {
    dse <- ds %>% dplyr::filter(edat != 9999)
    req(nrow(dse) > 0)
    bins <- seq(floor(min(dse$edat)/5)*5, ceiling(max(dse$edat)/5)*5, by = 5)
    h <- hist(dse$edat, breaks = bins, plot = FALSE)
    
    p <- plot_ly(
      x = paste0(head(h$breaks, -1), "-", tail(h$breaks, -1)),
      y = h$counts,
      type = "bar",
      marker = list(color = "green", line = list(color = "black", width = 1)),
      hoverinfo = "y"
    ) %>% layout(
      xaxis = list(title = "Edat"),
      yaxis = list(title = "Freqüència", tickformat = "..0f"),
      bargap = 0.1
    )
    return(apply_plot_theme(p))
  }
  
  if (plot_type == "pena") {
    dfp <- ds %>% count(tipus_pena)
    p <- plot_ly(
      dfp, x = ~tipus_pena, y = ~n, type = "bar",
      marker = list(color = "green", line = list(color = "black", width = 1)),
      hoverinfo = "y"
    ) %>% layout(
      xaxis = list(title = "Tipus Pena"),
      yaxis = list(title = "Freqüència", tickformat = "..0f"),
      bargap = 0.3
    )
    return(apply_plot_theme(p))
  }
  
  if (plot_type == "proc") {
    dfp <- ds %>% count(tipus_procediment_2)
    p <- plot_ly(
      dfp, x = ~tipus_procediment_2, y = ~n, type = "bar",
      marker = list(color = "green", line = list(color = "black", width = 1)),
      hoverinfo = "y"
    ) %>% layout(
      xaxis = list(title = "Procediment"),
      yaxis = list(title = "Freqüència", tickformat = "..0f"),
      bargap = 0.3
    )
    return(apply_plot_theme(p))
  }
  
  if (plot_type == "sexe") {
    dfs <- ds %>% count(sexe)
    p <- plot_ly(
      dfs, x = ~sexe, y = ~n, type = "bar",
      color = ~sexe, colors = c("Home" = "blue", "Dona" = "red"),
      marker = list(line = list(color = "black", width = 1)),
      hoverinfo = "y", showlegend = FALSE
    ) %>% layout(
      xaxis = list(title = "Sexe"),
      yaxis = list(title = "Freqüència", tickformat = "..0f"),
      bargap = 0.3
    )
    return(apply_plot_theme(p))
  }
  
  if (plot_type == "box_tipo_pena") {
    p <- plot_ly(
      data = ds, x = ~tipus_pena, y = ~anys_pena,
      type = "box", boxpoints = FALSE,
      fillcolor = "green", line = list(color = "black")
    ) %>% layout(
      xaxis = list(title = "Tipus Pena", tickangle = 45),
      yaxis = list(title = "Anys Pena")
    )
    return(apply_plot_theme(p))
  }
  
  if (plot_type == "box_sexe_pena") {
    p <- plot_ly(
      data = ds, x = ~sexe, y = ~anys_pena,
      type = "box", boxpoints = FALSE,
      color = ~sexe, colors = c("Home" = "blue", "Dona" = "red"),
      line = list(color = "black")
    ) %>% layout(
      xaxis = list(title = "Sexe"),
      yaxis = list(title = "Anys Pena"),
      showlegend = FALSE
    )
    return(apply_plot_theme(p))
  }
  
  if (plot_type == "violin_edat_sexe") {
    dse <- ds %>% dplyr::filter(edat != 9999)
    req(nrow(dse) > 0)
    
    p <- plot_ly(
      data = dse, x = ~sexe, y = ~edat,
      type = "violin",
      box = list(visible = TRUE),
      meanline = list(visible = FALSE),
      points = FALSE,
      color = ~sexe, colors = c("Home" = "blue", "Dona" = "red"),
      line = list(color = "black")
    ) %>% layout(
      xaxis = list(title = "Sexe"),
      yaxis = list(title = "Edat"),
      showlegend = FALSE
    )
    return(apply_plot_theme(p))
  }
  
  if (plot_type == "line_mitja_pena_resol") {
    dsl <- ds %>%
      group_by(any_aprovacio_sen_o_altra_resol) %>%
      summarise(Mitjana = round(mean(anys_pena, na.rm = TRUE), 2), .groups = "drop")
    
    p <- plot_ly(
      data = dsl,
      x = ~any_aprovacio_sen_o_altra_resol,
      y = ~Mitjana,
      type = "scatter",
      mode = "lines+markers"
    ) %>% layout(
      xaxis = list(title = "Any Aprovació/Resol.", tickangle = 45),
      yaxis = list(title = "Mitjana Anys Pena")
    )
    return(apply_plot_theme(p))
  }
  
  if (plot_type == "heatmap_proc_pena") {
    dsh <- ds %>% count(tipus_procediment_2, tipus_pena)
    req(nrow(dsh) > 0)
    tab <- xtabs(n ~ tipus_pena + tipus_procediment_2, data = dsh)
    
    p <- plot_ly(
      x = colnames(tab),
      y = rownames(tab),
      z = unclass(tab),
      type = "heatmap",
      colorscale = "Greens"
    ) %>% layout(
      xaxis = list(title = "Procediment", tickangle = 45),
      yaxis = list(title = "Tipus Pena")
    )
    return(apply_plot_theme(p))
  }
  
  if (plot_type == "box_edat_proc") {
    dse <- ds %>% dplyr::filter(edat != 9999)
    req(nrow(dse) > 0)
    
    p <- plot_ly(
      data = dse, x = ~tipus_procediment_2, y = ~edat,
      type = "box", boxpoints = FALSE,
      fillcolor = "green", line = list(color = "black")
    ) %>% layout(
      xaxis = list(title = "Procediment", tickangle = 45),
      yaxis = list(title = "Edat")
    )
    return(apply_plot_theme(p))
  }
  
  apply_plot_theme(plot_ly())
}

# ---------------------------------------------------------------
# UI
# ---------------------------------------------------------------
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "TMT Barcelona"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Presentació", tabName = "presentacio", icon = icon("book")),
      tags$hr(),
      menuItem("Exploració", tabName = "cercador", icon = icon("search")),
      tags$hr(),
      menuItem("Anàlisi geogràfica", icon = icon("map")),
      menuItem("Naixement", tabName = "naixement", icon = icon("child")),
      menuItem("Residència", tabName = "residencia", icon = icon("home")),
      tags$hr(),
      menuItem("Predicció", tabName = "predictor", icon = icon("wand-magic-sparkles"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f7f7f7; }
        .box { border-radius: 10px; }
        .box-header { padding: 12px 15px; }
        .box-body { padding: 15px; }
        .alert { border-radius: 10px; }
        .soft-note { font-size: 12px; color: #666; margin-top: 6px; }
      "))
    ),
    tabItems(
      # ----------------------- Presentació -----------------------
      tabItem(
        tabName = "presentacio",
        
        fluidRow(
          box(
            width = 8, status = "primary", solidHeader = TRUE,
            h2("TMT Barcelona — visió general"),
            p("Aplicació interactiva del TFM per analitzar víctimes del franquisme jutjades pel Tribunal Militar Tercer de Barcelona."),
            p("Aquesta eina busca oferir una mirada analítica per comprendre millor els perfils i els patrons repressius durant aquest període històric.")
          ),
          
          box(
            width = 4, status = "warning", solidHeader = TRUE,
            title = "Guia ràpida",
            tags$ul(
              tags$li(tags$b("Exploració (Cercador): "), "filtra per edat, pena, procediment i cerca per nom."),
              tags$li(tags$b("Anàlisi geogràfica: "), "usa els filtres en cascada (País → CA → Província → Municipi)."),
              tags$li(tags$b("Predictor: "), "emplena el cas i calcula una probabilitat orientativa.")
            )
          )
        ),
        
        fluidRow(
          box(
            width = 6, status = "info", solidHeader = TRUE,
            title = "Fonts i documentació",
            p("Enllaços útils per contextualitzar les dades i ampliar informació:"),
            tags$ul(
              tags$li(tags$a(
                href = "https://anc.gencat.cat/ca/detall/noticia/Llista-de-processos-instruits-pel-regim-franquista",
                target = "_blank",
                "Arxiu Nacional de Catalunya — Llista de processos instruïts pel règim franquista"
              )),
              tags$li(tags$a(
                href = "https://arxiusenlinia.cultura.gencat.cat/#/cercaavancada/detallunitat/ANC1-10-T-645",
                target = "_blank",
                "Guia Procediments judicials militars ('Sumaríssims') 1939-1980 de l'Arxiu del Tribunal Militar Territorial Tercer de Barcelona"
              )),
              tags$li(tags$a(
                href = "https://memoria.gencat.cat/ca/inici/",
                target = "_blank",
                "Memòria Democràtica (Generalitat de Catalunya)"
              )),
              tags$li(tags$a(
                href = "https://pares.cultura.gob.es/archivos-estatales.html",
                target = "_blank",
                "Portal de Archivos Españoles - Archivos Estatales"
              )),

            ),
            div(
              class = "soft-note",
              "Nota: algunes fonts són hemeroteca/portal general; la disponibilitat pot variar segons el fons i la descripció arxivística."
            )
          ),
          
          box(
            width = 6, status = "success", solidHeader = TRUE,
            title = "Notes d'ús",
            div(
              class = "alert alert-info",
              strong("Consell: "),
              "Si l’app va lenta, aplica filtres abans de canviar de gràfic i evita seleccionar massa municipis alhora."
            ),
            div(
              class = "alert alert-warning",
              strong("Avís: "),
              "Les visualitzacions i prediccions depenen de la qualitat i cobertura del conjunt de dades."
            )
          )
        )
      ),
      
      # ----------------------- Cercador -----------------------
      tabItem(
        tabName = "cercador",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE, title = "Filtres i exportació",
            fluidRow(
              column(5, textInput("txt_search", "Buscar per nom/cognoms:", value = "", placeholder = "Ex.: García, Josep...")),
              column(4,
                     sliderInput("filt_edat", "Edat:",
                                 min   = min(dades$edat, na.rm = TRUE),
                                 max   = max(dades$edat[dades$edat != 9999], na.rm = TRUE),
                                 value = c(min(dades$edat, na.rm = TRUE),
                                           max(dades$edat[dades$edat != 9999], na.rm = TRUE)))
              ),
              column(3,
                     tags$div(style="display:flex; gap:10px; margin-top:25px; justify-content:flex-end;",
                              downloadButton("btn_export", "Exportar CSV"),
                              actionButton("btn_apply_cercador", "Aplicar", icon = icon("play")),
                              actionButton("btn_reset_cercador", "Reiniciar", icon = icon("undo"))
                     )
              )
            ),
            fluidRow(
              column(3,
                     checkboxInput("chk_sense_edat", "Sense edat coneguda", value = FALSE),
                     checkboxInput("chk_afusellades", "Afusellades", value = FALSE)
              ),
              column(4,
                     sliderInput("filt_anys", "Anys de Pena:",
                                 min   = min(dades$anys_pena, na.rm = TRUE),
                                 max   = max(dades$anys_pena, na.rm = TRUE),
                                 value = c(min(dades$anys_pena, na.rm = TRUE),
                                           max(dades$anys_pena, na.rm = TRUE)))
              ),
              column(4,
                     sliderInput("filt_mesos", "Mesos de Pena:",
                                 min   = min(dades$mesos_pena, na.rm = TRUE),
                                 max   = max(dades$mesos_pena, na.rm = TRUE),
                                 value = c(min(dades$mesos_pena, na.rm = TRUE),
                                           max(dades$mesos_pena, na.rm = TRUE)))
              )
            ),
            fluidRow(
              column(6,
                     selectizeInput("sel_tipuspena", "Tipus Pena:",
                                    choices  = sort(unique(dades$tipus_pena)),
                                    selected = NULL,
                                    multiple = TRUE,
                                    options  = list(placeholder = "Tots"))
              ),
              column(6,
                     selectizeInput("sel_tipusproc", "Tipus Procediment:",
                                    choices  = sort(unique(dades$tipus_procediment_2)),
                                    selected = NULL,
                                    multiple = TRUE,
                                    options  = list(placeholder = "Tots"))
              )
            )
          )
        ),
        fluidRow(
          box(width = 12, status = "info", solidHeader = TRUE, title = "Resultats",
              reactableOutput("tbl_victimes"))
        )
      ),
      
      # ----------------------- Naixement -----------------------
      tabItem(
        tabName = "naixement",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE, title = "Filtres (Naixement)",
            geoCascadeUI(
              "naix_geo",
              labels = list(
                pais = "País naixement:",
                ca   = "Comunitat autònoma naixement:",
                prov = "Província naixement:",
                mun  = "Municipi naixement:"
              ),
              multiple = TRUE,
              layout = "horizontal"
            ),
            fluidRow(
              column(3,
                     sliderInput("slider_anys_naix", "Anys de pena:",
                                 min = min(dades$anys_pena, na.rm = TRUE),
                                 max = max(dades$anys_pena, na.rm = TRUE),
                                 value = c(min(dades$anys_pena, na.rm = TRUE),
                                           max(dades$anys_pena, na.rm = TRUE)))
              ),
              column(3,
                     selectInput(
                       "sel_graf_naix",
                       "Gràfic:",
                       choices = c(
                         "Distribució per Edat" = "edat",
                         "Distribució per Tipus de Pena" = "pena",
                         "Distribució per Procediment" = "proc",
                         "Distribució per Sexe" = "sexe",
                         "Anys Pena per Tipus Pena" = "box_tipo_pena",
                         "Anys Pena per Sexe" = "box_sexe_pena",
                         "Edat per Sexe" = "violin_edat_sexe",
                         "Mitjana Anys Pena per Any Resolució Procediment" = "line_mitja_pena_resol",
                         "Heatmap Proc. vs Tipus Pena" = "heatmap_proc_pena",
                         "Boxplot Edat per Procediment" = "box_edat_proc"
                       ),
                       selected = "edat"
                     )
              ),
              column(3,
                     tags$div(style="display:flex; gap:10px; margin-top:25px;",
                              actionButton("btn_apply_naix", "Aplicar", icon = icon("play")),
                              actionButton("btn_reset_naix", "Reiniciar", icon = icon("undo"))
                     )
              ),
              column(3,
                     fluidRow(
                       column(6, uiOutput("ui_total_naix")),
                       column(6, uiOutput("ui_afus_naix"))
                     )
              )
            )
          )
        ),
        fluidRow(
          box(width = 12, status = "success", solidHeader = TRUE, title = "Gràfic (Naixement)",
              withSpinner(plotlyOutput("plot_naix_main", height = "560px")))
        )
      ),
      
      # ----------------------- Residència -----------------------
      tabItem(
        tabName = "residencia",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE, title = "Filtres (Residència)",
            geoCascadeUI(
              "res_geo",
              labels = list(
                pais = "País residència:",
                ca   = "Comunitat autònoma residència:",
                prov = "Província residència:",
                mun  = "Municipi residència:"
              ),
              multiple = TRUE,
              layout = "horizontal"
            ),
            fluidRow(
              column(3,
                     sliderInput("slider_anys_res", "Anys de pena:",
                                 min = min(dades$anys_pena, na.rm = TRUE),
                                 max = max(dades$anys_pena, na.rm = TRUE),
                                 value = c(min(dades$anys_pena, na.rm = TRUE),
                                           max(dades$anys_pena, na.rm = TRUE)))
              ),
              column(3,
                     selectInput(
                       "sel_graf_res",
                       "Gràfic:",
                       choices = c(
                         "Distribució per Edat" = "edat",
                         "Distribució per Tipus de Pena" = "pena",
                         "Distribució per Procediment" = "proc",
                         "Distribució per Sexe" = "sexe",
                         "Anys Pena per Tipus Pena" = "box_tipo_pena",
                         "Anys Pena per Sexe" = "box_sexe_pena",
                         "Edat per Sexe" = "violin_edat_sexe",
                         "Mitjana Anys Pena per Any Resolució Procediment" = "line_mitja_pena_resol",
                         "Heatmap Proc. vs Tipus Pena" = "heatmap_proc_pena",
                         "Boxplot Edat per Procediment" = "box_edat_proc"
                       ),
                       selected = "edat"
                     )
              ),
              column(3,
                     tags$div(style="display:flex; gap:10px; margin-top:25px;",
                              actionButton("btn_apply_res", "Aplicar", icon = icon("play")),
                              actionButton("btn_reset_res", "Reiniciar", icon = icon("undo"))
                     )
              ),
              column(3,
                     fluidRow(
                       column(6, uiOutput("ui_total_res")),
                       column(6, uiOutput("ui_afus_res"))
                     )
              )
            )
          )
        ),
        fluidRow(
          box(width = 12, status = "success", solidHeader = TRUE, title = "Gràfic (Residència)",
              withSpinner(plotlyOutput("plot_res_main", height = "560px")))
        )
      ),
      
      # ----------------------- Predictor -----------------------
      tabItem(
        tabName = "predictor",
        
        # Filtres dalt
        fluidRow(
          box(
            width = 12, status = "warning", solidHeader = TRUE, title = "Paràmetres del Cas (Predictor)",
            
            fluidRow(
              column(
                3,
                selectInput(
                  "pred_model",
                  "Model predictiu:",
                  choices = c(
                    "Regressió logística"  = "LogisticRegression",
                    "LightGBM"             = "LightGBM",
                    "Linear SVC calibrat"  = "LinearSVC",
                    "Xarxa neuronal"       = "NeuralNetwork"
                  ),
                  selected = "LogisticRegression"
                )
              ),
              column(
                3,
                sliderInput(
                  "pred_edat", "Edat:",
                  min   = min(dades$edat, na.rm = TRUE),
                  max   = max(dades$edat[dades$edat != 9999], na.rm = TRUE),
                  value = median(dades$edat[dades$edat != 9999], na.rm = TRUE)
                )
              ),
              column(
                3,
                selectInput("pred_sexe", "Sexe:", choices = unique(dades$sexe), selected = unique(dades$sexe)[1])
              ),
              column(
                3,
                selectInput("pred_proc2", "Tipus Procediment:",
                            choices = unique(dades$tipus_procediment_2),
                            selected = unique(dades$tipus_procediment_2)[1])
              )
            ),
            
            tags$hr(),
            
            div(
              class = "alert alert-info",
              strong("Nota: "),
              "Els filtres de lloc s’han d'escollir de major a menor granularitat (País → CA → Província → Municipi)."
            ),
            
            fluidRow(
              column(
                6,
                h4("Lloc de naixement"),
                geoCascadeUI(
                  "pred_naix_geo",
                  labels = list(
                    pais = "País Naixement:",
                    ca   = "Comunitat Aut. Naixement:",
                    prov = "Província Naixement:",
                    mun  = "Municipi Naixement:"
                  ),
                  multiple = FALSE,
                  layout   = "horizontal"
                )
              ),
              column(
                6,
                h4("Lloc de residència"),
                geoCascadeUI(
                  "pred_res_geo",
                  labels = list(
                    pais = "País Residència:",
                    ca   = "Comunitat Aut. Residència:",
                    prov = "Província Residència:",
                    mun  = "Municipi Residència:"
                  ),
                  multiple = FALSE,
                  layout   = "horizontal"
                )
              )
            ),
            
            tags$hr(),
            
            fluidRow(
              column(
                3,
                numericInput("pred_any_inicial", "Any Inicial:",
                             value = median(as.numeric(dades$any_inicial), na.rm = TRUE))
              ),
              column(
                3,
                numericInput(
                  "pred_delay_resol", "Delay Resolució:",
                  value = median(
                    as.numeric(dades$any_aprovacio_sen_o_altra_resol) - as.numeric(dades$any_inicial),
                    na.rm = TRUE
                  ),
                  min = 0
                )
              ),
              column(
                6,
                tags$div(style="display:flex; gap:10px; margin-top:25px; justify-content:flex-end;",
                         actionButton("btn_predict", "Calcular Probabilitat", icon = icon("magic"))
                ),
                div(class = "soft-note",
                    "La predicció és orientativa i depén del model i de la cobertura de les dades.")
              )
            )
          )
        ),
        
        # Resultat baix
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE, title = "Resultat (Predictor)",
            verbatimTextOutput("out_prob"),
            tags$hr(),
            div(
              class = "alert alert-warning",
              strong("Interpretació: "),
              tags$ul(
                tags$li("Resultat orientatiu, basat en patrons del conjunt de dades."),
                tags$li("No implica causalitat ni substitueix una anàlisi històrica detallada.")
              )
            )
          )
        )
      )
    )
  )
)

# ---------------------------------------------------------------
# Server
# ---------------------------------------------------------------
server <- function(input, output, session) {
  
  df_cercador <- reactiveVal(dades)
  df_naix     <- reactiveVal(dades)
  df_res      <- reactiveVal(dades)
  
  observeEvent(TRUE, {
    df_cercador(dades); df_naix(dades); df_res(dades)
  }, once = TRUE)
  
  # Geo mòduls
  naix_geo <- geoCascadeServer(
    "naix_geo",
    data = reactive(dades),
    cols = list(
      pais = "pais_naixement",
      ca   = "comunitat_autonoma_naixement",
      prov = "provincia_naixement",
      mun  = "municipi_naixement"
    ),
    multiple = TRUE
  )
  
  res_geo <- geoCascadeServer(
    "res_geo",
    data = reactive(dades),
    cols = list(
      pais = "pais_residencia",
      ca   = "comunitat_autonoma_residencia",
      prov = "provincia_residencia",
      mun  = "municipi_residencia"
    ),
    multiple = TRUE
  )
  
  pred_naix_geo <- geoCascadeServer(
    "pred_naix_geo",
    data = reactive(dades),
    cols = list(
      pais = "pais_naixement",
      ca   = "comunitat_autonoma_naixement",
      prov = "provincia_naixement",
      mun  = "municipi_naixement"
    ),
    multiple = FALSE
  )
  
  pred_res_geo <- geoCascadeServer(
    "pred_res_geo",
    data = reactive(dades),
    cols = list(
      pais = "pais_residencia",
      ca   = "comunitat_autonoma_residencia",
      prov = "provincia_residencia",
      mun  = "municipi_residencia"
    ),
    multiple = FALSE
  )
  
  # ------------------------ Cercador ------------------------
  observeEvent(input$btn_apply_cercador, {
    df <- dades
    
    if (isTRUE(input$chk_sense_edat)) {
      df <- df[df$edat == 9999, ]
    } else {
      df <- df[df$edat >= input$filt_edat[1] & df$edat <= input$filt_edat[2], ]
    }
    
    if (isTRUE(input$chk_afusellades)) df <- df[df$afusellades == "executat/da", ]
    
    df <- df[
      df$anys_pena  >= input$filt_anys[1]  & df$anys_pena  <= input$filt_anys[2] &
        df$mesos_pena >= input$filt_mesos[1] & df$mesos_pena <= input$filt_mesos[2],
    ]
    
    if (length(input$sel_tipuspena) > 0) df <- df[df$tipus_pena %in% input$sel_tipuspena, ]
    if (length(input$sel_tipusproc) > 0) df <- df[df$tipus_procediment_2 %in% input$sel_tipusproc, ]
    
    txt <- trimws(input$txt_search %||% "")
    if (nchar(txt) > 0) {
      pat <- tolower(txt)
      hay <- tolower(paste(df$cognoms_nom, df$cognoms, df$nom))
      df <- df[grepl(pat, hay, fixed = TRUE), , drop = FALSE]
    }
    
    df_cercador(df)
    
    updateSelectizeInput(session, "sel_tipuspena",
                         choices = sort(unique(df$tipus_pena)),
                         selected = intersect(input$sel_tipuspena, sort(unique(df$tipus_pena))),
                         server = TRUE)
    updateSelectizeInput(session, "sel_tipusproc",
                         choices = sort(unique(df$tipus_procediment_2)),
                         selected = intersect(input$sel_tipusproc, sort(unique(df$tipus_procediment_2))),
                         server = TRUE)
  })
  
  observeEvent(input$btn_reset_cercador, {
    df_cercador(dades)
    updateTextInput(session, "txt_search", value = "")
    
    updateSliderInput(session, "filt_edat",
                      value = c(min(dades$edat, na.rm = TRUE),
                                max(dades$edat[dades$edat != 9999], na.rm = TRUE)))
    updateSliderInput(session, "filt_anys",
                      value = c(min(dades$anys_pena, na.rm = TRUE),
                                max(dades$anys_pena, na.rm = TRUE)))
    updateSliderInput(session, "filt_mesos",
                      value = c(min(dades$mesos_pena, na.rm = TRUE),
                                max(dades$mesos_pena, na.rm = TRUE)))
    
    updateCheckboxInput(session, "chk_sense_edat", value = FALSE)
    updateCheckboxInput(session, "chk_afusellades", value = FALSE)
    
    updateSelectizeInput(session, "sel_tipuspena",
                         choices = sort(unique(dades$tipus_pena)),
                         selected = NULL, server = TRUE)
    updateSelectizeInput(session, "sel_tipusproc",
                         choices = sort(unique(dades$tipus_procediment_2)),
                         selected = NULL, server = TRUE)
  })
  
  output$btn_export <- downloadHandler(
    filename = function() paste0("cercador_resultats_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(df_cercador(), file, row.names = FALSE)
  )
  
  output$tbl_victimes <- renderReactable({
    df <- df_cercador()
    
    cols_show <- c(
      "cognoms_nom", "sexe", "edat", "tipus_procediment_2",
      "afusellades", "anys_pena", "mesos_pena", "tipus_pena",
      "municipi_residencia", "provincia_residencia", "pais_residencia",
      "municipi_naixement", "provincia_naixement", "pais_naixement"
    )
    cols_show <- intersect(cols_show, names(df))
    df <- df[, cols_show, drop = FALSE]
    
    pretty <- list(
      cognoms_nom = "Cognoms i Nom",
      sexe = "Sexe",
      edat = "Edat",
      tipus_procediment_2 = "Tipus Proc.",
      afusellades = "Afusellat",
      anys_pena = "Anys Pena",
      mesos_pena = "Mesos Pena",
      tipus_pena = "Tipus Pena",
      municipi_residencia = "Municipi Residència",
      provincia_residencia = "Província Residència",
      pais_residencia = "País Residència",
      municipi_naixement = "Municipi Naixement",
      provincia_naixement = "Província Naixement",
      pais_naixement = "País Naixement"
    )
    
    cols_defs <- lapply(names(df), function(col) {
      colDef(name = pretty[[col]] %||% col,
             filterable = col %in% c("sexe", "tipus_pena", "tipus_procediment_2"))
    })
    names(cols_defs) <- names(df)
    
    reactable(df, filterable = TRUE, searchable = TRUE, pagination = TRUE, defaultPageSize = 12, columns = cols_defs)
  })
  
  # ------------------------ Naixement/Residència ------------------------
  observeEvent(input$btn_apply_naix, {
    df <- dades
    df <- df[df$anys_pena >= input$slider_anys_naix[1] & df$anys_pena <= input$slider_anys_naix[2], ]
    df <- naix_geo$filter(df)
    df_naix(df)
  })
  
  observeEvent(input$btn_reset_naix, {
    df_naix(dades)
    updateSliderInput(session, "slider_anys_naix",
                      value = c(min(dades$anys_pena, na.rm = TRUE), max(dades$anys_pena, na.rm = TRUE)))
    naix_geo$reset()
  })
  
  observeEvent(input$btn_apply_res, {
    df <- dades
    df <- df[df$anys_pena >= input$slider_anys_res[1] & df$anys_pena <= input$slider_anys_res[2], ]
    df <- res_geo$filter(df)
    df_res(df)
  })
  
  observeEvent(input$btn_reset_res, {
    df_res(dades)
    updateSliderInput(session, "slider_anys_res",
                      value = c(min(dades$anys_pena, na.rm = TRUE), max(dades$anys_pena, na.rm = TRUE)))
    res_geo$reset()
  })
  
  output$ui_total_naix <- make_ui_total(df_naix)
  output$ui_afus_naix  <- make_ui_afus(df_naix)
  output$ui_total_res  <- make_ui_total(df_res)
  output$ui_afus_res   <- make_ui_afus(df_res)
  
  output$plot_naix_main <- renderPlotly({ build_plot(df_naix(), input$sel_graf_naix) })
  output$plot_res_main  <- renderPlotly({ build_plot(df_res(),  input$sel_graf_res)  })
  
  # ------------------------ Predictor ------------------------
  df_case <- eventReactive(input$btn_predict, {
    validate(need(is.finite(as.numeric(input$pred_any_inicial)), "L'any inicial ha de ser un número."))
    validate(need(is.finite(as.numeric(input$pred_delay_resol)), "El delay ha de ser un número."))
    validate(need(as.numeric(input$pred_delay_resol) >= 0, "El delay de resolució no pot ser negatiu."))
    
    # Forcem selecció completa per evitar NAs al model
    validate(need(!is.null(pred_naix_geo$pais()) && nzchar(pred_naix_geo$pais()), "Selecciona País de naixement."))
    validate(need(!is.null(pred_naix_geo$ca())   && nzchar(pred_naix_geo$ca()),   "Selecciona CA de naixement."))
    validate(need(!is.null(pred_naix_geo$prov()) && nzchar(pred_naix_geo$prov()), "Selecciona Província de naixement."))
    validate(need(!is.null(pred_naix_geo$mun())  && nzchar(pred_naix_geo$mun()),  "Selecciona Municipi de naixement."))
    
    validate(need(!is.null(pred_res_geo$pais()) && nzchar(pred_res_geo$pais()), "Selecciona País de residència."))
    validate(need(!is.null(pred_res_geo$ca())   && nzchar(pred_res_geo$ca()),   "Selecciona CA de residència."))
    validate(need(!is.null(pred_res_geo$prov()) && nzchar(pred_res_geo$prov()), "Selecciona Província de residència."))
    validate(need(!is.null(pred_res_geo$mun())  && nzchar(pred_res_geo$mun()),  "Selecciona Municipi de residència."))
    
    data.frame(
      edat  = as.numeric(input$pred_edat),
      sexe  = input$pred_sexe,
      tipus_procediment_2           = input$pred_proc2,
      comunitat_autonoma_naixement  = pred_naix_geo$ca(),
      comunitat_autonoma_residencia = pred_res_geo$ca(),
      pais_naixement                = pred_naix_geo$pais(),
      pais_residencia               = pred_res_geo$pais(),
      provincia_naixement           = pred_naix_geo$prov(),
      provincia_residencia          = pred_res_geo$prov(),
      municipi_naixement            = pred_naix_geo$mun(),
      municipi_residencia           = pred_res_geo$mun(),
      any_inicial                   = as.numeric(input$pred_any_inicial),
      delay_resol                   = as.numeric(input$pred_delay_resol),
      stringsAsFactors = FALSE
    )
  })
  
  output$out_prob <- renderPrint({
    model_name <- input$pred_model
    req(model_name %in% names(model_files))
    
    m <- get_model(model_name)
    req(!is.null(m))
    
    newdata <- df_case()
    req(nrow(newdata) == 1)
    
    pd <- pandas$DataFrame(newdata)
    proba <- m$predict_proba(pd)[, 2]
    
    cat(
      sprintf("Model utilitzat: %s\n", model_name),
      sprintf("Probabilitat d’afusellament: %.1f%%", proba * 100),
      sep = ""
    )
  })
}

# ---------------------------------------------------------------
# Llança l'aplicació
# ---------------------------------------------------------------
shinyApp(ui, server)
