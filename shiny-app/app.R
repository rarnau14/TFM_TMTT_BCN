  # ---------------------------------------------------------------
  # Llibreries
  # ---------------------------------------------------------------
  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(reactable)
  


  # ---------------------------------------------------------------
  # Reticulate (shinyapps.io) - Python managed + cache estable + versions fixes
  # ---------------------------------------------------------------

  # Cache estable (evita errors de .tmpXXXX/bin/python)
  dir.create("reticulate_uv_cache", showWarnings = FALSE, recursive = TRUE)
  
  # Variables ABANS de carregar reticulate
  Sys.setenv(
    RETICULATE_USE_MANAGED_PYTHON = "true",
    RETICULATE_UV_CACHE_DIR = normalizePath("reticulate_uv_cache", mustWork = FALSE)
  )
  
  library(reticulate)
  
  # Requeriments Python (versions fixes)
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
  
  # ---------------------------------------------------------------
  # Llibreries de Python
  # ---------------------------------------------------------------
  pandas <- import("pandas", delay_load = TRUE)
  joblib <- import("joblib", delay_load = TRUE)
  
  # ---------------------------------------------------------------
  # Carrega del model
  # ---------------------------------------------------------------
  models <- joblib$load("models.pkl")
  
  # ---------------------------------------------------------------
  # Carrega de dades
  # ---------------------------------------------------------------
  dades <- read.csv("data_final.csv", stringsAsFactors = FALSE)
  
  # ---------------------------------------------------------------
  #  MÒDUL REUTILITZABLE: Cascada geogràfica (País -> CA -> Prov. -> Mun.)
  # ---------------------------------------------------------------
  # UI del mòdul: crea 4 selectizeInput per municipi, província, CA i país
  # Afegit argument 'layout' per poder fer servir el mòdul verticalment al predictor
  geoCascadeUI <- function(id, labels, multiple = FALSE,
                           layout = c("horizontal", "vertical")) {
    layout <- match.arg(layout)
    ns <- NS(id)
    
    if (layout == "horizontal") {
      tagList(
        column(
          2,
          selectizeInput(
            ns("mun"), labels$mun,
            choices = NULL,
            multiple = multiple
          )
        ),
        column(
          2,
          selectizeInput(
            ns("prov"), labels$prov,
            choices = NULL,
            multiple = multiple
          )
        ),
        column(
          2,
          selectizeInput(
            ns("ca"), labels$ca,
            choices = NULL,
            multiple = multiple
          )
        ),
        column(
          2,
          selectizeInput(
            ns("pais"), labels$pais,
            choices = NULL,
            multiple = multiple
          )
        )
      )
    } else {
      # Distribució vertical per al predictor (mateix aspecte "de formulari")
      tagList(
        selectizeInput(
          ns("mun"), labels$mun,
          choices = NULL,
          multiple = multiple
        ),
        selectizeInput(
          ns("prov"), labels$prov,
          choices = NULL,
          multiple = multiple
        ),
        selectizeInput(
          ns("ca"), labels$ca,
          choices = NULL,
          multiple = multiple
        ),
        selectizeInput(
          ns("pais"), labels$pais,
          choices = NULL,
          multiple = multiple
        )
      )
    }
  }
  
  # Server del mòdul:
  # - Inicialitza opcions de país/CA/prov/mun.
  # - Actualitza les opcions en cascada quan canvien.
  # - Retorna reactius per a les seleccions i una funció reset().
  # - Afegim també una funció filter(df) que aplica el filtre geogràfic a un df.
  geoCascadeServer <- function(id, data, cols, multiple = FALSE) {
    moduleServer(id, function(input, output, session) {
      
      # Inicialització de les opcions a partir de tot el dataset
      observeEvent(data(), {
        df <- data()
        
        opc_pais <- sort(unique(df[[cols$pais]]))
        opc_ca   <- sort(unique(df[[cols$ca]]))
        opc_prov <- sort(unique(df[[cols$prov]]))
        opc_mun  <- sort(unique(df[[cols$mun]]))
        
        updateSelectizeInput(session, "pais",
                             choices  = opc_pais,
                             selected = NULL,
                             server   = TRUE)
        updateSelectizeInput(session, "ca",
                             choices  = opc_ca,
                             selected = NULL,
                             server   = TRUE)
        updateSelectizeInput(session, "prov",
                             choices  = opc_prov,
                             selected = NULL,
                             server   = TRUE)
        updateSelectizeInput(session, "mun",
                             choices  = opc_mun,
                             selected = NULL,
                             server   = TRUE)
      }, ignoreInit = FALSE)
      
      # Quan canvia País
      observeEvent(input$pais, {
        df <- data()
        
        if (!is.null(input$pais) && length(input$pais) > 0) {
          df <- df[df[[cols$pais]] %in% input$pais, , drop = FALSE]
        }
        
        opc_ca   <- sort(unique(df[[cols$ca]]))
        opc_prov <- sort(unique(df[[cols$prov]]))
        opc_mun  <- sort(unique(df[[cols$mun]]))
        
        sel_ca   <- intersect(input$ca,   opc_ca)
        sel_prov <- intersect(input$prov, opc_prov)
        sel_mun  <- intersect(input$mun,  opc_mun)
        
        updateSelectizeInput(session, "ca",
                             choices  = opc_ca,
                             selected = sel_ca,
                             server   = TRUE)
        updateSelectizeInput(session, "prov",
                             choices  = opc_prov,
                             selected = sel_prov,
                             server   = TRUE)
        updateSelectizeInput(session, "mun",
                             choices  = opc_mun,
                             selected = sel_mun,
                             server   = TRUE)
      }, ignoreInit = TRUE)
      
      # Quan canvia CA
      observeEvent(input$ca, {
        df <- data()
        
        if (!is.null(input$pais) && length(input$pais) > 0) {
          df <- df[df[[cols$pais]] %in% input$pais, , drop = FALSE]
        }
        if (!is.null(input$ca) && length(input$ca) > 0) {
          df <- df[df[[cols$ca]] %in% input$ca, , drop = FALSE]
        }
        
        opc_prov <- sort(unique(df[[cols$prov]]))
        opc_mun  <- sort(unique(df[[cols$mun]]))
        
        sel_prov <- intersect(input$prov, opc_prov)
        sel_mun  <- intersect(input$mun,  opc_mun)
        
        updateSelectizeInput(session, "prov",
                             choices  = opc_prov,
                             selected = sel_prov,
                             server   = TRUE)
        updateSelectizeInput(session, "mun",
                             choices  = opc_mun,
                             selected = sel_mun,
                             server   = TRUE)
      }, ignoreInit = TRUE)
      
      # Quan canvia Província
      observeEvent(input$prov, {
        df <- data()
        
        if (!is.null(input$pais) && length(input$pais) > 0) {
          df <- df[df[[cols$pais]] %in% input$pais, , drop = FALSE]
        }
        if (!is.null(input$ca) && length(input$ca) > 0) {
          df <- df[df[[cols$ca]] %in% input$ca, , drop = FALSE]
        }
        if (!is.null(input$prov) && length(input$prov) > 0) {
          df <- df[df[[cols$prov]] %in% input$prov, , drop = FALSE]
        }
        
        opc_mun <- sort(unique(df[[cols$mun]]))
        sel_mun <- intersect(input$mun, opc_mun)
        
        updateSelectizeInput(session, "mun",
                             choices  = opc_mun,
                             selected = sel_mun,
                             server   = TRUE)
      }, ignoreInit = TRUE)
      
      # Funció de reset per reutilitzar des de fora
      reset_fun <- function() {
        df <- data()
        opc_pais <- sort(unique(df[[cols$pais]]))
        opc_ca   <- sort(unique(df[[cols$ca]]))
        opc_prov <- sort(unique(df[[cols$prov]]))
        opc_mun  <- sort(unique(df[[cols$mun]]))
        
        updateSelectizeInput(session, "pais",
                             choices  = opc_pais,
                             selected = NULL,
                             server   = TRUE)
        updateSelectizeInput(session, "ca",
                             choices  = opc_ca,
                             selected = NULL,
                             server   = TRUE)
        updateSelectizeInput(session, "prov",
                             choices  = opc_prov,
                             selected = NULL,
                             server   = TRUE)
        updateSelectizeInput(session, "mun",
                             choices  = opc_mun,
                             selected = NULL,
                             server   = TRUE)
      }
      
      # Funció per aplicar els filtres geogràfics a un df passat des de fora
      filter_fun <- function(df) {
        pais_sel <- input$pais
        ca_sel   <- input$ca
        prov_sel <- input$prov
        mun_sel  <- input$mun
        
        if (!is.null(pais_sel) && length(pais_sel) > 0) {
          df <- df[df[[cols$pais]] %in% pais_sel, , drop = FALSE]
        }
        if (!is.null(ca_sel) && length(ca_sel) > 0) {
          df <- df[df[[cols$ca]] %in% ca_sel, , drop = FALSE]
        }
        if (!is.null(prov_sel) && length(prov_sel) > 0) {
          df <- df[df[[cols[["prov"]]]] %in% prov_sel, , drop = FALSE]
        }
        if (!is.null(mun_sel) && length(mun_sel) > 0) {
          df <- df[df[[cols$mun]] %in% mun_sel, , drop = FALSE]
        }
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
  # Funció auxiliar per crear caixes de gràfic (mateix aspecte)
  # ---------------------------------------------------------------
  make_graph_box <- function(id, title, status) {
    box(
      width = 12,
      title = title,
      status = status,
      solidHeader = TRUE,
      height = "600px",
      plotlyOutput(id, height = "540px")
    )
  }
  
  # ---------------------------------------------------------------
  # HELPERS DE GRÀFICS
  # ---------------------------------------------------------------
  
  # Histograma d'edats (intervals de 5 anys, excloent edat 9999)
  make_hist_edat <- function(df_react) {
    renderPlotly({
      req(nrow(df_react()) > 0)
      ds <- df_react() %>% dplyr::filter(edat != 9999)
      req(nrow(ds) > 0)
      
      bins <- seq(floor(min(ds$edat)/5)*5,
                  ceiling(max(ds$edat)/5)*5,
                  by = 5)
      h <- hist(ds$edat, breaks = bins, plot = FALSE)
      
      plot_ly(
        x = paste0(head(h$breaks, -1), "-", tail(h$breaks, -1)),
        y = h$counts,
        type = 'bar',
        marker = list(color = 'green', line = list(color = 'black', width = 1)),
        hoverinfo = 'y'
      ) %>% layout(
        xaxis = list(title = 'Edat'),
        yaxis = list(title = "Freqüència", tickformat = "..0f"),
        bargap = 0.1
      )
    })
  }
  
  # Barres per tipus de pena
  make_bar_pena <- function(df_react) {
    renderPlotly({
      req(nrow(df_react()) > 0)
      df_pena <- df_react() %>% count(tipus_pena)
      
      plot_ly(
        df_pena, x = ~tipus_pena, y = ~n, type = 'bar',
        marker = list(color = 'green', line = list(color = 'black', width = 1)),
        hoverinfo = 'y'
      ) %>%
        layout(
          xaxis = list(title = 'Tipus Pena'),
          yaxis = list(title = "Freqüència", tickformat = "..0f"),
          bargap = 0.3
        )
    })
  }
  
  # Barres per tipus de procediment
  make_bar_proc2 <- function(df_react) {
    renderPlotly({
      req(nrow(df_react()) > 0)
      df_proc <- df_react() %>% count(tipus_procediment_2)
      
      plot_ly(
        df_proc, x = ~tipus_procediment_2, y = ~n, type = 'bar',
        marker = list(color = 'green', line = list(color = 'black', width = 1)),
        hoverinfo = 'y'
      ) %>%
        layout(
          xaxis = list(title = 'Procediment'),
          yaxis = list(title = "Freqüència", tickformat = "..0f"),
          bargap = 0.3
        )
    })
  }
  
  # Barres per sexe amb colors
  make_bar_sexe <- function(df_react) {
    renderPlotly({
      req(nrow(df_react()) > 0)
      df_sexe <- df_react() %>% count(sexe)
      
      plot_ly(
        df_sexe, x = ~sexe, y = ~n, type = 'bar',
        color = ~sexe, colors = c("Home" = "blue", "Dona" = "red"),
        marker = list(line = list(color = 'black', width = 1)),
        hoverinfo = 'y', showlegend = FALSE
      ) %>%
        layout(
          xaxis = list(title = 'Sexe'),
          yaxis = list(title = "Freqüència", tickformat = "..0f"),
          bargap = 0.3
        )
    })
  }
  
  # Boxplot: anys de pena per tipus de pena
  make_box_tipo_pena <- function(df_react) {
    renderPlotly({
      req(nrow(df_react()) > 0)
      ds <- df_react()
      p <- ggplot(ds, aes(x = tipus_pena, y = anys_pena)) +
        geom_boxplot(fill = "green") +
        labs(x = "Tipus Pena", y = "Anys Pena") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    })
  }
  
  # Boxplot: anys de pena per sexe
  make_box_sexe_pena <- function(df_react) {
    renderPlotly({
      req(nrow(df_react()) > 0)
      ds <- df_react()
      p <- ggplot(ds, aes(x = sexe, y = anys_pena, fill = sexe)) +
        geom_boxplot() +
        labs(x = "Sexe", y = "Anys Pena") +
        scale_fill_manual(values = c("Home" = "blue", "Dona" = "red")) +
        theme_minimal()
      ggplotly(p)
    })
  }
  
  # Violí: edat per sexe
  make_violin_edat_sexe <- function(df_react) {
    renderPlotly({
      req(nrow(df_react()) > 0)
      ds <- df_react() %>% dplyr::filter(edat != 9999)
      req(nrow(ds) > 0)
      p <- ggplot(ds, aes(x = sexe, y = edat, fill = sexe)) +
        geom_violin() +
        scale_fill_manual(values = c("Home" = "blue", "Dona" = "red")) +
        labs(x = "Sexe", y = "Edat") +
        theme_minimal()
      ggplotly(p)
    })
  }
  
  # Línia: mitjana anys de pena per any de resolució (x com a factor)
  make_line_mitja_pena_resol_factor <- function(df_react) {
    renderPlotly({
      req(nrow(df_react()) > 0)
      ds <- df_react() %>%
        group_by(any_aprovacio_sen_o_altra_resol) %>%
        summarise(Mitjana = round(mean(anys_pena, na.rm = TRUE), 2))
      p <- ggplot(ds,
                  aes(x = factor(any_aprovacio_sen_o_altra_resol),
                      y = Mitjana, group = 1)) +
        geom_line() + geom_point() +
        labs(x = "Any Aprovació/Resol.", y = "Mitjana Anys Pena") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    })
  }
  
  # Línia: mitjana anys de pena per any de resolució (x numèrica)
  make_line_mitja_pena_resol <- function(df_react) {
    renderPlotly({
      req(nrow(df_react()) > 0)
      ds <- df_react() %>%
        group_by(any_aprovacio_sen_o_altra_resol) %>%
        summarise(Mitjana = round(mean(anys_pena, na.rm = TRUE), 2))
      p <- ggplot(ds,
                  aes(x = any_aprovacio_sen_o_altra_resol,
                      y = Mitjana, group = 1)) +
        geom_line() + geom_point() +
        labs(x = "Any Aprovació/Resol.", y = "Mitjana Anys Pena") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    })
  }
  
  # Scatter: edat vs anys de pena
  make_scatter_edat_pena <- function(df_react) {
    renderPlotly({
      req(nrow(df_react()) > 0)
      ds <- df_react() %>% dplyr::filter(edat != 9999)
      req(nrow(ds) > 0)
      p <- ggplot(ds, aes(x = edat, y = anys_pena, color = sexe)) +
        geom_point(alpha = 0.6) +
        labs(
          x = "Edat", y = "Anys de Pena",
          title = "Edat vs Anys de Pena"
        ) +
        theme_minimal()
      ggplotly(p)
    })
  }
  
  # Heatmap: procediment vs tipus de pena
  make_heatmap_proc_pena <- function(df_react) {
    renderPlotly({
      req(nrow(df_react()) > 0)
      ds <- df_react() %>% count(tipus_procediment_2, tipus_pena)
      p <- ggplot(ds, aes(x = tipus_procediment_2, y = tipus_pena, fill = n)) +
        geom_tile() +
        labs(
          x = "Procediment", y = "Tipus Pena",
          title = "Heatmap Proc. vs Tipus Pena"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    })
  }
  
  # Boxplot: edat per procediment
  make_box_edat_proc <- function(df_react) {
    renderPlotly({
      req(nrow(df_react()) > 0)
      ds <- df_react() %>% dplyr::filter(edat != 9999)
      req(nrow(ds) > 0)
      p <- ggplot(ds, aes(x = tipus_procediment_2, y = edat)) +
        geom_boxplot(fill = "green") +
        labs(
          x = "Procediment", y = "Edat",
          title = "Boxplot Edat per Procediment"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    })
  }
  
  # KPI: caixa amb nombre d'afusellats
  make_ui_afus <- function(df_react) {
    renderUI({
      cnt <- sum(df_react()$afusellades == "executat/da", na.rm = TRUE)
      box(
        width = NULL, height = "100px", title = "Afusellats",
        status = "danger", solidHeader = TRUE,
        div(style = "font-size: 24px; line-height: 1.2; text-align: center;", cnt)
      )
    })
  }
  
  # KPI: caixa amb total de registres
  make_ui_total <- function(df_react) {
    renderUI({
      cnt <- nrow(df_react())
      box(
        width = NULL, height = "100px", title = "Total",
        status = "info", solidHeader = TRUE,
        div(style = "font-size: 24px; line-height: 1.2; text-align: center;", cnt)
      )
    })
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
        menuItem("Cercador",    tabName = "cercador",    icon = icon("search")),
        menuItem("Víctimes per lloc de Naixement",   tabName = "naixement",   icon = icon("child")),
        menuItem("Víctimes per lloc de Residència",  tabName = "residencia",  icon = icon("home")),
        menuItem("Predictor",   tabName = "predictor",   icon = icon("chart-line"))
      )
    ),
    dashboardBody(
      tabItems(
        # Presentació
        tabItem(tabName = "presentacio",
                fluidRow(
                  box(width = 12, status = "primary", solidHeader = TRUE,
                      h2("Presentació"),
                      HTML("
                <p>Aquesta aplicació interactiva forma part del meu Treball Final de Màster (TFM) i analitza les víctimes del franquisme jutjades pel Tribunal Militar Tercer de Barcelona.</p>
                <p>El contingut principal de l'aplicació es distribueix en diferents pestanyes:</p>
                <ul>
                  <li><b>Cercador:</b> Permet filtrar i explorar les víctimes segons les seves característiques, com l'edat, el tipus de pena o el tipus de procediment judicial.</li>
                  <li><b>Víctimes per lloc de Naixement:</b> Ofereix visualitzacions per analitzar la distribució geogràfica de les víctimes segons el seu municipi, província, comunitat autònoma o país de naixement.</li>
                  <li><b>Víctimes per lloc de Residència:</b> Mostra gràfiques similars però segons el lloc de residència, permetent comparar patrons i diferències respecte al lloc de naixement.</li>
                  <li><b>Predictor:</b> Inclou un model predictiu que estima la probabilitat que una víctima fos afusellada, segons diverses característiques seleccionades per l'usuari.</li>
                </ul>
                <p>En aplicació de la Llei 11/2017 de reparació jurídica de víctimes del franquisme, aquesta eina busca oferir una mirada analítica per comprendre millor els perfils i els patrons repressius durant aquest període històric.</p>
                ")
                  )
                )
        ),
        
        # Cercador
        tabItem(tabName = "cercador",
                fluidPage(
                  fluidRow(
                    # BOX DE FILTRES
                    box(width = 12, status = "primary", solidHeader = TRUE, title = "Filtres",
                        
                        # Controls d'edat i checkboxes alineats
                        tags$div(style = "display: flex; align-items: center; flex-wrap: wrap; gap: 1rem; margin-bottom: 1rem; width: 100%;",
                                 div(style = "flex: 2 1 auto; min-width: 300px;",
                                     sliderInput("filt_edat", "Edat:",
                                                 min   = min(dades$edat, na.rm = TRUE),
                                                 max   = max(dades$edat[dades$edat != 9999], na.rm = TRUE),
                                                 value = c(min(dades$edat, na.rm = TRUE),
                                                           max(dades$edat[dades$edat != 9999], na.rm = TRUE))
                                     )
                                 ),
                                 div(style = "flex: 1 1 auto; min-width: 200px; display: flex; flex-direction: column; gap: 0.5rem; margin-left: 2rem;",
                                     checkboxInput("chk_sense_edat", "Persones sense edat coneguda", value = FALSE),
                                     checkboxInput("chk_afusellades", "Afusellades", value = FALSE)
                                 )
                        ),
                        
                        # Controls de pena i mesos
                        fluidRow(
                          column(6,
                                 sliderInput("filt_anys", "Anys de Pena:",
                                             min   = min(dades$anys_pena, na.rm = TRUE),
                                             max   = max(dades$anys_pena, na.rm = TRUE),
                                             value = c(min(dades$anys_pena, na.rm = TRUE),
                                                       max(dades$anys_pena, na.rm = TRUE))
                                 )
                          ),
                          column(6,
                                 sliderInput("filt_mesos", "Mesos de Pena:",
                                             min   = min(dades$mesos_pena, na.rm = TRUE),
                                             max   = max(dades$mesos_pena, na.rm = TRUE),
                                             value = c(min(dades$mesos_pena, na.rm = TRUE),
                                                       max(dades$mesos_pena, na.rm = TRUE))
                                 )
                          )
                        ),
                        
                        # Selectize externs
                        fluidRow(
                          column(4,
                                 selectizeInput("sel_tipuspena", "Tipus Pena:",
                                                choices  = sort(unique(dades$tipus_pena)),
                                                selected = NULL,
                                                multiple = TRUE,
                                                options  = list(placeholder = "Tots")
                                 )
                          ),
                          column(4,
                                 selectizeInput("sel_tipusproc", "Tipus Procediment:",
                                                choices  = sort(unique(dades$tipus_procediment_2)),
                                                selected = NULL,
                                                multiple = TRUE,
                                                options  = list(placeholder = "Tots")
                                 )
                          )
                        ),
                        
                        # Botons Aplicar / Reiniciar (Cercador)
                        div(style = "margin-top: 10px; display: flex; gap: 10px;",
                            actionButton("btn_apply_cercador",  "Aplicar filtres",   icon = icon("play")),
                            actionButton("btn_reset_cercador",  "Reiniciar filtres", icon = icon("undo"))
                        )
                    )
                  ),
                  
                  # BOX SEPARAT PER A LA TAULA CERCADOR
                  fluidRow(
                    box(width = 12,
                        reactableOutput("tbl_victimes")
                    )
                  )
                )
        ),
        
        # Pestanya Naixement
        tabItem(tabName = "naixement",
                fluidPage(
                  fluidRow(
                    box(width = 12, title = "Filtres", status = "primary", solidHeader = TRUE,
                        tags$div(style = "display: flex; align-items: center; gap: 3rem; flex-wrap: wrap;",
                                 # Mòdul de cascada geogràfica (Naixement)
                                 geoCascadeUI(
                                   "naix_geo",
                                   labels = list(
                                     mun  = "Municipi naixement:",
                                     prov = "Província naixement:",
                                     ca   = "Comunitat autònoma naixement:",
                                     pais = "País naixement:"
                                   ),
                                   multiple = TRUE,
                                   layout   = "horizontal"
                                 ),
                                 column(2,
                                        sliderInput("slider_anys_naix", "Anys de pena:",
                                                    min = min(dades$anys_pena, na.rm = TRUE),
                                                    max = max(dades$anys_pena, na.rm = TRUE),
                                                    value = c(min(dades$anys_pena, na.rm = TRUE),
                                                              max(dades$anys_pena, na.rm = TRUE))
                                        )
                                 )
                        ),
                        # Botons Aplicar / Reiniciar (Naixement)
                        div(style = "margin-top: 10px; display: flex; gap: 10px;",
                            actionButton("btn_apply_naix",  "Aplicar filtres",   icon = icon("play")),
                            actionButton("btn_reset_naix",  "Reiniciar filtres", icon = icon("undo"))
                        )
                    )
                  ),
                  fluidRow(
                    column(width = 6,
                           box(width = NULL, height = "100px", title = "Tria Gràfic", status = "warning", solidHeader = TRUE,
                               selectInput("sel_graf_naix", NULL,
                                           choices = c(
                                             "Distribució per Edat" = "edat",
                                             "Distribució per Tipus de Pena" = "pena",
                                             "Distribució per Procediment" = "proc",
                                             "Distribució per Sexe" = "sexe",
                                             "Anys Pena per Tipus Pena" = "box_tipo_pena",
                                             "Anys Pena per Sexe" = "box_sexe_pena",
                                             "Edat per Sexe" = "violin_edat_sexe",
                                             "Mitjana Anys Pena per Any Resolució Procediment" = "line_mitja_pena_resol",
                                             "Edat vs Anys de Pena (scatter)"   = "scatter_edat_pena",
                                             "Heatmap Proc. vs Tipus Pena"      = "heatmap_proc_pena",
                                             "Boxplot Edat per Procediment"     = "box_edat_proc"
                                           ),
                                           selected = "edat"
                               )
                           )
                    ),
                    column(width = 2, uiOutput("ui_afus_naix")),
                    column(width = 2, uiOutput("ui_total_naix"))
                  ),
                  uiOutput("graf_naix")
                )
        ),
        
        # Pestanya Residència
        tabItem(tabName = "residencia",
                fluidPage(
                  fluidRow(
                    box(width = 12, title = "Filtres", status = "primary", solidHeader = TRUE,
                        tags$div(style = "display: flex; align-items: center; gap: 3rem; flex-wrap: wrap;",
                                 # Mòdul de cascada geogràfica (Residència)
                                 geoCascadeUI(
                                   "res_geo",
                                   labels = list(
                                     mun  = "Municipi residència:",
                                     prov = "Província residència:",
                                     ca   = "Comunitat autònoma residència:",
                                     pais = "País residència:"
                                   ),
                                   multiple = TRUE,
                                   layout   = "horizontal"
                                 ),
                                 column(2,
                                        sliderInput("slider_anys_res", "Anys de pena:",
                                                    min = min(dades$anys_pena, na.rm = TRUE),
                                                    max = max(dades$anys_pena, na.rm = TRUE),
                                                    value = c(min(dades$anys_pena, na.rm = TRUE),
                                                              max(dades$anys_pena, na.rm = TRUE))
                                        )
                                 )
                        ),
                        # Botons Aplicar / Reiniciar (Residència)
                        div(style = "margin-top: 10px; display: flex; gap: 10px;",
                            actionButton("btn_apply_res",  "Aplicar filtres",   icon = icon("play")),
                            actionButton("btn_reset_res",  "Reiniciar filtres", icon = icon("undo"))
                        )
                    )
                  ),
                  fluidRow(
                    column(width = 6,
                           box(width = NULL, height = "100px", title = "Tria Gràfic", status = "warning", solidHeader = TRUE,
                               selectInput("sel_graf_res", NULL,
                                           choices = c(
                                             "Distribució per Edat" = "edat",
                                             "Distribució per Tipus de Pena" = "pena",
                                             "Distribució per Procediment" = "proc",
                                             "Distribució per Sexe" = "sexe",
                                             "Anys Pena per Tipus Pena" = "box_tipo_pena",
                                             "Anys Pena per Sexe" = "box_sexe_pena",
                                             "Edat per Sexe" = "violin_edat_sexe",
                                             "Mitjana Anys Pena per Any Resolució Procediment" = "line_mitja_pena_resol",
                                             "Edat vs Anys de Pena (scatter)"   = "scatter_edat_pena",
                                             "Heatmap Proc. vs Tipus Pena"      = "heatmap_proc_pena",
                                             "Boxplot Edat per Procediment"     = "box_edat_proc"
                                           ),
                                           selected = "edat"
                               )
                           )
                    ),
                    column(width = 2, uiOutput("ui_afus_res")),
                    column(width = 2, uiOutput("ui_total_res"))
                  ),
                  uiOutput("graf_res")
                )
        ),
        
        # Predictor
        tabItem(
          tabName = "predictor",
          fluidRow(
            box(
              width = 4, status = "warning", solidHeader = TRUE,
              title = "Paràmetres del Cas",
              
              # Selecció de model predictiu
              selectInput(
                "pred_model",
                "Model predictiu:",
                choices = c(
                  "Random Forest"        = "RandomForest",
                  "Regressió logística"  = "LogisticRegression",
                  "LightGBM"             = "LightGBM",
                  "Linear SVC calibrat"  = "LinearSVC",
                  "Xarxa neuronal"       = "NeuralNetwork"
                ),
                selected = "RandomForest"
              ),
              
              # Inputs del cas
              sliderInput(
                "pred_edat", "Edat:",
                min   = min(dades$edat, na.rm = TRUE),
                max   = max(dades$edat[dades$edat != 9999], na.rm = TRUE),
                value = median(dades$edat[dades$edat != 9999], na.rm = TRUE)
              ),
              
              selectInput(
                "pred_sexe", "Sexe:",
                choices  = unique(dades$sexe),
                selected = unique(dades$sexe)[1]
              ),
              
              selectInput(
                "pred_proc2", "Tipus Procediment:",
                choices  = unique(dades$tipus_procediment_2),
                selected = unique(dades$tipus_procediment_2)[1]
              ),
              
              tags$hr(),
              h4("Lloc de naixement"),
              geoCascadeUI(
                "pred_naix_geo",
                labels = list(
                  mun  = "Municipi Naixement:",
                  prov = "Província Naixement:",
                  ca   = "Comunitat Aut. Naixement:",
                  pais = "País Naixement:"
                ),
                multiple = FALSE,
                layout   = "vertical"
              ),
              
              tags$hr(),
              h4("Lloc de residència"),
              geoCascadeUI(
                "pred_res_geo",
                labels = list(
                  mun  = "Municipi Residència:",
                  prov = "Província Residència:",
                  ca   = "Comunitat Aut. Residència:",
                  pais = "País Residència:"
                ),
                multiple = FALSE,
                layout   = "vertical"
              ),
              
              tags$hr(),
              numericInput(
                "pred_any_inicial", "Any Inicial:",
                value = median(as.numeric(dades$any_inicial), na.rm = TRUE)
              ),
              
              numericInput(
                "pred_delay_resol", "Delay Resolució:",
                value = median(
                  as.numeric(dades$any_aprovacio_sen_o_altra_resol) -
                    as.numeric(dades$any_inicial),
                  na.rm = TRUE
                )
              ),
              
              actionButton("btn_predict", "Calcular Probabilitat", icon = icon("magic"))
            ),
            
            box(
              width = 8, status = "info", solidHeader = TRUE,
              title = "Probabilitat d’Afusellament",
              verbatimTextOutput("out_prob")
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
    
    # Dataframes filtrats "aplicats" per a cada pestanya
    df_cercador <- reactiveVal(dades)
    df_naix     <- reactiveVal(dades)
    df_res      <- reactiveVal(dades)
    
    # Inicialització
    observeEvent(TRUE, {
      df_cercador(dades)
      df_naix(dades)
      df_res(dades)
    }, once = TRUE)
    
    # Instàncies del mòdul de cascada geogràfica
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
    
    # Mòduls geocascada específics del predictor (sense múltiples)
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
    
    #------------------------------------------------------------------
    # 1) CERCADOR - Aplicar / Reiniciar filtres
    #------------------------------------------------------------------
    
    observeEvent(input$btn_apply_cercador, {
      df <- dades
      
      # Edat / sense edat
      if (input$chk_sense_edat) {
        df <- df[df$edat == 9999, ]
      } else {
        df <- df[df$edat >= input$filt_edat[1] & df$edat <= input$filt_edat[2], ]
      }
      
      # Afusellades
      if (input$chk_afusellades) {
        df <- df[df$afusellades == "executat/da", ]
      }
      
      # Anys i mesos de pena
      df <- df[
        df$anys_pena  >= input$filt_anys[1]  & df$anys_pena  <= input$filt_anys[2] &
          df$mesos_pena >= input$filt_mesos[1] & df$mesos_pena <= input$filt_mesos[2],
      ]
      
      # Tipus pena / procediment
      if (length(input$sel_tipuspena) > 0) {
        df <- df[df$tipus_pena %in% input$sel_tipuspena, ]
      }
      if (length(input$sel_tipusproc) > 0) {
        df <- df[df$tipus_procediment_2 %in% input$sel_tipusproc, ]
      }
      
      df_cercador(df)
      
      # Actualitzar opcions disponibles en els selectize segons el df filtrat
      opc_pena <- sort(unique(df$tipus_pena))
      opc_proc <- sort(unique(df$tipus_procediment_2))
      
      updateSelectizeInput(session, "sel_tipuspena",
                           choices  = opc_pena,
                           selected = intersect(input$sel_tipuspena, opc_pena),
                           server   = TRUE)
      updateSelectizeInput(session, "sel_tipusproc",
                           choices  = opc_proc,
                           selected = intersect(input$sel_tipusproc, opc_proc),
                           server   = TRUE)
    })
    
    observeEvent(input$btn_reset_cercador, {
      df_cercador(dades)
      
      updateSliderInput(session, "filt_edat",
                        min = min(dades$edat, na.rm = TRUE),
                        max = max(dades$edat[dades$edat != 9999], na.rm = TRUE),
                        value = c(min(dades$edat, na.rm = TRUE),
                                  max(dades$edat[dades$edat != 9999], na.rm = TRUE)))
      
      updateSliderInput(session, "filt_anys",
                        min = min(dades$anys_pena, na.rm = TRUE),
                        max = max(dades$anys_pena, na.rm = TRUE),
                        value = c(min(dades$anys_pena, na.rm = TRUE),
                                  max(dades$anys_pena, na.rm = TRUE)))
      
      updateSliderInput(session, "filt_mesos",
                        min = min(dades$mesos_pena, na.rm = TRUE),
                        max = max(dades$mesos_pena, na.rm = TRUE),
                        value = c(min(dades$mesos_pena, na.rm = TRUE),
                                  max(dades$mesos_pena, na.rm = TRUE)))
      
      updateCheckboxInput(session, "chk_sense_edat", value = FALSE)
      updateCheckboxInput(session, "chk_afusellades", value = FALSE)
      
      updateSelectizeInput(session, "sel_tipuspena",
                           choices  = sort(unique(dades$tipus_pena)),
                           selected = NULL,
                           server   = TRUE)
      updateSelectizeInput(session, "sel_tipusproc",
                           choices  = sort(unique(dades$tipus_procediment_2)),
                           selected = NULL,
                           server   = TRUE)
    })
    
    # Taula Cercador
    output$tbl_victimes <- renderReactable({
      col_order <- c(
        "cognoms", "cognoms_nom", "nom", "sexe", "edat",
        "tipus_procediment_2","any_inicial", 
        "any_aprovacio_sen_o_altra_resol","commutacio_indult_demanat", 
        "afusellades","anys_pena", "mesos_pena", "tipus_pena",
        "municipi_residencia", "provincia_residencia",
        "comunitat_autonoma_residencia", "pais_residencia",
        "municipi_naixement", "provincia_naixement",
        "comunitat_autonoma_naixement", "pais_naixement"
      )
      
      drop_cols <- c(
        "comarca_naixement", "comarca_residencia",
        "municipi_naixement_longitud_etrs89",
        "municipi_naixement_latitud_etrs89",
        "municipi_residencia_longitud_etrs89",
        "municipi_residencia_latitud_etrs89",
        "Naixement_Longitud_REAL",
        "Naixement_Latitud_REAL",
        "Residencia_Longitud_REAL",
        "Residencia_Latitud_REAL",
        "tipus_procediment_1", 
        "pena", "codi"
      )
      
      pretty_names <- list(
        cognoms                         = "Cognoms",
        cognoms_nom                     = "Cognoms i Nom",
        nom                             = "Nom",
        sexe                            = "Sexe",
        edat                            = "Edat",
        tipus_procediment_2             = "Tipus Proc.",
        any_inicial                     = "Any Inicial",
        any_aprovacio_sen_o_altra_resol = "Any Aprov./Resol.",
        commutacio_indult_demanat       = "Indult Sol·licitat",
        afusellades                     = "Afusellat",
        anys_pena                       = "Anys Pena",
        mesos_pena                      = "Mesos Pena",
        tipus_pena                      = "Tipus Pena",
        municipi_residencia             = "Municipi Residència",
        provincia_residencia            = "Província Residència",
        comunitat_autonoma_residencia   = "Com. Autònoma Residència",
        pais_residencia                 = "País Residència",
        municipi_naixement              = "Municipi Naixement",
        provincia_naixement             = "Província Naixement",
        comunitat_autonoma_naixement    = "Com. Autònoma Naixement",
        pais_naixement                  = "País Naixement"
      )
      
      df <- df_cercador()
      
      cols_to_show <- intersect(col_order, names(df))
      cols_to_show <- setdiff(cols_to_show, drop_cols)
      data_show   <- df[, cols_to_show, drop = FALSE]
      
      cols_defs <- lapply(cols_to_show, function(col) {
        colDef(
          name       = pretty_names[[col]],
          filterable = TRUE,
          align      = "left"
        )
      })
      names(cols_defs) <- cols_to_show
      
      reactable(
        data_show,
        filterable      = TRUE,
        searchable      = FALSE,
        pagination      = TRUE,
        defaultPageSize = 10,
        columns         = cols_defs
      )
    })
    
    #------------------------------------------------------------------
    # 2) NAIXEMENT - Aplicar / Reiniciar filtres
    #------------------------------------------------------------------
    
    observeEvent(input$btn_apply_naix, {
      df <- dades
      
      # Filtre per anys de pena
      df <- df[df$anys_pena >= input$slider_anys_naix[1] &
                 df$anys_pena <= input$slider_anys_naix[2], ]
      
      # Aplicar filtres geogràfics des del mòdul
      df <- naix_geo$filter(df)
      
      df_naix(df)
    })
    
    observeEvent(input$btn_reset_naix, {
      df_naix(dades)
      
      updateSliderInput(session, "slider_anys_naix",
                        min = min(dades$anys_pena, na.rm = TRUE),
                        max = max(dades$anys_pena, na.rm = TRUE),
                        value = c(min(dades$anys_pena, na.rm = TRUE),
                                  max(dades$anys_pena, na.rm = TRUE)))
      
      # Reset del mòdul geogràfic
      naix_geo$reset()
    })
    
    #------------------------------------------------------------------
    # 3) RESIDÈNCIA - Aplicar / Reiniciar filtres
    #------------------------------------------------------------------
    
    observeEvent(input$btn_apply_res, {
      df <- dades
      
      # Filtre per anys de pena
      df <- df[df$anys_pena >= input$slider_anys_res[1] &
                 df$anys_pena <= input$slider_anys_res[2], ]
      
      # Aplicar filtres geogràfics des del mòdul
      df <- res_geo$filter(df)
      
      df_res(df)
    })
    
    observeEvent(input$btn_reset_res, {
      df_res(dades)
      
      updateSliderInput(session, "slider_anys_res",
                        min = min(dades$anys_pena, na.rm = TRUE),
                        max = max(dades$anys_pena, na.rm = TRUE),
                        value = c(min(dades$anys_pena, na.rm = TRUE),
                                  max(dades$anys_pena, na.rm = TRUE)))
      
      # Reset del mòdul geogràfic
      res_geo$reset()
    })
    
    #------------------------------------------------------------------
    # 4) GRÀFICS NAIXEMENT
    #------------------------------------------------------------------
    output$hist_edat_naix             <- make_hist_edat(df_naix)
    output$bar_pena_naix              <- make_bar_pena(df_naix)
    output$bar_proc2_naix             <- make_bar_proc2(df_naix)
    output$bar_sexe_naix              <- make_bar_sexe(df_naix)
    output$box_tipo_pena_naix         <- make_box_tipo_pena(df_naix)
    output$box_sexe_pena_naix         <- make_box_sexe_pena(df_naix)
    output$violin_edat_sexe_naix      <- make_violin_edat_sexe(df_naix)
    output$line_mitja_pena_resol_naix <- make_line_mitja_pena_resol_factor(df_naix)
    output$scatter_edat_pena_naix     <- make_scatter_edat_pena(df_naix)
    output$heatmap_proc_pena_naix     <- make_heatmap_proc_pena(df_naix)
    output$box_edat_proc_naix         <- make_box_edat_proc(df_naix)
    
    output$ui_afus_naix  <- make_ui_afus(df_naix)
    output$ui_total_naix <- make_ui_total(df_naix)
    
    output$graf_naix <- renderUI({
      req(input$sel_graf_naix)
      switch(
        input$sel_graf_naix,
        edat               = make_graph_box('hist_edat_naix', 'Distribució per Edat', 'success'),
        pena               = make_graph_box('bar_pena_naix', 'Distribució per Tipus de Pena', 'success'),
        proc               = make_graph_box('bar_proc2_naix', 'Distribució per Procediment', 'success'),
        sexe               = make_graph_box('bar_sexe_naix', 'Distribució per Sexe', 'success'),
        box_tipo_pena      = make_graph_box('box_tipo_pena_naix', 'Anys Pena per Tipus Pena', 'success'),
        box_sexe_pena      = make_graph_box('box_sexe_pena_naix', 'Anys Pena per Sexe', 'success'),
        violin_edat_sexe   = make_graph_box('violin_edat_sexe_naix', 'Edat per Sexe', 'success'),
        line_mitja_pena_resol = make_graph_box('line_mitja_pena_resol_naix', 'Mitjana Anys Pena per Any Resolució Procediment', 'success'),
        scatter_edat_pena      = make_graph_box('scatter_edat_pena_naix', 'Edat vs Anys de Pena', 'success'),
        heatmap_proc_pena      = make_graph_box('heatmap_proc_pena_naix', 'Heatmap Proc. vs Tipus Pena', 'success'),
        box_edat_proc          = make_graph_box('box_edat_proc_naix', 'Boxplot Edat per Procediment', 'success')
      )
    })
    
    #------------------------------------------------------------------
    # 5) GRÀFICS RESIDÈNCIA
    #------------------------------------------------------------------
    output$hist_edat_res             <- make_hist_edat(df_res)
    output$bar_pena_res              <- make_bar_pena(df_res)
    output$bar_proc2_res             <- make_bar_proc2(df_res)
    output$bar_sexe_res              <- make_bar_sexe(df_res)
    output$box_tipo_pena_res         <- make_box_tipo_pena(df_res)
    output$box_sexe_pena_res         <- make_box_sexe_pena(df_res)
    output$violin_edat_sexe_res      <- make_violin_edat_sexe(df_res)
    output$line_mitja_pena_resol_res <- make_line_mitja_pena_resol(df_res)
    output$scatter_edat_pena_res     <- make_scatter_edat_pena(df_res)
    output$heatmap_proc_pena_res     <- make_heatmap_proc_pena(df_res)
    output$box_edat_proc_res         <- make_box_edat_proc(df_res)
    
    output$ui_afus_res  <- make_ui_afus(df_res)
    output$ui_total_res <- make_ui_total(df_res)
    
    output$graf_res <- renderUI({
      req(input$sel_graf_res)
      switch(
        input$sel_graf_res,
        edat               = make_graph_box('hist_edat_res', 'Distribució per Edat', 'success'),
        pena               = make_graph_box('bar_pena_res', 'Distribució per Tipus de Pena', 'success'),
        proc               = make_graph_box('bar_proc2_res', 'Distribució per Procediment', 'success'),
        sexe               = make_graph_box('bar_sexe_res', 'Distribució per Sexe', 'success'),
        box_tipo_pena      = make_graph_box('box_tipo_pena_res', 'Anys Pena per Tipus Pena', 'success'),
        box_sexe_pena      = make_graph_box('box_sexe_pena_res', 'Anys Pena per Sexe', 'success'),
        violin_edat_sexe   = make_graph_box('violin_edat_sexe_res', 'Edat per Sexe', 'success'),
        line_mitja_pena_resol = make_graph_box('line_mitja_pena_resol_res', 'Mitjana Anys Pena per Any Resolució Procediment', 'success'),
        scatter_edat_pena      = make_graph_box('scatter_edat_pena_res', 'Edat vs Anys de Pena', 'success'),
        heatmap_proc_pena      = make_graph_box('heatmap_proc_pena_res', 'Heatmap Proc. vs Tipus Pena', 'success'),
        box_edat_proc          = make_graph_box('box_edat_proc_res', 'Boxplot Edat per Procediment', 'success')
      )
    })
    
    #------------------------------------------------------------------
    # 6) PREDICTOR (model Python ja entrenat, carregat amb joblib)
    #------------------------------------------------------------------
    
    # Construcció del cas a partir dels inputs + mòduls de geocascada
    df_case <- eventReactive(input$btn_predict, {
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
        municipi_residencia          = pred_res_geo$mun(),
        any_inicial                   = as.numeric(input$pred_any_inicial),
        delay_resol                   = as.numeric(input$pred_delay_resol),
        stringsAsFactors = FALSE
      )
    })
    
    # Càlcul de la probabilitat amb el model triat
    output$out_prob <- renderPrint({
      req(!is.null(models))
      
      model_name <- input$pred_model
      req(model_name %in% names(models))
      
      m <- models[[model_name]]
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
  
  # Llança l'aplicació
  shinyApp(ui, server)
