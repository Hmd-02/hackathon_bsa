library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(forecast)
library(tseries)
library(readr)
library(readxl)
library(haven)
library(seasonal)
library(lmtest)
library(knitr)
library(rmarkdown)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Modélisation des Séries Temporelles"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "home", icon = icon("home")),
      menuItem("Chargement des données", tabName = "upload", icon = icon("upload")),
      menuItem("Tendance et Saisonnalité", tabName = "stationarity", icon = icon("chart-line")),
      menuItem("Estimation des modèles", tabName = "model", icon = icon("cogs")),
      menuItem("Tests de significativité", tabName = "significance", icon = icon("check")),
      menuItem("Prévisions", tabName = "forecast", icon = icon("arrow-trend-up")),
      menuItem("Rapport", tabName = "report", icon = icon("file"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
    /* Police générale */
    body {
      font-family: 'Poppins', 'Helvetica Neue', sans-serif;
      background: linear-gradient(135deg, #f0f2f5, #ffffff);
      color: #333;
      margin: 0;
    }

    /* Header moderne */
    .main-header {
      background: linear-gradient(90deg, #6a11cb, #2575fc);
      color: white;
      box-shadow: 0 2px 8px rgba(0,0,0,0.15);
      height: 60px;
      line-height: 60px;
      font-weight: 600;
      font-size: 20px;
      padding-left: 20px;
      position: fixed;
      width: 100%;
      z-index: 1050;
      transition: all 0.3s ease;
    }

    /* Sidebar moderne */
    .main-sidebar {
      background: #ffffff;
      box-shadow: 2px 0 8px rgba(0,0,0,0.08);
      border-right: 1px solid #eee;
      position: fixed;
      height: 100%;
      width: 230px;
      padding-top: 70px;
      overflow-y: auto;
      transition: all 0.3s ease;
    }

    /* Menu items */
    .sidebar-menu > li > a {
      font-weight: 500;
      font-size: 16px;
      padding: 12px 20px;
      color: #555;
      transition: background 0.3s, color 0.3s;
    }
    .sidebar-menu > li.active > a,
    .sidebar-menu > li:hover > a {
      background: linear-gradient(90deg, #6a11cb, #2575fc);
      color: white;
      border-radius: 8px;
    }

    /* Wrapper du contenu */
    .content-wrapper {
      margin-left: 230px;
      margin-top: 70px;
      padding: 25px;
      background: #fafafa;
      min-height: calc(100vh - 70px);
      transition: all 0.3s ease;
    }

    /* Boxes */
    .box {
      background: white;
      border: none;
      box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      border-radius: 12px;
      padding: 20px;
      margin-bottom: 30px;
      transition: box-shadow 0.3s ease;
    }
    .box:hover {
      box-shadow: 0 4px 16px rgba(0,0,0,0.12);
    }

    /* Titres dans les boîtes */
    .box-title {
      font-size: 22px;
      font-weight: 600;
      margin-bottom: 15px;
      color: #4a4a4a;
    }

    /* Boutons */
    .btn {
      border-radius: 8px;
      font-weight: 600;
      font-size: 16px;
      padding: 10px 20px;
      transition: background 0.3s, box-shadow 0.3s;
    }
    .btn-primary {
      background: linear-gradient(90deg, #00c6ff, #0072ff);
      border: none;
      color: white;
    }
    .btn-primary:hover {
      background: linear-gradient(90deg, #0072ff, #00c6ff);
      box-shadow: 0 4px 12px rgba(0,0,0,0.15);
    }
    .btn-success {
      background: linear-gradient(90deg, #56ab2f, #a8e063);
      border: none;
      color: white;
    }
    .btn-success:hover {
      background: linear-gradient(90deg, #a8e063, #56ab2f);
      box-shadow: 0 4px 12px rgba(0,0,0,0.15);
    }

    /* Inputs */
    .form-control {
      border-radius: 8px;
      border: 1px solid #ccc;
      box-shadow: none;
      transition: border-color 0.3s;
    }
    .form-control:focus {
      border-color: #6a11cb;
      box-shadow: 0 0 5px rgba(106,17,203,0.3);
    }

    /* Notifications */
    .shiny-notification {
      border-radius: 8px;
      background: #ffffff;
      box-shadow: 0 2px 10px rgba(0,0,0,0.2);
      animation: fadeInUp 0.6s ease;
    }

    @keyframes fadeInUp {
      0% {
        opacity: 0;
        transform: translateY(20px);
      }
      100% {
        opacity: 1;
        transform: translateY(0);
      }
    }

    /* Datatable */
    table.dataTable {
      border-radius: 8px;
      overflow: hidden;
    }

    /* Plotly plots */
    .plotly {
      background: white;
      border-radius: 12px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      padding: 15px;
    }

    /* Mobile friendly */
    @media (max-width: 768px) {
      .main-sidebar {
        width: 100%;
        height: auto;
        position: relative;
      }
      .content-wrapper {
        margin-left: 0;
      }
    }
  "))
    )
    ,
    tabItems(
      # Page d'accueil
      tabItem(tabName = "home",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    h1("Modélisation Automatique des Séries Temporelles", align = "center", style = "color: blue;"),
                    div(style = "text-align: center; margin: 20px 0;",
                        img(src = "img.JPG", width = "70%", height = "auto")
                    ),
                    h4("Cette application vous permet de modéliser tout type de données temporelles en suivant une méthodologie bien définie.", align = "center"),
                    h4("Vous pourrez visualiser vos résultats et utiliser plusieurs méthodes pour tester la significativité.", align = "center"),
                    fluidRow(
                      column(width = 6, p("Ange DASSI, Baba BA et Faïçal Ouedraogo, AS3-ENSAE Dakar", align = "center")),
                      column(width = 6, p("janvier 2025", align = "center"))
                    )
                )
              )
      ),

      # Page chargement des données
      tabItem(tabName = "upload",
              h3("Chargement et visualisation des Données", align = "center"),
              tabBox(width = 12,
                     tabPanel("Chargement de la base",
                              fluidRow(
                                column(width = 6,
                                       box(width = NULL, title = "Choisissez la base qui contient le fichier à modéliser",
                                           fileInput("upload_data", "Charger votre fichier",
                                                     accept = c(".csv", ".xls", ".xlsx", ".sav", ".dta", ".rds")),
                                           uiOutput("sheet_selector")
                                       )
                                ),
                                column(width = 6,
                                       box(width = NULL, title = "Choisissez la variable à modéliser",
                                           uiOutput("var_selector"),
                                           conditionalPanel(
                                             condition = "input.choix_var_col",
                                             numericInput("freq_series", "Fréquence de la série temporelle:",
                                                          value = 12, min = 1, max = 365)
                                           )
                                       )
                                )
                              ),
                              fluidRow(
                                box(width = 12, title = "Aperçu des données",
                                    DT::dataTableOutput("data_preview")
                                )
                              )
                     ),
                     tabPanel("Visualisation de la série",
                              plotlyOutput("graph_serie", height = "500px")
                     ),
                     tabPanel("Visualisation de l'acf",
                              fluidRow(
                                column(width = 6,
                                       box(width = NULL, title = "Nombre maximal d'écarts",
                                           sliderInput("lag_acf", "Lag maximum:", min = 1, max = 40, value = 20, step = 1)
                                       )
                                ),
                                column(width = 6,
                                       box(width = NULL, title = "Niveau de risque",
                                           sliderInput("alpha_acf", "Alpha:", min = 0, max = 0.1, value = 0.05, step = 0.01)
                                       )
                                )
                              ),
                              plotlyOutput("graph_acf", height = "500px")
                     ),
                     tabPanel("Visualisation du pacf",
                              fluidRow(
                                column(width = 6,
                                       box(width = NULL, title = "Nombre maximal d'écarts",
                                           sliderInput("lag_pacf", "Lag maximum:", min = 1, max = 25, value = 20, step = 1)
                                       )
                                ),
                                column(width = 6,
                                       box(width = NULL, title = "Niveau de risque",
                                           sliderInput("alpha_pacf", "Alpha:", min = 0, max = 0.1, value = 0.05, step = 0.01)
                                       )
                                )
                              ),
                              plotlyOutput("graph_pacf", height = "500px")
                     ),
                     tabPanel("Informations",
                              box(width = 12,
                                  h4("Visualisation de la série"),
                                  p("La visualisation de la série temporelle dans l'application permet de représenter graphiquement l'évolution de votre variable au fil du temps. Elle offre une vue d'ensemble des tendances générales, des variations saisonnières ou des éventuelles anomalies présentes dans les données. Cette étape est essentielle pour mieux comprendre la structure de la série, identifier des patterns ou des irrégularités, et guider les choix des méthodes d'analyse ou de modélisation. Grâce à cette approche visuelle, vous pouvez rapidement interpréter les informations clés et prendre des décisions éclairées basées sur les dynamiques observées."),
                                  h4("La fonction d'autocorrelation"),
                                  p("La fonction d'autocorrélation (ACF) mesure la corrélation entre les valeurs de la série temporelle et ses décalages (ou lags). En d'autres termes, elle évalue dans quelle mesure les observations passées influencent celles du présent. L'ACF est un outil essentiel pour analyser la structure d'une série temporelle, notamment pour identifier la persistance, les schémas répétitifs ou les dépendances à différentes échelles de temps."),
                                  h4("La fonction d'autocorrelation partielle"),
                                  p("La fonction d'autocorrélation partielle (PACF) mesure la corrélation entre une observation d'une série temporelle et ses décalages (lags), tout en éliminant l'effet des lags intermédiaires. Contrairement à l'ACF, qui reflète les corrélations cumulées, la PACF se concentre sur les corrélations directes. Dans notre application, la visualisation de la PACF permet d'identifier l'ordre des modèles autorégressifs (AR), en déterminant le dernier lag significatif avant que les corrélations ne deviennent insignifiantes.")
                              )
                     )
              )
      ),

      # Page tendance et saisonnalité
      tabItem(tabName = "stationarity",
              h3("Evaluation de la tendance et de la saisonnalité de la série", align = "center"),
              tabBox(width = 12,
                     tabPanel("Stationnarité",
                              fluidRow(
                                column(width = 8,
                                       fluidRow(
                                         column(width = 6,
                                                box(width = NULL, title = "Type de test",
                                                    selectInput("test_stationnarite", "Choisir un test:",
                                                                choices = c("ADF", "PP", "KPSS"),
                                                                selected = "ADF")
                                                )
                                         ),
                                         column(width = 6,
                                                box(width = NULL, title = "Niveau de risque",
                                                    sliderInput("alpha_stationnarite", "Alpha:",
                                                                min = 0, max = 0.1, value = 0.05, step = 0.01)
                                                )
                                         )
                                       ),
                                       box(width = NULL, title = "Résultats des tests",
                                           verbatimTextOutput("stationnarite_results")
                                       )
                                ),
                                column(width = 4,
                                       box(width = NULL, title = "Explications",
                                           h5("Définition"),
                                           p("La tendance représente l'évolution générale d'une série temporelle sur une période prolongée."),
                                           h5("Détection de la tendance"),
                                           p("La tendance est détectée ici à travers des tests de stationnarité de la série, pour celà, vous devez choisir le ou les test(s) qui vous convient(ent) (adf,pp,kpss) et le niveau de risque associé"),
                                           p("Notez bien: parmi les tests proposés, les tests adf et pp ont pour hypothèses nulle (H0), la stationarité de la série contrairement au test kpss")
                                       )
                                )
                              )
                     ),
                     tabPanel("Saisonnalité",
                              fluidRow(
                                column(width = 8,
                                       fluidRow(
                                         column(width = 6,
                                                box(width = NULL, title = "Méthode de décomposition",
                                                    selectInput("decomp_method", "Méthode de décomposition:",
                                                                choices = c("Additive" = "additive",
                                                                            "Multiplicative" = "multiplicative"),
                                                                selected = "additive")
                                                )
                                         ),
                                         column(width = 6,
                                                box(width = NULL, title = "Test de saisonnalité",
                                                    checkboxInput("test_seasonality", "Effectuer un test de saisonnalité",
                                                                  value = TRUE)
                                                )
                                         )
                                       ),
                                       box(width = NULL, title = "Décomposition de la série",
                                           plotOutput("seasonality_decomp", height = "500px")
                                       ),
                                       conditionalPanel(
                                         condition = "input.test_seasonality == true",
                                         box(width = NULL, title = "Résultats du test de saisonnalité",
                                             verbatimTextOutput("seasonality_test_results")
                                         )
                                       )
                                ),
                                column(width = 4,
                                       box(width = NULL, title = "Explications",
                                           h5("Saisonnalité"),
                                           p("La saisonnalité correspond à des fluctuations périodiques qui se répètent à intervalles réguliers dans une série temporelle. Par exemple, des variations mensuelles, trimestrielles ou annuelles."),
                                           h5("Décomposition"),
                                           p("La décomposition permet de séparer la série temporelle en plusieurs composantes: tendance, saisonnalité et résidus."),
                                           h5("Méthodes de décomposition"),
                                           p("Additive: Utilisée lorsque l'amplitude des fluctuations saisonnières est constante au fil du temps."),
                                           p("Multiplicative: Utilisée lorsque l'amplitude des fluctuations saisonnières varie proportionnellement au niveau de la série.")
                                       )
                                )
                              )
                     ),
                     tabPanel("Différenciation",
                              fluidRow(
                                column(width = 8,
                                       fluidRow(
                                         column(width = 6,
                                                box(width = NULL, title = "Paramètres de différenciation",
                                                    numericInput("diff_order", "Ordre de différenciation (d):",  value = 1, min = 0, max = 3),
                                                    numericInput("diff_seasonal_order", "Ordre de différenciation saisonnière (D):",  value = 0, min = 0, max = 2),
                                                    actionButton("apply_diff", "Appliquer la différenciation",
                                                                 class = "btn-primary")
                                                )
                                         ),
                                         column(width = 6,
                                                box(width = NULL, title = "Tests sur série différenciée",
                                                    checkboxInput("test_diff_adf", "Test ADF sur série différenciée", value = TRUE),
                                                    checkboxInput("test_diff_kpss", "Test KPSS sur série différenciée", value = TRUE)
                                                )
                                         )
                                       ),
                                       box(width = NULL, title = "Série différenciée",
                                           plotlyOutput("diff_series_plot")
                                       ),
                                       box(width = NULL, title = "Résultats des tests sur série différenciée",
                                           verbatimTextOutput("diff_test_results")
                                       )
                                ),
                                column(width = 4,
                                       box(width = NULL, title = "Explications",
                                           h5("Différenciation"),
                                           p("La différenciation est une technique de transformation utilisée pour rendre une série temporelle stationnaire en éliminant les tendances et les saisonnalités."),
                                           h5("Différenciation simple (d)"),
                                           p("Soustrait chaque observation de sa valeur précédente. Efficace pour éliminer les tendances linéaires."),
                                           h5("Différenciation saisonnière (D)"),
                                           p("Soustrait chaque observation de sa valeur correspondante dans la période précédente (ex: même mois de l'année précédente). Efficace pour éliminer les schémas saisonniers.")
                                       )
                                )
                              )
                     )
              )
      ),

      # Page d'estimation des modèles
      tabItem(tabName = "model",
              h3("Estimation des modèles de séries temporelles", align = "center"),
              tabBox(width = 12,
                     tabPanel("Modèles ARIMA",
                              fluidRow(
                                column(width = 8,
                                       fluidRow(
                                         column(width = 6,
                                                box(width = NULL, title = "Paramètres ARIMA(p,d,q)",
                                                    numericInput("arima_p", "Ordre autorégressif (p):",  value = 1, min = 0, max = 5),
                                                    numericInput("arima_d", "Ordre d'intégration (d):",  value = 1, min = 0, max = 2),
                                                    numericInput("arima_q", "Ordre moyenne mobile (q):",  value = 1, min = 0, max = 5)
                                                )
                                         ),
                                         column(width = 6,
                                                box(width = NULL, title = "Paramètres SARIMA(P,D,Q)s",
                                                    numericInput("sarima_P", "Ordre autorégressif saisonnier (P):",  value = 0, min = 0, max = 2),
                                                    numericInput("sarima_D", "Ordre d'intégration saisonnier (D):",  value = 0, min = 0, max = 1),
                                                    numericInput("sarima_Q", "Ordre moyenne mobile saisonnier (Q):",  value = 0, min = 0, max = 2),
                                                    actionButton("estimate_arima", "Estimer le modèle",  class = "btn-primary")
                                                )
                                         )
                                       ),
                                       box(width = NULL, title = "Résultats de l'estimation",
                                           verbatimTextOutput("arima_results")
                                       ),
                                       box(width = NULL, title = "Graphique des résidus",
                                           plotOutput("arima_residuals_plot")
                                       )
                                ),
                                column(width = 4,
                                       box(width = NULL, title = "Sélection automatique",
                                           checkboxInput("auto_arima", "Utiliser auto.arima()", value = FALSE),
                                           conditionalPanel(
                                             condition = "input.auto_arima == true",
                                             sliderInput("max_p", "p max:", min = 0, max = 5, value = 5),
                                             sliderInput("max_q", "q max:", min = 0, max = 5, value = 5),
                                             sliderInput("max_P", "P max:", min = 0, max = 2, value = 2),
                                             sliderInput("max_Q", "Q max:", min = 0, max = 2, value = 2),
                                             checkboxInput("stepwise", "Méthode par étapes", value = TRUE),
                                             checkboxInput("approximation", "Utiliser l'approximation", value = FALSE)
                                           )
                                       ),
                                       box(width = NULL, title = "Explications",
                                           h5("ARIMA(p,d,q)"),
                                           p("p: Ordre du processus autorégressif (AR)"),
                                           p("d: Ordre d'intégration ou de différenciation"),
                                           p("q: Ordre du processus de moyenne mobile (MA)"),
                                           h5("SARIMA(P,D,Q)s"),
                                           p("P: Ordre du processus AR saisonnier"),
                                           p("D: Ordre d'intégration saisonnière"),
                                           p("Q: Ordre du processus MA saisonnier"),
                                           p("s: Période saisonnière")
                                       )
                                )
                              )
                     ),
                     tabPanel("Modèles exponentiels",
                              fluidRow(
                                column(width = 8,
                                       fluidRow(
                                         column(width = 12,
                                                box(width = NULL, title = "Type de modèle exponentiel",
                                                    selectInput("exp_model_type", "Sélectionner un modèle:",
                                                                choices = c("Lissage exponentiel simple" = "ses",
                                                                            "Holt (tendance)" = "holt",
                                                                            "Holt-Winters (tendance + saisonnalité)" = "hw"),
                                                                selected = "ses"),
                                                    conditionalPanel(
                                                      condition = "input.exp_model_type == 'hw'",
                                                      selectInput("hw_seasonal", "Type de saisonnalité:",
                                                                  choices = c("Additive" = "additive",
                                                                              "Multiplicative" = "multiplicative"),
                                                                  selected = "additive")
                                                    ),
                                                    actionButton("estimate_exp", "Estimer le modèle",  class = "btn-primary")
                                                )
                                         )
                                       ),
                                       box(width = NULL, title = "Résultats de l'estimation",
                                           verbatimTextOutput("exp_results")
                                       ),
                                       box(width = NULL, title = "Graphique d'ajustement",
                                           plotOutput("exp_fit_plot")
                                       )
                                ),
                                column(width = 4,
                                       box(width = NULL, title = "Explications des modèles exponentiels",
                                           h5("Lissage exponentiel simple"),
                                           p("Modèle utilisé pour les séries sans tendance ni saisonnalité. Il attribue des poids exponentiellement décroissants aux observations passées."),
                                           h5("Méthode de Holt"),
                                           p("Extension du lissage exponentiel simple qui prend en compte la tendance dans la série temporelle."),
                                           h5("Méthode de Holt-Winters"),
                                           p("Extension qui gère à la fois la tendance et la saisonnalité."),
                                           h5("Types de saisonnalité"),
                                           p("Additive: l'amplitude de la saisonnalité est constante"),
                                           p("Multiplicative: l'amplitude de la saisonnalité varie avec le niveau de la série")
                                       )
                                )
                              )
                     ),
                     # Dans la partie UI (tabPanel("Comparaison des modèles", ...))
                     tabPanel("Comparaison des modèles",
                              fluidRow(
                                column(width = 12,
                                       box(width = NULL, title = "Métriques de comparaison",
                                           DT::dataTableOutput("model_comparison_table")
                                       ),
                                       box(width = NULL, title = "Graphique de comparaison",
                                           plotOutput("model_comparison_plot")
                                       )
                                )
                              )
                     )
              )
      ),

      # Page des tests de significativité
      tabItem(tabName = "significance",
              h3("Tests de significativité et diagnostic des modèles", align = "center"),
              tabBox(width = 12,
                     tabPanel("Tests sur les résidus",
                              fluidRow(
                                column(width = 8,
                                       fluidRow(
                                         column(width = 6,
                                                box(width = NULL, title = "Tests disponibles",
                                                    checkboxInput("ljung_box", "Test de Ljung-Box (autocorrélation)", value = TRUE),
                                                    checkboxInput("jarque_bera", "Test de Jarque-Bera (normalité)", value = TRUE),
                                                    checkboxInput("shapiro", "Test de Shapiro-Wilk (normalité)", value = TRUE),
                                                    checkboxInput("arch", "Test ARCH (hétéroscédasticité)", value = TRUE),
                                                    sliderInput("sign_level", "Niveau de significativité (alpha):",  min = 0.01, max = 0.1, value = 0.05, step = 0.01)
                                                )
                                         ),
                                         column(width = 6,
                                                box(width = NULL, title = "Modèle à tester",
                                                    selectInput("model_to_test", "Sélectionner un modèle estimé:",
                                                                choices = c("Aucun modèle disponible" = "")),
                                                    actionButton("run_tests", "Lancer les tests",  class = "btn-primary")
                                                )
                                         )
                                       ),
                                       box(width = NULL, title = "Résultats des tests",
                                           verbatimTextOutput("test_results")
                                       ),
                                       box(width = NULL, title = "Graphiques de diagnostic",
                                           plotOutput("diagnostic_plots")
                                       )
                                ),
                                column(width = 4,
                                       box(width = NULL, title = "Explications des tests",
                                           h5("Test de Ljung-Box"),
                                           p("Teste l'hypothèse nulle d'absence d'autocorrélation dans les résidus jusqu'à un certain ordre."),
                                           h5("Test de Jarque-Bera"),
                                           p("Teste si les résidus suivent une distribution normale en examinant l'asymétrie et l'aplatissement."),
                                           h5("Test de Shapiro-Wilk"),
                                           p("Test de normalité basé sur la corrélation entre les résidus et leurs scores normaux correspondants."),
                                           h5("Test ARCH"),
                                           p("Teste la présence d'hétéroscédasticité conditionnelle autorégressive, indiquant si la variance des résidus change au fil du temps.")
                                       )
                                )
                              )
                     ),
                     tabPanel("Significativité des coefficients",
                              fluidRow(
                                column(width = 8,
                                       box(width = NULL, title = "Tableau des coefficients",
                                           verbatimTextOutput("coefficients_table")
                                       ),
                                       box(width = NULL, title = "Interprétation des coefficients",
                                           verbatimTextOutput("coef_interpretation")
                                       )
                                ),
                                column(width = 4,
                                       box(width = NULL, title = "Comment interpréter",
                                           h5("Coefficients"),
                                           p("Représentent l'impact des termes autorégressifs (AR) ou de moyenne mobile (MA) sur la série."),
                                           h5("Erreur standard"),
                                           p("Mesure la précision de l'estimation du coefficient."),
                                           h5("Valeur t"),
                                           p("Rapport entre le coefficient et son erreur standard."),
                                           h5("p-value"),
                                           p("Probabilité d'observer une valeur t aussi extrême si le coefficient était réellement nul."),
                                           h5("Interprétation"),
                                           p("Si p-value < alpha (typiquement 0.05), le coefficient est considéré comme statistiquement significatif.")
                                       )
                                )
                              )
                     )
              )
      ),

      # Page des prévisions
      tabItem(tabName = "forecast",
              h3("Prévisions et intervalles de confiance", align = "center"),
              fluidRow(
                column(width = 8,
                       fluidRow(
                         column(width = 6,
                                box(width = NULL, title = "Paramètres de prévision",
                                    numericInput("forecast_horizon", "Horizon de prévision:",  value = 12, min = 1, max = 100),
                                    sliderInput("conf_level", "Niveau de confiance:",  min = 0.8, max = 0.99, value = 0.95, step = 0.01),
                                    selectInput("forecast_model", "Modèle à utiliser pour la prévision:",
                                                choices = c("Aucun modèle disponible" = "")),
                                    actionButton("generate_forecast", "Générer les prévisions",  class = "btn-primary")
                                )
                         ),
                         column(width = 6,
                                box(width = NULL, title = "Options d'affichage",
                                    checkboxInput("show_original", "Afficher la série originale", value = TRUE),
                                    checkboxInput("show_fitted", "Afficher les valeurs ajustées", value = TRUE),
                                    checkboxInput("show_intervals", "Afficher les intervalles de confiance", value = TRUE),
                                    checkboxInput("log_scale", "Échelle logarithmique", value = FALSE)
                                )
                         )
                       ),
                       box(width = NULL, title = "Graphique des prévisions",
                           plotlyOutput("forecast_plot", height = "400px")
                       ),
                       box(width = NULL, title = "Tableau des prévisions",
                           DT::dataTableOutput("forecast_table")
                       )
                ),
                column(width = 4,
                       box(width = NULL, title = "Métriques de précision",
                           verbatimTextOutput("forecast_metrics")
                       ),
                       box(width = NULL, title = "Explications",
                           h5("Horizon de prévision"),
                           p("Nombre de périodes futures pour lesquelles des prévisions sont générées."),
                           h5("Intervalles de confiance"),
                           p("Plages de valeurs dans lesquelles les observations futures devraient se situer avec une probabilité définie."),
                           h5("Métriques de précision"),
                           p("ME: Erreur moyenne"),
                           p("RMSE: Racine carrée de l'erreur quadratique moyenne"),
                           p("MAE: Erreur absolue moyenne"),
                           p("MAPE: Erreur absolue moyenne en pourcentage"),
                           p("MASE: Erreur absolue moyenne mise à l'échelle")
                       ),
                       box(width = NULL, title = "Télécharger les prévisions",
                           downloadButton("download_forecast", "Télécharger (CSV)")
                       )
                )
              )
      ),
      tabItem(tabName = "report",
              h3("Génération de rapport", align = "center"),
              fluidRow(
                box(width = 6, status = "primary", solidHeader = TRUE, title = "Paramètres du rapport",
                    textInput("report_title", "Titre du rapport :", value = "Rapport sur les séries temporelles"),
                    textInput("nom_auteur", "Votre nom :", value = "Nom de l'utilisateur"),
                    selectInput("format_rapport", "Format du rapport :", 
                                choices = c("HTML" = "html_document", 
                                            "Word" = "word_document"))
                ),
                box(width = 6, status = "info", solidHeader = TRUE, title = "Téléchargement",
                    p("Cliquez sur le bouton ci-dessous pour générer et télécharger votre rapport personnalisé."),
                    downloadButton("download_report", "Télécharger le rapport", class = "btn-success")
                )
              ),
              box(width = 12, title = "Contenu du rapport", collapsible = TRUE,
                  p("Le rapport inclura un résumé du modèle estimé, les prévisions générées (si disponibles), et les paramètres utilisés."),
                  p("Le format peut être HTML ou Word selon votre choix.")
              )
      )
      
    )
  ))

# Server
server <- function(input, output, session) {
  # Réactifs pour stocker les données et modèles
  data_store <- reactiveVal(NULL)
  ts_data <- reactiveVal(NULL)
  models_list <- reactiveValues(arima = NULL, exp = NULL, auto_arima = NULL)
  forecasts_list <- reactiveValues()
  comparison_metrics <- reactiveVal(NULL)
  diff_series <- reactiveVal(NULL)
  
  observe({
    req(input$upload_data)
    ext <- tools::file_ext(input$upload_data$name)
    
    if (ext %in% c("xls", "xlsx")) {
      sheets <- tryCatch({
        excel_sheets(input$upload_data$datapath)
      }, error = function(e) {
        showNotification("Impossible de lire les feuilles du fichier Excel.", type = "error")
        return(NULL)
      })
      
      if (!is.null(sheets)) {
        output$sheet_selector <- renderUI({
          selectInput("selected_sheet", "Choisir la feuille:", choices = sheets, selected = sheets[1])
        })
      }
    } else {
      output$sheet_selector <- renderUI(NULL)
    }
    
    if (ext == "csv") {
      output$csv_sep_selector <- renderUI({
        selectInput("csv_sep", "Séparateur CSV :", choices = c("Virgule" = ",", "Point-virgule" = ";", "Tabulation" = "\t"), selected = ",")
      })
    } else {
      output$csv_sep_selector <- renderUI(NULL)
    }
  })
  # Modifiez le chargement des données pour prendre en compte la feuille sélectionnée
  observe({
    req(input$upload_data)
    inFile <- input$upload_data
    ext <- tools::file_ext(inFile$name)
    
    df <- tryCatch({
      switch(ext,
             "csv" = {
               sep <- if (!is.null(input$csv_sep)) input$csv_sep else ","
               read.csv(inFile$datapath, sep = sep, stringsAsFactors = FALSE)
             },
             "xls" = {
               sheet <- if (!is.null(input$selected_sheet)) input$selected_sheet else excel_sheets(inFile$datapath)[1]
               read_excel(inFile$datapath, sheet = sheet)
             },
             "xlsx" = {
               sheet <- if (!is.null(input$selected_sheet)) input$selected_sheet else excel_sheets(inFile$datapath)[1]
               read_excel(inFile$datapath, sheet = sheet)
             },
             "sav" = read_spss(inFile$datapath),
             "dta" = read_dta(inFile$datapath),
             "rds" = readRDS(inFile$datapath),
             {
               showNotification("Format de fichier non pris en charge.", type = "error")
               return(NULL)
             }
      )
    }, error = function(e) {
      showNotification(paste("Erreur lors de la lecture du fichier :", e$message), type = "error")
      return(NULL)
    })
    
    if (!is.null(df)) {
      data_store(df)
      ts_data(NULL)
      models_list$arima <- NULL
      models_list$exp <- NULL
      models_list$auto_arima <- NULL
      forecasts_list <- reactiveValues()
      comparison_metrics(NULL)
      diff_series(NULL)
      showNotification("Données chargées avec succès.", type = "message")
    } else {
      showNotification("Les données n'ont pas pu être chargées.", type = "error")
    }
  })
  # Interface pour sélectionner les variables
  output$var_selector <- renderUI({
    req(data_store())
    df <- data_store()
    
    numeric_cols <- sapply(df, is.numeric)
    choices <- names(df)[numeric_cols]
    
    selectInput("choix_var_col", "Sélectionner une variable numérique:", choices = choices)
  })
  
  # Création de la série temporelle sécurisée
  observe({
    req(data_store(), input$choix_var_col, input$freq_series)
    
    df <- data_store()
    var_name <- input$choix_var_col
    freq <- input$freq_series
    
    # Vérification que la variable existe et contient des données valides
    if (!(var_name %in% names(df))) {
      showNotification("La variable sélectionnée n'existe pas dans les données.", type = "error")
      return()
    }
    
    series <- df[[var_name]]
    
    # Nettoyage : suppression des NA
    series <- na.omit(series)
    
    # Vérifier qu'on a des observations valides
    if (length(series) < 2) {
      showNotification("La variable sélectionnée ne contient pas suffisamment de données numériques valides.", type = "error")
      ts_data(NULL)
      return()
    }
    
    # Conversion en série temporelle
    ts_obj <- tryCatch({
      ts(series, frequency = freq)
    }, error = function(e) {
      showNotification("Impossible de créer la série temporelle : données invalides.", type = "error")
      return(NULL)
    })
    
    if (!is.null(ts_obj)) {
      ts_data(ts_obj)
      
      # Réinitialiser les choix de modèles
      updateSelectInput(session, "model_to_test", choices = c("Aucun modèle disponible" = ""))
      updateSelectInput(session, "forecast_model", choices = c("Aucun modèle disponible" = ""))
    }
  })
  
  
  # Aperçu des données
  output$data_preview <- DT::renderDataTable({
    req(data_store())
    datatable(head(data_store(), 10), options = list(scrollX = TRUE))
  })

  # Graphique de la série
  output$graph_serie <- renderPlotly({
    req(ts_data())
    series <- ts_data()
    var_name <- input$choix_var_col

    df_plot <- data.frame(time = time(series), value = as.numeric(series))

    p <- plot_ly(df_plot, x = ~time, y = ~value, type = 'scatter', mode = 'lines', name = var_name) %>%
      layout(title = paste("Série temporelle:", var_name),
             xaxis = list(title = "Temps"),
             yaxis = list(title = var_name))

    return(p)
  })

  # Graphique ACF
  output$graph_acf <- renderPlotly({
    req(ts_data(), input$lag_acf, input$alpha_acf)
    series <- ts_data()
    lag_max <- input$lag_acf
    alpha <- input$alpha_acf

    # Calcul ACF
    acf_result <- acf(series, lag.max = lag_max, plot = FALSE)
    acf_values <- acf_result$acf

    # Calcul des intervalles de confiance
    T <- length(series)
    q_acf <- qnorm(1 - alpha/2)
    acf_values_carree <- cumsum(acf_values^2)
    factors <- sqrt((1 / T) * (1 + 2 * acf_values_carree))
    conf_upper <- q_acf * factors
    conf_lower <- -conf_upper

    # Création du graphique
    p <- plot_ly() %>%
      add_bars(x = as.numeric(acf_result$lag), y = acf_values, name = "ACF", marker = list(color = 'blue')) %>%
      add_trace(x = as.numeric(acf_result$lag), y = conf_upper, type = 'scatter', mode = 'lines',
                line = list(color = 'red', dash = 'dot'), showlegend = FALSE) %>%
      add_trace(x = as.numeric(acf_result$lag), y = conf_lower, type = 'scatter', mode = 'lines',
                line = list(color = 'red', dash = 'dot'), showlegend = FALSE) %>%
      layout(title = "Fonction d'Autocorrélation (ACF)",
             xaxis = list(title = "Lag"),
             yaxis = list(title = "ACF"))

    return(p)
  })

  # Graphique PACF
  output$graph_pacf <- renderPlotly({
    req(ts_data(), input$lag_pacf, input$alpha_pacf)
    series <- ts_data()
    lag_max <- input$lag_pacf
    alpha <- input$alpha_pacf

    # Calcul PACF
    pacf_result <- pacf(series, lag.max = lag_max, plot = FALSE)
    pacf_values <- pacf_result$acf

    # Calcul des intervalles de confiance
    T <- length(series)
    q_pacf <- qnorm(1 - alpha/2)
    conf_limit <- q_pacf / sqrt(T)
    conf_upper <- rep(conf_limit, length(pacf_values))
    conf_lower <- rep(-conf_limit, length(pacf_values))

    # Création du graphique
    p <- plot_ly() %>%
      add_bars(x = as.numeric(pacf_result$lag), y = pacf_values, name = "PACF", marker = list(color = 'blue')) %>%
      add_trace(x = as.numeric(pacf_result$lag), y = conf_upper, type = 'scatter', mode = 'lines',
                line = list(color = 'red', dash = 'dot'), showlegend = FALSE) %>%
      add_trace(x = as.numeric(pacf_result$lag), y = conf_lower, type = 'scatter', mode = 'lines',
                line = list(color = 'red', dash = 'dot'), showlegend = FALSE) %>%
      layout(title = "Fonction d'Autocorrélation Partielle (PACF)",
             xaxis = list(title = "Lag"),
             yaxis = list(title = "PACF"))

    return(p)
  })

  # Test de stationnarité
  output$stationnarite_results <- renderPrint({
    req(ts_data(), input$test_stationnarite, input$alpha_stationnarite)
    series <- ts_data()
    test_type <- input$test_stationnarite
    alpha <- input$alpha_stationnarite

    if (test_type == "ADF") {
      test_result <- adf.test(series)
      cat("Test Augmented Dickey-Fuller (ADF)\n")
      cat("-------------------------------\n")
      cat("Hypothèse nulle: La série a une racine unitaire (non stationnaire)\n")
      cat("Statistique de test:", test_result$statistic, "\n")
      cat("p-value:", test_result$p.value, "\n")
      cat("Valeur critique (", alpha, "):", qnorm(alpha), "\n")
      cat("Conclusion:", ifelse(test_result$p.value < alpha,
                                "Rejet de H0: La série est stationnaire",
                                "Non rejet de H0: La série est non stationnaire"), "\n")
    } else if (test_type == "PP") {
      test_result <- pp.test(series)
      cat("Test Phillips-Perron (PP)\n")
      cat("-------------------------\n")
      cat("Hypothèse nulle: La série a une racine unitaire (non stationnaire)\n")
      cat("Statistique de test:", test_result$statistic, "\n")
      cat("p-value:", test_result$p.value, "\n")
      cat("Valeur critique (", alpha, "):", qnorm(alpha), "\n")
      cat("Conclusion:", ifelse(test_result$p.value < alpha,
                                "Rejet de H0: La série est stationnaire",
                                "Non rejet de H0: La série est non stationnaire"), "\n")
    } else if (test_type == "KPSS") {
      test_result <- kpss.test(series)
      cat("Test KPSS\n")
      cat("---------\n")
      cat("Hypothèse nulle: La série est stationnaire\n")
      cat("Statistique de test:", test_result$statistic, "\n")
      cat("p-value:", test_result$p.value, "\n")
      cat("Valeur critique (", alpha, "):", qnorm(1-alpha), "\n")
      cat("Conclusion:", ifelse(test_result$p.value < alpha,
                                "Rejet de H0: La série est non stationnaire",
                                "Non rejet de H0: La série est stationnaire"), "\n")
    }
  })

  # Décomposition saisonnière
  output$seasonality_decomp <- renderPlot({
    req(ts_data(), input$decomp_method)
    series <- ts_data()
    method <- input$decomp_method

    # Vérifier si la fréquence est suffisante pour la décomposition
    if (frequency(series) <= 1) {
      plot(series, main = "Série non saisonnière (fréquence <= 1)")
      return()
    }

    # Décomposition de la série
    decomp <- decompose(series, type = method)

    # Afficher la décomposition
    plot(decomp, xlab = "Temps")
  })

  # Test de saisonnalité
  output$seasonality_test_results <- renderPrint({
    req(ts_data(), input$test_seasonality)
    series <- ts_data()

    # Si la fréquence est insuffisante pour un test de saisonnalité
    if (frequency(series) <= 1) {
      cat("Impossible de tester la saisonnalité - fréquence de la série trop faible (", frequency(series), ")\n")
      return()
    }

    freq <- frequency(series)
    n <- length(series)

    # Test de Friedman pour la saisonnalité
    cat("Test de saisonnalité\n")
    cat("-------------------\n")

    # Organiser les données par saison
    seasons <- as.factor(cycle(series))
    years <- as.factor(floor(time(series)))

    # Implémenter un test simple basé sur l'analyse de variance par saison
    seasonal_model <- try(aov(as.numeric(series) ~ seasons))

    if (!inherits(seasonal_model, "try-error")) {
      summary_result <- summary(seasonal_model)
      cat("Analyse de variance par saison:\n")
      print(summary_result)

      F_value <- summary_result[[1]]["seasons", "F value"]
      p_value <- summary_result[[1]]["seasons", "Pr(>F)"]

      cat("\nConclusion:\n")
      if (p_value < 0.05) {
        cat("Présence d'effet saisonnier (p-value <", p_value, ")\n")
      } else {
        cat("Pas d'effet saisonnier significatif (p-value =", p_value, ")\n")
      }
    } else {
      cat("Impossible de réaliser le test - probablement des données insuffisantes\n")
    }
  })

  # Gestion de la différenciation
  observeEvent(input$apply_diff, {
    req(ts_data(), input$diff_order, input$diff_seasonal_order)
    series <- ts_data()
    d <- input$diff_order
    D <- input$diff_seasonal_order

    # Différenciation simple
    series_diff <- series
    if (d > 0) {
      for (i in 1:d) {
        series_diff <- diff(series_diff)
      }
    }

    # Différenciation saisonnière
    if (D > 0 && frequency(series) > 1) {
      for (i in 1:D) {
        series_diff <- diff(series_diff, lag = frequency(series))
      }
    }

    diff_series(series_diff)
  })

  # Graphique de la série différenciée
  output$diff_series_plot <- renderPlotly({
    req(diff_series())
    series_diff <- diff_series()

    df_plot <- data.frame(time = time(series_diff), value = as.numeric(series_diff))

    p <- plot_ly(df_plot, x = ~time, y = ~value, type = 'scatter', mode = 'lines', name = "Série différenciée") %>%
      layout(title = "Série après différenciation",
             xaxis = list(title = "Temps"),
             yaxis = list(title = "Valeur différenciée"))

    return(p)
  })

  # Tests sur série différenciée
  output$diff_test_results <- renderPrint({
    req(diff_series(), input$test_diff_adf || input$test_diff_kpss)
    series_diff <- diff_series()

    cat("Tests de stationnarité sur la série différenciée\n")
    cat("----------------------------------------------\n")

    if (input$test_diff_adf) {
      test_result <- try(adf.test(series_diff))
      if (!inherits(test_result, "try-error")) {
        cat("\nTest Augmented Dickey-Fuller (ADF):\n")
        cat("Statistique de test:", test_result$statistic, "\n")
        cat("p-value:", test_result$p.value, "\n")
        cat("Conclusion:", ifelse(test_result$p.value < 0.05,
                                  "La série différenciée est stationnaire",
                                  "La série différenciée n'est pas stationnaire"), "\n")
      } else {
        cat("\nImpossible de réaliser le test ADF - probablement des données insuffisantes\n")
      }
    }

    if (input$test_diff_kpss) {
      test_result <- try(kpss.test(series_diff))
      if (!inherits(test_result, "try-error")) {
        cat("\nTest KPSS:\n")
        cat("Statistique de test:", test_result$statistic, "\n")
        cat("p-value:", test_result$p.value, "\n")
        cat("Conclusion:", ifelse(test_result$p.value < 0.05,
                                  "La série différenciée n'est pas stationnaire",
                                  "La série différenciée est stationnaire"), "\n")
      } else {
        cat("\nImpossible de réaliser le test KPSS - probablement des données insuffisantes\n")
      }
    }
  })

  # Estimation du modèle ARIMA
  observeEvent(input$estimate_arima, {
    req(ts_data())
    series <- ts_data()

    if (input$auto_arima) {
      # Utilisation de auto.arima()
      withProgress(message = "Estimation auto.arima en cours...", {
        model <- auto.arima(series,
                            max.p = input$max_p, max.q = input$max_q,
                            max.P = input$max_P, max.Q = input$max_Q,
                            stepwise = input$stepwise, approximation = input$approximation)
        models_list$auto_arima <- model

        # Mettre à jour les listes des modèles disponibles
        updateSelectInput(session, "model_to_test",
                          choices = c("ARIMA auto" = "auto_arima",
                                      if (!is.null(models_list$arima)) "ARIMA manuel" = "arima",
                                      if (!is.null(models_list$exp)) "Exponentiel" = "exp"))

        updateSelectInput(session, "forecast_model",
                          choices = c("ARIMA auto" = "auto_arima",
                                      if (!is.null(models_list$arima)) "ARIMA manuel" = "arima",
                                      if (!is.null(models_list$exp)) "Exponentiel" = "exp"))
      })
    } else {
      # Estimation manuelle du modèle ARIMA/SARIMA
      p <- input$arima_p
      d <- input$arima_d
      q <- input$arima_q
      P <- input$sarima_P
      D <- input$sarima_D
      Q <- input$sarima_Q
      period <- frequency(series)

      withProgress(message = "Estimation ARIMA en cours...", {
        model <- try(Arima(series, order = c(p, d, q),
                           seasonal = list(order = c(P, D, Q), period = period)))

        if (!inherits(model, "try-error")) {
          models_list$arima <- model

          # Mettre à jour les listes des modèles disponibles
          updateSelectInput(session, "model_to_test",
                            choices = c("ARIMA manuel" = "arima",
                                        if (!is.null(models_list$auto_arima)) "ARIMA auto" = "auto_arima",
                                        if (!is.null(models_list$exp)) "Exponentiel" = "exp"))

          updateSelectInput(session, "forecast_model",
                            choices = c("ARIMA manuel" = "arima",
                                        if (!is.null(models_list$auto_arima)) "ARIMA auto" = "auto_arima",
                                        if (!is.null(models_list$exp)) "Exponentiel" = "exp"))
        } else {
          showNotification("Erreur dans l'estimation du modèle ARIMA. Vérifiez les paramètres.", type = "error")
        }
      })
    }
  })

  # Affichage des résultats ARIMA
  output$arima_results <- renderPrint({
    if (input$auto_arima && !is.null(models_list$auto_arima)) {
      cat("Résultats du modèle ARIMA automatique\n")
      cat("-----------------------------------\n")
      print(summary(models_list$auto_arima))
      cat("\nCritères d'information:\n")
      cat("AIC:", models_list$auto_arima$aic, "\n")
      cat("BIC:", models_list$auto_arima$bic, "\n")
      cat("Log-vraisemblance:", models_list$auto_arima$loglik, "\n")
    } else if (!is.null(models_list$arima)) {
      cat("Résultats du modèle ARIMA manuel\n")
      cat("------------------------------\n")
      print(summary(models_list$arima))
      cat("\nCritères d'information:\n")
      cat("AIC:", models_list$arima$aic, "\n")
      cat("BIC:", models_list$arima$bic, "\n")
      cat("Log-vraisemblance:", models_list$arima$loglik, "\n")
    } else {
      cat("Aucun modèle ARIMA estimé. Utilisez le bouton 'Estimer le modèle'.")
    }
  })

  # Graphique des résidus ARIMA
  output$arima_residuals_plot <- renderPlot({
    model <- NULL
    if (input$auto_arima && !is.null(models_list$auto_arima)) {
      model <- models_list$auto_arima
    } else if (!is.null(models_list$arima)) {
      model <- models_list$arima
    }

    if (!is.null(model)) {
      par(mfrow = c(2, 2))
      # Plot des résidus
      plot(model$residuals, main = "Résidus", ylab = "Résidus")
      abline(h = 0, col = "red")

      # Histogramme des résidus
      hist(model$residuals, main = "Histogramme des résidus", xlab = "Résidus")

      # ACF des résidus
      acf(model$residuals, main = "ACF des résidus")

      # QQ-plot des résidus
      qqnorm(model$residuals)
      qqline(model$residuals, col = 2)
    } else {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Aucun modèle ARIMA estimé.")
    }
  })

  # Estimation des modèles exponentiels
  observeEvent(input$estimate_exp, {
    req(ts_data(), input$exp_model_type)
    series <- ts_data()
    model_type <- input$exp_model_type

    withProgress(message = "Estimation du modèle exponentiel en cours...", {
      if (model_type == "ses") {
        model <- ses(series, h = 1)
      } else if (model_type == "holt") {
        model <- holt(series, h = 1)
      } else if (model_type == "hw") {
        model <- hw(series, seasonal = input$hw_seasonal, h = 1)
      }

      models_list$exp <- model

      # Mettre à jour les listes des modèles disponibles
      updateSelectInput(session, "model_to_test",
                        choices = c("Exponentiel" = "exp",
                                    if (!is.null(models_list$arima)) "ARIMA manuel" = "arima",
                                    if (!is.null(models_list$auto_arima)) "ARIMA auto" = "auto_arima"))

      updateSelectInput(session, "forecast_model",
                        choices = c("Exponentiel" = "exp",
                                    if (!is.null(models_list$arima)) "ARIMA manuel" = "arima",
                                    if (!is.null(models_list$auto_arima)) "ARIMA auto" = "auto_arima"))
    })
  })

  # Affichage des résultats des modèles exponentiels
  output$exp_results <- renderPrint({
    req(models_list$exp)
    model <- models_list$exp

    cat("Résultats du modèle de lissage exponentiel\n")
    cat("----------------------------------------\n")
    print(summary(model))

    # Afficher les paramètres du modèle
    cat("\nParamètres du modèle:\n")
    if (inherits(model, "ses")) {
      cat("Alpha (niveau):", model$model$par["alpha"], "\n")
    } else if (inherits(model, "holt")) {
      cat("Alpha (niveau):", model$model$par["alpha"], "\n")
      cat("Beta (tendance):", model$model$par["beta"], "\n")
    } else if (inherits(model, "hw")) {
      cat("Alpha (niveau):", model$model$par["alpha"], "\n")
      cat("Beta (tendance):", model$model$par["beta"], "\n")
      cat("Gamma (saisonnalité):", model$model$par["gamma"], "\n")
    }

    # Mesures de précision
    cat("\nMesures de précision:\n")
    accuracy_measures <- accuracy(model)
    print(accuracy_measures)
  })

  # Graphique d'ajustement du modèle exponentiel
  output$exp_fit_plot <- renderPlot({
    req(models_list$exp, ts_data())
    model <- models_list$exp
    series <- ts_data()

    # Plot du modèle avec les données originales
    plot(model, main = "Modèle exponentiel et données originales",
         xlab = "Temps", ylab = "Valeurs",
         fcol = "blue", type = "o")
    lines(series, col = "black", type = "o")
    legend("topleft", legend = c("Données originales", "Valeurs ajustées"),
           col = c("black", "blue"), lty = 1, pch = 1)
  })

  # Comparaison des modèles
  observe({
    # Créer un data frame pour comparer les modèles
    models_to_compare <- list()
    metrics <- data.frame(Model = character(0), AIC = numeric(0), BIC = numeric(0),
                          RMSE = numeric(0), MAE = numeric(0), MAPE = numeric(0))

    if (!is.null(models_list$arima)) {
      arima_acc <- accuracy(models_list$arima)
      metrics <- rbind(metrics, data.frame(
        Model = "ARIMA manuel",
        AIC = models_list$arima$aic,
        BIC = models_list$arima$bic,
        RMSE = arima_acc[1, "RMSE"],
        MAE = arima_acc[1, "MAE"],
        MAPE = arima_acc[1, "MAPE"]
      ))
    }

    if (!is.null(models_list$auto_arima)) {
      auto_acc <- accuracy(models_list$auto_arima)
      metrics <- rbind(metrics, data.frame(
        Model = "ARIMA auto",
        AIC = models_list$auto_arima$aic,
        BIC = models_list$auto_arima$bic,
        RMSE = auto_acc[1, "RMSE"],
        MAE = auto_acc[1, "MAE"],
        MAPE = auto_acc[1, "MAPE"]
      ))
    }

    if (!is.null(models_list$exp)) {
      exp_acc <- accuracy(models_list$exp)
      # Les modèles exponentiels n'ont pas AIC/BIC directs, utilisation de NA
      metrics <- rbind(metrics, data.frame(
        Model = "Exponentiel",
        AIC = NA,
        BIC = NA,
        RMSE = exp_acc[1, "RMSE"],
        MAE = exp_acc[1, "MAE"],
        MAPE = exp_acc[1, "MAPE"]
      ))
    }

    if (nrow(metrics) > 0) {
      comparison_metrics(metrics)
    }
  })

  # Tableau de comparaison des modèles
  output$model_comparison_table <- DT::renderDataTable({
    req(comparison_metrics())
    metrics <- comparison_metrics()

    # Formater les valeurs numériques
    metrics$AIC <- round(metrics$AIC, 2)
    metrics$BIC <- round(metrics$BIC, 2)
    metrics$RMSE <- round(metrics$RMSE, 4)
    metrics$MAE <- round(metrics$MAE, 4)
    metrics$MAPE <- round(metrics$MAPE, 2)

    DT::datatable(metrics, options = list(pageLength = 10))
  })

  # Graphique de comparaison des modèles
  output$model_comparison_plot <- renderPlot({
    req(comparison_metrics(), ts_data())
    metrics <- comparison_metrics()
    series <- ts_data()

    # Si nous avons des modèles à comparer
    if (nrow(metrics) > 0) {
      # Créer un data frame pour stocker les valeurs ajustées
      time_points <- time(series)
      fitted_df <- data.frame(Time = time_points, Original = as.numeric(series))

      # Ajouter les valeurs ajustées de chaque modèle
      if (!is.null(models_list$arima)) {
        fitted_df$ARIMA_manuel <- as.numeric(fitted(models_list$arima))
      }
      if (!is.null(models_list$auto_arima)) {
        fitted_df$ARIMA_auto <- as.numeric(fitted(models_list$auto_arima))
      }
      if (!is.null(models_list$exp)) {
        # Pour les modèles exponentiels, nous devons extraire les valeurs ajustées
        fitted_values <- fitted(models_list$exp)
        # S'assurer que la longueur correspond
        if (length(fitted_values) == length(series)) {
          fitted_df$Exponentiel <- as.numeric(fitted_values)
        }
      }

      # Tracer les séries
      matplot(fitted_df$Time, fitted_df[, -1], type = "l",
              col = c("black", "blue", "red", "green"), lty = 1, lwd = 2,
              xlab = "Temps", ylab = "Valeurs", main = "Comparaison des modèles")
      legend("topleft", legend = colnames(fitted_df)[-1],
             col = c("black", "blue", "red", "green"), lty = 1, lwd = 2)
    }
  })

  # Génération des prévisions
  observeEvent(input$generate_forecast, {
    req(input$forecast_model, input$forecast_horizon)

    model_name <- input$forecast_model
    h <- input$forecast_horizon
    level <- input$conf_level * 100

    model <- NULL
    if (model_name == "arima") {
      model <- models_list$arima
    } else if (model_name == "auto_arima") {
      model <- models_list$auto_arima
    } else if (model_name == "exp") {
      model <- models_list$exp
    }

    if (!is.null(model)) {
      fc <- forecast(model, h = h, level = level)
      forecasts_list[[model_name]] <- fc
    } else {
      showNotification("Aucun modèle sélectionné pour la prévision.", type = "error")
    }
  })

  # Graphique de prévision
  output$forecast_plot <- renderPlotly({
    req(input$forecast_model, forecasts_list[[input$forecast_model]])
    forecast_obj <- forecasts_list[[input$forecast_model]]

    # Préparer les données pour plotly
    fcast_df <- data.frame(Time = time(forecast_obj$mean),
                           Forecast = as.numeric(forecast_obj$mean),
                           Lower = as.numeric(forecast_obj$lower[,1]),
                           Upper = as.numeric(forecast_obj$upper[,1]))

    p <- plot_ly()

    if (input$show_original) {
      p <- p %>% add_lines(x = time(ts_data()), y = as.numeric(ts_data()),
                           name = "Série originale", line = list(color = "black"))
    }
    if (input$show_fitted) {
      model <- NULL
      if (input$forecast_model == "arima") {
        model <- models_list$arima
      } else if (input$forecast_model == "auto_arima") {
        model <- models_list$auto_arima
      } else if (input$forecast_model == "exp") {
        model <- models_list$exp
      }
      if (!is.null(model)) {
        fitted_values <- fitted(model)
        p <- p %>% add_lines(x = time(fitted_values), y = as.numeric(fitted_values),
                             name = "Ajustements", line = list(color = "blue", dash = "dot"))
      }
    }

    p <- p %>% add_lines(x = fcast_df$Time, y = fcast_df$Forecast,
                         name = "Prévision", line = list(color = "red"))

    if (input$show_intervals) {
      p <- p %>% add_ribbons(x = fcast_df$Time, ymin = fcast_df$Lower, ymax = fcast_df$Upper,
                             name = "Intervalle de confiance", line = list(color = 'rgba(7, 164, 181, 0.05)'),
                             fillcolor = 'rgba(7, 164, 181, 0.2)')
    }

    p <- p %>% layout(title = "Prévisions",
                      xaxis = list(title = "Temps"),
                      yaxis = list(title = "Valeurs"),
                      yaxis = if (input$log_scale) list(type = "log") else list())

    return(p)
  })

  # Tableau des prévisions
  output$forecast_table <- DT::renderDataTable({
    req(input$forecast_model, forecasts_list[[input$forecast_model]])
    forecast_obj <- forecasts_list[[input$forecast_model]]

    df <- data.frame(Time = time(forecast_obj$mean),
                     Prévision = as.numeric(forecast_obj$mean),
                     Lower = as.numeric(forecast_obj$lower[,1]),
                     Upper = as.numeric(forecast_obj$upper[,1]))

    datatable(df, options = list(pageLength = 10))
  })

  # Métriques de prévision
  output$forecast_metrics <- renderPrint({
    req(input$forecast_model, forecasts_list[[input$forecast_model]])
    fc <- forecasts_list[[input$forecast_model]]
    print(accuracy(fc))
  })

  # Téléchargement prévision
  output$download_forecast <- downloadHandler(
    filename = function() {
      paste0("previsions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$forecast_model, forecasts_list[[input$forecast_model]])
      forecast_obj <- forecasts_list[[input$forecast_model]]

      df <- data.frame(Time = time(forecast_obj$mean),
                       Prévision = as.numeric(forecast_obj$mean),
                       Lower = as.numeric(forecast_obj$lower[,1]),
                       Upper = as.numeric(forecast_obj$upper[,1]))

      write.csv(df, file, row.names = FALSE)
    }
  )
  
  
  observe({
    # Mettre à jour la liste des modèles disponibles pour les tests
    model_choices <- c()
    
    if (!is.null(models_list$arima)) {
      model_choices <- c(model_choices, "ARIMA manuel" = "arima")
    }
    if (!is.null(models_list$auto_arima)) {
      model_choices <- c(model_choices, "ARIMA auto" = "auto_arima")
    }
    if (!is.null(models_list$exp)) {
      model_choices <- c(model_choices, "Modèle exponentiel" = "exp")
    }
    
    if (length(model_choices) == 0) {
      model_choices <- c("Aucun modèle disponible" = "")
    }
    
    updateSelectInput(session, "model_to_test", choices = model_choices)
  })
  
  # Gestion des tests de significativité
  observeEvent(input$run_tests, {
    req(input$model_to_test, input$model_to_test != "")
    
    model_name <- input$model_to_test
    alpha <- input$sign_level
    
    # Récupérer le modèle sélectionné
    model <- switch(model_name,
                    "arima" = models_list$arima,
                    "auto_arima" = models_list$auto_arima,
                    "exp" = models_list$exp)
    
    if (is.null(model)) {
      showNotification("Aucun modèle sélectionné pour les tests.", type = "error")
      return()
    }
    
    # Extraire les résidus
    if (inherits(model, "forecast")) {
      # Pour les modèles exponentiels
      residuals <- model$residuals
    } else {
      # Pour les modèles ARIMA
      residuals <- residuals(model)
    }
    
    # Supprimer les NA
    residuals <- na.omit(residuals)
    
    # Préparer les résultats des tests
    test_results <- list()
    
    # Test de Ljung-Box (autocorrélation)
    if (input$ljung_box) {
      lb_test <- Box.test(residuals, lag = 10, type = "Ljung-Box")
      test_results$LjungBox <- list(
        statistic = lb_test$statistic,
        p.value = lb_test$p.value,
        conclusion = ifelse(lb_test$p.value < alpha,
                            "Rejet de H0: Présence d'autocorrélation",
                            "Non rejet de H0: Pas d'autocorrélation détectée")
      )
    }
    
    # Test de Jarque-Bera (normalité)
    if (input$jarque_bera) {
      jb_test <- jarque.bera.test(residuals)
      test_results$JarqueBera <- list(
        statistic = jb_test$statistic,
        p.value = jb_test$p.value,
        conclusion = ifelse(jb_test$p.value < alpha,
                            "Rejet de H0: Les résidus ne suivent pas une loi normale",
                            "Non rejet de H0: Les résidus suivent une loi normale")
      )
    }
    
    # Test de Shapiro-Wilk (normalité)
    if (input$shapiro && length(residuals) <= 5000) {
      sw_test <- shapiro.test(residuals)
      test_results$ShapiroWilk <- list(
        statistic = sw_test$statistic,
        p.value = sw_test$p.value,
        conclusion = ifelse(sw_test$p.value < alpha,
                            "Rejet de H0: Les résidus ne suivent pas une loi normale",
                            "Non rejet de H0: Les résidus suivent une loi normale")
      )
    } else if (input$shapiro) {
      test_results$ShapiroWilk <- list(
        message = "Le test de Shapiro-Wilk ne peut être effectué sur plus de 5000 observations"
      )
    }
    
    # Test ARCH (hétéroscédasticité)
    if (input$arch) {
      # Implémentation manuelle du test ARCH
      squared_residuals <- residuals^2
      arch_lm <- lm(squared_residuals[-1] ~ squared_residuals[-length(squared_residuals)])
      arch_test_stat <- length(residuals) * summary(arch_lm)$r.squared
      arch_pvalue <- pchisq(arch_test_stat, df = 1, lower.tail = FALSE)
      
      test_results$ARCH <- list(
        statistic = arch_test_stat,
        p.value = arch_pvalue,
        conclusion = ifelse(arch_pvalue < alpha,
                            "Rejet de H0: Présence d'hétéroscédasticité",
                            "Non rejet de H0: Pas d'hétéroscédasticité détectée")
      )
    }
    
    # Stocker les résultats pour affichage
    output$test_results <- renderPrint({
      cat("Résultats des tests de significativité\n")
      cat("------------------------------------\n")
      cat("Modèle testé:", model_name, "\n")
      cat("Niveau de significativité (alpha):", alpha, "\n\n")
      
      if (!is.null(test_results$LjungBox)) {
        cat("\nTest de Ljung-Box (autocorrélation):\n")
        cat("Statistique:", test_results$LjungBox$statistic, "\n")
        cat("p-value:", test_results$LjungBox$p.value, "\n")
        cat("Conclusion:", test_results$LjungBox$conclusion, "\n")
      }
      
      if (!is.null(test_results$JarqueBera)) {
        cat("\nTest de Jarque-Bera (normalité):\n")
        cat("Statistique:", test_results$JarqueBera$statistic, "\n")
        cat("p-value:", test_results$JarqueBera$p.value, "\n")
        cat("Conclusion:", test_results$JarqueBera$conclusion, "\n")
      }
      
      if (!is.null(test_results$ShapiroWilk)) {
        if (!is.null(test_results$ShapiroWilk$message)) {
          cat("\n", test_results$ShapiroWilk$message, "\n")
        } else {
          cat("\nTest de Shapiro-Wilk (normalité):\n")
          cat("Statistique:", test_results$ShapiroWilk$statistic, "\n")
          cat("p-value:", test_results$ShapiroWilk$p.value, "\n")
          cat("Conclusion:", test_results$ShapiroWilk$conclusion, "\n")
        }
      }
      
      if (!is.null(test_results$ARCH)) {
        cat("\nTest ARCH (hétéroscédasticité):\n")
        cat("Statistique:", test_results$ARCH$statistic, "\n")
        cat("p-value:", test_results$ARCH$p.value, "\n")
        cat("Conclusion:", test_results$ARCH$conclusion, "\n")
      }
    })
    
    # Graphiques de diagnostic
    output$diagnostic_plots <- renderPlot({
      par(mfrow = c(2, 2))
      
      # Plot des résidus
      plot(residuals, main = "Résidus du modèle", ylab = "Résidus")
      abline(h = 0, col = "red")
      
      # Histogramme des résidus
      hist(residuals, main = "Distribution des résidus", xlab = "Résidus", probability = TRUE)
      curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)), 
            col = "red", add = TRUE)
      
      # ACF des résidus
      acf(residuals, main = "ACF des résidus")
      
      # QQ-plot des résidus
      qqnorm(residuals)
      qqline(residuals, col = 2)
    })
  })
  
  
  output$coefficients_table <- renderPrint({
    req(input$model_to_test, input$model_to_test != "")
    
    model_name <- input$model_to_test
    model <- switch(model_name,
                    "arima" = models_list$arima,
                    "auto_arima" = models_list$auto_arima,
                    "exp" = models_list$exp)
    
    if (is.null(model)) {
      cat("Aucun modèle sélectionné ou modèle non disponible.\n")
      return()
    }
    
    # Pour les modèles ARIMA
    if (model_name %in% c("arima", "auto_arima")) {
      if (!is.null(model)) {
        cat("Coefficients du modèle ARIMA:\n")
        print(coef(model))
      } else {
        cat("Le modèle ARIMA n'a pas été estimé.\n")
      }
    }
    # Pour les modèles exponentiels
    else if (model_name == "exp") {
      if (!is.null(model)) {
        cat("Paramètres du modèle exponentiel:\n")
        print(model$model$par)
      } else {
        cat("Le modèle exponentiel n'a pas été estimé.\n")
      }
    }
  })
  
  output$coef_interpretation <- renderPrint({
    req(input$model_to_test, input$model_to_test != "")
    
    model_name <- input$model_to_test
    model <- switch(model_name,
                    "arima" = models_list$arima,
                    "auto_arima" = models_list$auto_arima,
                    "exp" = models_list$exp)
    
    if (is.null(model)) {
      cat("Aucun modèle sélectionné pour l'interprétation.\n")
      return()
    }
    
    # Pour les modèles ARIMA
    if (model_name %in% c("arima", "auto_arima")) {
      if (!is.null(model)) {
        coef_table <- coef(model)
        significant_coefs <- coef_table[coef_table[,4] < 0.05, ]
        
        cat("\nInterprétation des coefficients significatifs (p-value < 0.05):\n")
        if (nrow(significant_coefs) > 0) {
          for (i in 1:nrow(significant_coefs)) {
            coef_name <- rownames(significant_coefs)[i]
            coef_value <- significant_coefs[i, 1]
            
            if (grepl("^ar", coef_name)) {
              cat("\n- ", coef_name, " (", coef_value, "): ")
              cat("Le terme autorégressif d'ordre", sub("ar", "", coef_name),
                  "est significatif et contribue au modèle.\n")
            } else if (grepl("^ma", coef_name)) {
              cat("\n- ", coef_name, " (", coef_value, "): ")
              cat("Le terme de moyenne mobile d'ordre", sub("ma", "", coef_name),
                  "est significatif et contribue au modèle.\n")
            } else if (grepl("^sar", coef_name)) {
              cat("\n- ", coef_name, " (", coef_value, "): ")
              cat("Le terme autorégressif saisonnier d'ordre", sub("sar", "", coef_name),
                  "est significatif et contribue au modèle.\n")
            } else if (grepl("^sma", coef_name)) {
              cat("\n- ", coef_name, " (", coef_value, "): ")
              cat("Le terme de moyenne mobile saisonnier d'ordre", sub("sma", "", coef_name),
                  "est significatif et contribue au modèle.\n")
            } else {
              cat("\n- ", coef_name, " (", coef_value, "): ")
              cat("Ce coefficient est significatif mais son interprétation spécifique dépend du contexte.\n")
            }
          }
        } else {
          cat("\nAucun coefficient significatif au seuil de 5%.\n")
        }
      }
    }
    # Pour les modèles exponentiels
    else if (model_name == "exp") {
      if (!is.null(model)) {
        cat("\nInterprétation des paramètres du modèle exponentiel:\n")
        
        if ("alpha" %in% names(model$model$par)) {
          cat("\n- alpha (", model$model$par["alpha"], "): ")
          cat("Paramètre de lissage pour le niveau. Une valeur proche de 1 indique que le modèle s'ajuste rapidement aux changements récents.\n")
        }
        
        if ("beta" %in% names(model$model$par)) {
          cat("\n- beta (", model$model$par["beta"], "): ")
          cat("Paramètre de lissage pour la tendance. Une valeur proche de 1 indique que la tendance est très sensible aux changements récents.\n")
        }
        
        if ("gamma" %in% names(model$model$par)) {
          cat("\n- gamma (", model$model$par["gamma"], "): ")
          cat("Paramètre de lissage pour la saisonnalité. Une valeur proche de 1 indique que le composant saisonnier est très sensible aux changements récents.\n")
        }
        
        cat("\n\nNote: Les modèles exponentiels n'ont pas de tests de significativité standards pour leurs paramètres.\n")
      }
    }
  })
  
  # Partie du server à inclure dans output$download_report
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("rapport_", Sys.Date(), switch(input$format_rapport,
                                            "html_document" = ".html",
                                            "word_document" = ".docx"))
    },
    content = function(file) {
      req(ts_data())
      
      # Objets nécessaires
      serie <- ts_data()
      
      # Résumé du modèle
      model <- models_list$auto_arima %||% models_list$arima %||% models_list$exp
      model_summary <- if (!is.null(model)) capture.output(summary(model)) else "Aucun modèle estimé."
      
      # Graphique de la série
      plot_serie <- ggplot(data.frame(Time = time(serie), Value = as.numeric(serie)), aes(x = Time, y = Value)) +
        geom_line(color = "blue") +
        ggtitle("Série Temporelle") + theme_minimal()
      
      # ACF et PACF
      plot_acf <- tryCatch({
        ggAcf(serie) + ggtitle("ACF") + theme_minimal()
      }, error = function(e) {
        ggplot() + ggtitle("Erreur ACF")
      })
      
      plot_pacf <- tryCatch({
        ggPacf(serie) + ggtitle("PACF") + theme_minimal()
      }, error = function(e) {
        ggplot() + ggtitle("Erreur PACF")
      })
      
      # Résultats des tests de stationnarité (ex. ADF)
      test_station <- tryCatch(adf.test(serie), error = function(e) NULL)
      stationarity_results <- if (!is.null(test_station)) capture.output(test_station) else "Test ADF non disponible."
      
      # Résultats de prévision
      fc <- forecasts_list[[input$forecast_model]]
      forecast_plot <- if (!is.null(fc) && inherits(fc, "forecast")) {
        autoplot(fc) + ggtitle("Prévisions") + theme_minimal()
      } else {
        ggplot() + ggtitle("Pas de prévision disponible")
      }
      
      forecast_table <- if (!is.null(fc)) {
        data.frame(Date = time(fc$mean),
                   Prévision = as.numeric(fc$mean),
                   Borne_Basse = as.numeric(fc$lower[,1]),
                   Borne_Haute = as.numeric(fc$upper[,1]))
      } else {
        data.frame()
      }
      
      # Résidus et tests
      residual_tests <- "Pas de tests réalisés."
      if (!is.null(model)) {
        res <- residuals(model)
        res <- na.omit(res)
        tests <- list()
        try({
          tests[["Ljung-Box"]] <- capture.output(Box.test(res, lag = 10, type = "Ljung-Box"))
          tests[["Jarque-Bera"]] <- capture.output(jarque.bera.test(res))
          if (length(res) <= 5000) {
            tests[["Shapiro"]] <- capture.output(shapiro.test(res))
          }
        }, silent = TRUE)
        residual_tests <- paste(unlist(tests), collapse = "\n")
      }
      
      # Rendu du rapport
      rmarkdown::render(
        input = "report_template_with_code.Rmd",
        output_format = input$format_rapport,
        output_file = file,
        params = list(
          title = input$report_title,
          author = input$nom_auteur,
          model_summary = model_summary,
          plot_serie = plot_serie,
          plot_acf = plot_acf,
          plot_pacf = plot_pacf,
          stationarity_results = stationarity_results,
          forecast_plot = forecast_plot,
          forecast_table = forecast_table,
          residual_tests = residual_tests
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
  observeEvent(input$find_best_model, {
    req(ts_data())
    
    withProgress(message = 'Recherche du meilleur modèle...', {
      # Trouver le meilleur modèle ARIMA automatiquement
      best_arima <- auto.arima(ts_data())
      models_list$best_model <- best_arima
      models_list$best_model_name <- "Modèle ARIMA optimal"
      
      # Mettre à jour les choix
      updateSelectInput(session, "selected_model", 
                        choices = c("Meilleur modèle automatique" = "auto",
                                    "ARIMA manuel" = "manual",
                                    "Modèle exponentiel" = "exp"),
                        selected = "auto")
    })
  })
  
  output$best_model_summary <- renderPrint({
    req(models_list$best_model)
    
    cat("=== MEILLEUR MODÈLE TROUVÉ ===\n\n")
    cat("Type: ARIMA", models_list$best_model$arma[1], models_list$best_model$arma[6], 
        models_list$best_model$arma[2], "\n")
    cat("\nCoefficients:\n")
    print(coef(models_list$best_model))
    cat("\nAIC:", models_list$best_model$aic, "\n")
    cat("BIC:", models_list$best_model$bic, "\n")
  })
  
  output$selected_model_plot <- renderPlot({
    req(input$selected_model)
    
    if (input$selected_model == "auto" && !is.null(models_list$best_model)) {
      plot(forecast(models_list$best_model, h = 12), main = "Prévisions - Meilleur modèle ARIMA")
    } else if (input$selected_model == "manual" && !is.null(models_list$arima)) {
      plot(forecast(models_list$arima, h = 12), main = "Prévisions - ARIMA manuel")
    } else if (input$selected_model == "exp" && !is.null(models_list$exp)) {
      plot(forecast(models_list$exp, h = 12), main = "Prévisions - Modèle exponentiel")
    } else {
      plot(1, type = "n", main = "Modèle non disponible")
      text(1, 1, "Veuillez d'abord estimer ce modèle")
    }
  })
  
  
}

shinyApp(ui = ui, server = server)