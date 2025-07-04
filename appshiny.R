library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(readr)

data <- read_csv2("student-mat.csv")

ui <- dashboardPage(dashboardHeader(title = "Dashboard Estudiantes"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gráficos de barras", tabName = "barras", icon = icon("chart-bar")),
      menuItem("Gráficos de dispersión", tabName = "dispersion", icon = icon("chart-scatter")),
      menuItem("Boxplots", tabName = "boxplots", icon = icon("chart-box")),
      menuItem("Violin Plots", tabName = "violins", icon = icon("chart-area")))),
  dashboardBody(tabItems(tabItem(tabName = "barras",
        fluidRow(box(title = "Selector de agrupación",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            pickerInput("agrupacion",
              "Mostrar porcentaje de aprobación según:",
              choices = c("Sexo" = "sex",
                "Educación de la madre" = "Medu",
                "Acceso a Internet" = "internet",
                "Clases extra" = "paid"), selected = "sex"))),
        fluidRow(box(title = "Porcentaje de Aprobación",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotOutput("barras")))), tabItem(tabName = "dispersion",
          fluidRow(box(title = "Selector de Ejes",
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              selectInput(
                "eje_x",
                "Selecciona la calificación parcial para comparar con G3:",
                choices = c("Primera calificación parcial (G1)" = "G1",
                            "Segunda calificación parcial (G2)" = "G2"),
                selected = "G1"))),
          fluidRow(box(title = "Gráficos de Dispersión",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              plotOutput("scatter_plot")))),
      tabItem(tabName = "boxplots",
        fluidRow(box(title = "Selector de Boxplot",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            radioButtons("boxplot_type",
              "Selecciona el boxplot:",
              choices = c("Nota final según tiempo de estudio semanal" = "studytime",
                "Clasificación por periodo (G1-G2-G3)" = "periodo"),
              inline = TRUE),
            plotOutput("boxplot")))),
      tabItem(tabName = "violins",
        fluidRow(box(title = "Violín: Ausencias vs. Tiempo de Viaje",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotOutput("violin_ausencias"))),
        fluidRow(box(title = "Violín: Nota Final vs. Tiempo de Viaje",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotOutput("violin_g3_traveltime"))),
        fluidRow(box(
            title = "Violín: Nota Final vs. Cursos Reprobados",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotOutput("violin_g3_failures")))))))

server <- function(input, output) {
  datos_estado <- reactive({
    data |> 
      mutate(Estado = if_else(G3 > 9, "Aprobado", "No aprobado"),
        sex = recode(sex, "F" = "Femenino", "M" = "Masculino"),
        internet = recode(internet, "yes" = "Tiene internet", "no" = "No tiene internet"),
        paid = recode(paid, "yes" = "Clases extra", "no" = "Sin clases extra"),
        Medu = factor(Medu,
          levels = 0:4,
          labels = c(
            "Sin educación",
            "Educación primaria",
            "De 5º a 9º grado",
            "Educación secundaria",
            "Educación superior"), ordered = TRUE),
        traveltime = factor(traveltime,
          levels = 1:4,
          labels = c("<15 min", "15-30 min", "30-60 min", ">1 hora"),
          ordered = TRUE),
        studytime = factor(studytime,
          levels = 1:4,
          labels = c("<2 horas", "2-5 horas", "5-10 horas", ">10 horas"),
          ordered = TRUE))
    })
  output$barras <- renderPlot({
    df <- datos_estado() |> group_by(!!sym(input$agrupacion)) |> 
      summarise(Total = n(),
        Aprobados = sum(G3 > 9),
        Porcentaje = Aprobados / Total * 100)
    eje_x <- switch(input$agrupacion,
      sex = "Sexo",
      Medu = "Educación de la madre",
      internet = "Acceso a internet",
      paid = "Clases extra")
    df <- df |> mutate(grupo = if (input$agrupacion == "Medu") {
        fct_reorder(!!sym(input$agrupacion), Porcentaje)
      } else {!!sym(input$agrupacion)})
    colores_pastel <- c("#A7C7E7", "#B5EAD7", "#C7CEEA", "#FFDAC1", "#FFF1B6")
      ggplot(df, aes(x = grupo, y = Porcentaje, fill = grupo)) +
      geom_col() +
      geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5, size = 5) +
      ylim(0, 100) +
      labs(title = paste("Porcentaje de Aprobación según", eje_x),
        x = eje_x,
        y = "Porcentaje de Aprobación") + scale_fill_manual(values = colores_pastel) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 15)))
  })
  output$scatter_plot <- renderPlot({
    df <- datos_estado()
    ggplot(df, aes_string(x = input$eje_x, y = "G3")) +
      geom_point(alpha = 0.6, color = "#2C3E50") +
      geom_smooth(method = "lm", se = FALSE, color = "skyblue") +
      labs(title = paste("Relación entre", input$eje_x, "y Nota Final (G3)"),
        x = input$eje_x,
        y = "Nota Final (G3)") +
      theme_minimal(base_size = 14) +
      theme(axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"))
  })
  output$boxplot <- renderPlot({
    df <- datos_estado()
    if (input$boxplot_type == "studytime") {
      ggplot(df, aes(x = studytime, y = G3, fill = studytime)) +
        geom_boxplot(alpha = 0.9) +
        labs(title = "Nota final (G3) según tiempo de estudio semanal",
          x = "Tiempo de estudio semanal",
          y = "Nota final") + scale_fill_brewer(palette = "Pastel2") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none",
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"))
    } else {df_long <- df |>
        pivot_longer(cols = c(G1, G2, G3), names_to = "Periodo", values_to = "Nota")
      ggplot(df_long, aes(x = Periodo, y = Nota, fill = Periodo)) +
        geom_boxplot() +
        scale_fill_brewer(palette = "Paired") +
        stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
        labs(title = "Boxplot de Clasificación por Periodo",
          x = "Periodo",
          y = "Nota") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none",
          axis.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"))}
  })
  output$violin_ausencias <- renderPlot({
  df <- datos_estado()
 ggplot(df, aes(x = traveltime, y = absences, fill = traveltime)) +
      geom_violin(trim = FALSE, alpha = 0.7) +
      scale_y_continuous(trans = "log10") +
      labs(title = "Distribución de Ausencias según Tiempo de Viaje",
        x = "Tiempo de viaje",
        y = "Número de ausencias (escala log)") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"))
  })
  output$violin_g3_traveltime <- renderPlot({
    df <- datos_estado()
    ggplot(df, aes(x = traveltime, y = G3, fill = traveltime)) +
      geom_violin(trim = FALSE, alpha = 0.7) +
      labs(title = "Distribución de la Nota Final (G3) según Tiempo de Viaje",
        x = "Tiempo de viaje",
        y = "Nota final") +
      scale_fill_brewer(palette = "Set1") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"))
  })
  output$violin_g3_failures <- renderPlot({
    df <- datos_estado()
    ggplot(df, aes(x = as.factor(failures), y = G3, fill = as.factor(failures))) +
      geom_violin(trim = FALSE, alpha = 0.7) +
      labs(title = "Distribución de la Nota Final (G3) según Cantidad de Cursos Reprobados",
        x = "Cantidad de cursos reprobados (failures)",
        y = "Nota final") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"))
  })
  }

shinyApp(ui, server)

