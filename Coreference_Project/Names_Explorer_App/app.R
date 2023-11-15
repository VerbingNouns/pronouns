#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny); library(shinyWidgets); library(shinyBS); library(tidyverse); library(readr); library(scales); library(ggrepel); library(viridis)
theme_set(theme_bw())

#line_color_palette_default <- viridis(n = 8, alpha = 1, begin = .95, end = .1, direction = 1, option = "turbo")

names_combined <- read_csv("data/names_combined.csv", 
                           show_col_types = FALSE)

unisex_names <- names_combined |> 
  mutate(asab = case_when(asab == "F" ~ "AFAB", asab == "M" ~ "AMAB", TRUE ~ "X"),
         asab = factor(asab, levels = c("AFAB", "AMAB", "X")),
         name = str_to_sentence(name)) |> 
  #filter(number > 1) |> 
  group_by(name, asab, region) |> 
  mutate(year_count = n_distinct(year)) |> ungroup() |> 
  pivot_wider(names_from = asab,
              values_from = c(number, rank, proportion, year_count),
              values_fill = 0) |> 
  #filter(!is.na(number_AFAB),!is.na(number_AMAB)) |> 
  mutate(year_count_reg = case_when(year_count_AFAB < year_count_AMAB ~ year_count_AFAB, TRUE ~ year_count_AMAB)) |> 
  select(-c(year_count_AFAB, year_count_AMAB)) |> 
  group_by(name) |> mutate(year_count = max(year_count_reg)) |> ungroup() |> 
  filter(year_count > 0)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(windowTitle = "International Name Data Explorer", h1("International Name Data Explorer (v0.1)")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(h3("Name picker"),
                 p("Please select which name to view. You can narrow your choices by changing the parameters below."),
                 pickerInput(inputId = 'name_picker',
                             label = 'Which name?',
                             choices = unisex_names |> pull(name) |> unique() |> sort(),
                             options = list(`style` = "btn-info")),
                 hr(),
                 p("Since there are so many names, we can narrow down the options based on how many babies were given the name and/or how many years the name is attested."),
                 sliderInput(inputId = 'minimum_years',
                             label = 'Minimum number of years where the name is attested',
                             value = 50,
                             min = 2, max = max(unisex_names$year_count)),
                 sliderInput(inputId = 'minimum_observations',
                             label = 'Minumum number of annual name assignments',
                             value = 400,
                             min = 0, max = 500,
                             step = 10),
                 checkboxGroupInput(inputId = "which_regions",
                                    label = "Select names from only these regions",
                                    choices = c("Australia","Canada","England and Wales","Ireland",
                                                "Northern Ireland","New Zealand","Scotland","USA"),
                                    selected = c("Australia","Canada","England and Wales","Ireland",
                                                 "Northern Ireland","New Zealand","Scotland","USA"),
                                    inline = TRUE),
                 checkboxGroupInput(inputId = "exclude_names",
                                    label = "Exclude \"default\" baby names?",
                                    choices = c("Baby","Infant","Unnamed","Unknown"),
                                    selected = c("Baby","Infant","Unnamed","Unknown"),
                                    inline = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4("Learn about how names are used across time and space in predominantly English-speaking countries"),
      
      div(class = "pull-right",
        dropdown( # https://dreamrs.github.io/shinyWidgets/reference/actionBttn.html
        tags$h3("Visual settings"),
        sliderInput(inputId = "user_defined_plot_width",
                    label = "Size of the plot in your browser window",
                    value = .85,
                    min = 0, max = 1, step = .01, animate = FALSE),
        
        pickerInput(inputId = 'line_color_palette',
                    label = 'Color palette',
                    choices = c("viridis","magma","plasma","inferno","cividis","mako","rocket","turbo"),
                    selected = "turbo",
                    options = list(`style` = "btn-default")),
        
        sliderInput(inputId = 'year_overlaps',
                    label = 'Number of permitted overlapping labels',
                    value = 10,
                    min = 0, max = 30),
        
        style = "bordered", # simple, bordered, minimal, stretch, jelly, gradient, fill, material-circle, material-flat, pill, float, unite
        icon = icon("gear"),
        size = "xs", # xs,sm, md, lg
        status = "primary", # default, primary, warning, danger, success, royal
        width = "350px",
        right = TRUE,
        tooltip = tooltipOptions(placement = "bottom", title = "Adjust visual properties of the graph", html = FALSE),
        checkboxInput(inputId = "xy_line", label = "Show the `x=y` line", value = TRUE),
        checkboxInput(inputId = "axis_scaling", label = "Scale axes logarithmically", value = TRUE),
        checkboxInput(inputId = "mirror_aspect_ratio", label = "Equi-balanced view", value = TRUE)
        )
       ),
      
      plotOutput("names_plot", height = "auto", width = "auto"),
      hr(),
      h4("Options for modifying the content of the graph"),
      checkboxInput(inputId = "exclude_zeros",
                    label = "Exclude years where either AMAB and AFAB counts are zero?",
                    value = TRUE, width = "75%"),
      checkboxGroupInput(inputId = "switch_regions",
                         label = "Show or hide data from these regions",
                         choices = c("Australia","Canada","England and Wales","Ireland",
                                     "Northern Ireland","New Zealand","Scotland","USA"),
                         selected = c("Australia","Canada","England and Wales","Ireland",
                                      "Northern Ireland","New Zealand","Scotland","USA"),
                         inline = TRUE, width = "75%"),
      sliderInput(inputId = "which_years_range",
                  label = "Which years to plot",
                  min = min(unisex_names$year),
                  max = max(unisex_names$year),
                  value = c(min(unisex_names$year),max(unisex_names$year)),
                  width = "75%",
                  sep = "", 
                  animate = animationOptions(interval = 500, 
                                             loop = FALSE, 
                                             playButton = NULL,
                                             pauseButton = NULL)),
      bsPopover(id = "which_years_range", 
                title = "Animate your selection", 
                content = "By selecting a shorter range and clicking the \"play\" button on the right of the scale on the right, you can animate the plot to show year on year changes.", 
                placement = "top", 
                trigger = "hover",
                options = NULL)
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observe(updatePickerInput(session, 
                            inputId = "name_picker", 
                            choices = unisex_names |>
                              filter(number_AFAB >= input$minimum_observations,
                                     number_AMAB >= input$minimum_observations,
                                     region %in% input$which_regions,
                                     !name %in% input$exclude_names,
                                     year_count >= input$minimum_years) |> 
                              pull(name) |> unique() |> sort()
                            )
          )
  
  output$names_plot <- renderPlot({
    line_color_palette <- viridis(n = 8, #unisex_names |> pull(region) |> unique() |> length(), 
                                  alpha = 1, begin = .95, end = .1, direction = 1, 
                                  option = input$line_color_palette)
    xy_line_color <- ifelse(input$xy_line, "black", NA)
    
    # names plot
    unisex_names |> 
      filter(case_when(input$exclude_zeros ~ (number_AMAB & number_AFAB) > 0,
                       TRUE ~ (number_AMAB & number_AFAB) >= 0),
             region %in% input$switch_regions,
             name == input$name_picker,
             year >= input$which_years_range[1] & year <= input$which_years_range[2]) |> 
      ggplot(aes(x = proportion_AMAB,
                 y = proportion_AFAB)) +
      geom_abline(slope = 1, intercept = 0, color = xy_line_color) +
      {if (input$mirror_aspect_ratio) geom_point(aes(x = proportion_AFAB, y = proportion_AMAB), color = NA)} +
      geom_path(aes(colour = region,
                    group = region),
                na.rm = TRUE) +
      geom_text_repel(aes(label = year), 
                      verbose = FALSE,
                      #size = 3, 
                      color = "black", 
                      alpha = .5,
                      max.overlaps = input$year_overlaps) +
      {if (input$axis_scaling) scale_y_log10(labels = scales::percent_format())} + 
      {if (input$axis_scaling) scale_x_log10(labels = scales::percent_format())} +
      {if (!input$axis_scaling) scale_y_continuous(labels = scales::percent_format())} + 
      {if (!input$axis_scaling) scale_x_continuous(labels = scales::percent_format())} +
      scale_color_manual(values = c("Australia" = line_color_palette[1],
                                    "Canada" = line_color_palette[2],
                                    "England and Wales" = line_color_palette[3],
                                    "Ireland" = line_color_palette[4],
                                    "Northern Ireland" = line_color_palette[5],
                                    "New Zealand" = line_color_palette[6],
                                    "Scotland" = line_color_palette[7],
                                    "USA" = line_color_palette[8])) +
      #coord_equal() +
      ggtitle(paste0("Evolution of \"",as.character(input$name_picker),"\" over time"),
              #subtitle = paste(session$clientData$output_names_plot_width,",",
              #                 session$clientData$output_names_plot_height," : ",
              #                 input$user_defined_plot_width)
              ) +
      xlab("proportion of AMAB babies assigned given name each year") +
      ylab("proportion of AFAB babies assigned given name each year") +
      NULL
    }, 
    height = function() {session$clientData$output_names_plot_width * (input$user_defined_plot_width * 0.6 + 0.4) * 0.8},
    width  = function() {session$clientData$output_names_plot_width * (input$user_defined_plot_width * 0.6 + 0.4)}
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
