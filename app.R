# Start Trey Tribulations
# Last Change 10/9/2024
########################################################
library(shiny)
library(httr)
library(jsonlite)
library(DT)

# Define the UI
ui <- fluidPage(
  titlePanel("FBI Most Wanted Persons and Stolen Art"),
  sidebarLayout(sidebarPanel(
    helpText(
      "This dashboard displays information from the FBI Crime Data. There is a tab for Wanted Persons and Stolen Art. Click on an item in either table to see more information."
    )
  ),
  mainPanel(
    tabsetPanel(
      id = "tabs",
      tabPanel("Most Wanted",
               DTOutput("wanted_table")),
      # Table for wanted persons
      tabPanel("Art Crimes",
               DTOutput("artcrimes_table"))  # Table for art crimes
    )
  ))
)

# Define the server logic
server <- function(input, output) {
  # Fetch the data from the API
  url <- "https://api.fbi.gov/wanted/v1/list"
  response <- GET(url)
  
  # Check if the request was successful
  if (response$status_code == 200) {
    # Parse the JSON content
    data <- fromJSON(content(response, as = "text"))
    
    # Convert the relevant part of the data to a data frame
    df <- as.data.frame(data$items)
    
    # Prepare a data frame with relevant fields
    wanted_df <-
      df[, c(
        "title",
        "description",
        "uid",
        "eyes_raw",
        "hair_raw",
        "dates_of_birth_used",
        "height_max",
        "caution",
        "race_raw",
        "details",
        "possible_countries",
        "sex",
        "aliases",
        "scars_and_marks",
        "remarks",
        "place_of_birth",
        "warning_message",
        "weight_max",
        "images"
      )]
    
    # Create a reactive value to store the data
    wanted_data <- reactiveVal(wanted_df)
  } else {
    wanted_data <-
      reactiveVal(data.frame(
        title = "Error",
        description = paste("Error:", response$status_code)
      ))
  }
  
  # Render the wanted persons table using DT
  output$wanted_table <- renderDT({
    datatable(
      wanted_data()[, c("title", "description")],
      selection = "single",
      # Allow single row selection
      options = list(pageLength = 5, autoWidth = TRUE)
    )
  })
  
  # Observe table clicks
  observeEvent(input$wanted_table_rows_selected, {
    selected_row <- input$wanted_table_rows_selected
    if (!is.null(selected_row)) {
      # Get the selected person's details
      selected_person <- wanted_data()[selected_row, ]
      
      image_url <- selected_person$images[[1]]$original[1]
      
      # Show modal dialog with details
      showModal(
        modalDialog(
          title = selected_person$title,
          easyClose = TRUE,
          footer = NULL,
          HTML(
            paste0(
              "<img src='",
              image_url,
              "' style='max-width: 100%; height: auto;' alt='Image of ",
              selected_person$title,
              "' />",
              "<p><strong>_________________________________________________________________________</p>",
              "<p><strong>Date of Birth:</strong> ",
              selected_person$dates_of_birth_used,
              "</p>",
              "<p><strong>Height:</strong> ",
              selected_person$height_max,
              " inches </p>",
              "<p><strong>Weight:</strong> ",
              selected_person$weight_max,
              " lbs </p>",
              "<p><strong>Hair:</strong> ",
              selected_person$hair_raw,
              "</p>",
              "<p><strong>Eyes:</strong> ",
              selected_person$eyes_raw,
              "</p>",
              "<p><strong>Race:</strong> ",
              selected_person$race_raw,
              "</p>",
              "<p><strong>Sex:</strong> ",
              selected_person$sex,
              "</p>",
              "<p><strong>Scars/Marks:</strong> ",
              selected_person$scars_marks,
              "</p>",
              "<p><strong>Aliases:</strong> ",
              selected_person$aliases,
              "</p>",
              "<p><strong>Details:</strong> ",
              selected_person$details,
              "</p>",
              "<p><strong>Remarks:</strong> ",
              selected_person$remarks,
              "</p>",
              "<p><strong>Place of Birth:</strong> ",
              selected_person$place_of_birth,
              "</p>",
              "<p><strong>Possible Countries:</strong> ",
              selected_person$possible_countries,
              "</p>",
              "<p><strong>Crimes:</strong> ",
              selected_person$description,
              "</p>",
              "<p><strong>Caution:</strong> ",
              selected_person$caution,
              "</p>",
              "<p><strong>Warning:</strong> ",
              selected_person$warning_message,
              "</p>"
            )
          )
        )
      )
    }
  })
  
  url_artcrimes <- "https://api.fbi.gov/artcrimes/list"
  response_artcrimes <- GET(url_artcrimes)
  
  # Check if the request was successful
  if (response_artcrimes$status_code == 200) {
    # Parse the JSON content
    data_artcrimes <-
      fromJSON(content(response_artcrimes, as = "text"))
    
    # Convert the relevant part of the data to a data frame
    df_artcrimes <- as.data.frame(data_artcrimes$items)
    
    
    # Create a reactive value to store the data
    artcrimes_data <- reactiveVal(df_artcrimes)
  } else {
    artcrimes_data <-
      reactiveVal(data.frame(
        title = "Error",
        description = paste("Error:", response_artcrimes$status_code)
      ))
  }
  
  # Render the art crimes table
  output$artcrimes_table <- renderDT({
    datatable(
      artcrimes_data()[, c("title", "maker")],
      selection = "single",
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  # Observe table clicks for art crimes
  observeEvent(input$artcrimes_table_rows_selected, {
    selected_row <- input$artcrimes_table_rows_selected
    if (!is.null(selected_row)) {
      selected_artcrime <- artcrimes_data()[selected_row, ]
      
      art_image_url <- selected_artcrime$images[[1]]$original[1]
      
      showModal(
        modalDialog(
          title = selected_artcrime$title,
          easyClose = TRUE,
          footer = NULL,
          HTML(
            paste0(
              "<img src='",
              art_image_url,
              "' style='max-width: 100%; height: auto;' alt='Image of ",
              selected_artcrime$title,
              "' />",
              "<p><strong>_________________________________________________________________________</p>",
              "<p><strong>Maker:</strong> ",
              selected_artcrime$maker,
              "</p>",
              "<p><strong>Period Made:</strong> ",
              selected_artcrime$period,
              "</p>",
              "<p><strong>Measurements:</strong> ",
              selected_artcrime$measurements,
              "</p>",
              "<p><strong>Material(s):</strong> ",
              selected_artcrime$materials,
              "</p>",
              "<p><strong>Description:</strong> ",
              selected_artcrime$description,
              "</p>"
            )
          )
        )
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
