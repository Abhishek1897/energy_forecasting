# Install required packages if not already installed
# install.packages(c("shiny", "ggplot2", "dplyr", "scales"))

# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

# Assuming 'merge_static_house_info_df4' is your dataset
# Replace with your actual dataset if needed
data <- merge_static_house_info_df4

# Define UI
ui <- fluidPage(
  titlePanel("Energy Consumption Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Plot Options"),
      selectInput("plot_type", "Choose a plot:",
                  choices = c("Box Plot", "Vintage Bar Chart", "City Bar Chart", 
                              "Bedrooms Pie Chart", "Scatter Plot", "Heating Fuel Bar Chart"))
    ),
    mainPanel(
      plotOutput("selected_plot")
    )
  )
)

# Define Server
server <- function(input, output) {
  output$selected_plot <- renderPlot({
    # Plot based on user selection
    if (input$plot_type == "Box Plot") {
      # Box Plot
      ggplot(data, aes(x = in.building_america_climate_zone, y = day_total_energy)) +
        geom_boxplot(fill = "lightblue", color = "blue") +
        labs(title = "Distribution of Total Energy Consumption Across Building Climate Zones",
             x = "Building Climate Zone",
             y = "Total Energy Consumption") +
        theme_minimal()
    } else if (input$plot_type == "Vintage Bar Chart") {
      # Vintage Bar Chart
      average_energy_by_vintage <- data %>%
        group_by(in.vintage) %>%
        summarize(avg_energy = mean(day_total_energy, na.rm = TRUE))
      
      ggplot(average_energy_by_vintage, aes(x = in.vintage, y = avg_energy)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = "Average Energy Consumption by Vintage",
             x = "Vintage",
             y = "Average Energy Consumption")
    } else if (input$plot_type == "City Bar Chart") {
      # City Bar Chart
      average_energy_by_city <- data %>%
        group_by(in.weather_file_city) %>%
        summarize(avg_energy = mean(day_total_energy, na.rm = TRUE))
      
      ggplot(average_energy_by_city, aes(x = in.weather_file_city, y = avg_energy)) +
        geom_bar(stat = "identity", fill = "lightcoral") +
        labs(title = "Average Energy Consumption by City",
             x = "City",
             y = "Average Energy Consumption") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$plot_type == "Bedrooms Pie Chart") {
      # Bedrooms Pie Chart
      average_energy_by_bedrooms <- data %>%
        group_by(in.bedrooms) %>%
        summarize(avg_energy = mean(day_total_energy, na.rm = TRUE)) %>%
        mutate(percentage = avg_energy / sum(avg_energy) * 100)
      
      ggplot(average_energy_by_bedrooms, aes(x = "", y = avg_energy, fill = factor(in.bedrooms))) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = rainbow(length(unique(average_energy_by_bedrooms$in.bedrooms)))) +
        labs(title = "Average Energy Consumption by Number of Bedrooms",
             fill = "Number of Bedrooms",
             y = "Average Energy Consumption") +
        theme_minimal() +
        geom_text(aes(label = sprintf("%.1f%%", percentage)),
                  position = position_stack(vjust = 0.5), color = "white", size = 3)
    } else if (input$plot_type == "Scatter Plot") {
      # Scatter Plot
      ggplot(data, aes(x = median_Dry_Bulb_Temperature, y = day_total_energy, color = median_Dry_Bulb_Temperature)) +
        geom_point(size = 3) +
        scale_color_gradient(low = "lightblue", high = "darkblue", name = "Temperature") +
        labs(title = "Total Energy Consumption vs. Dry Bulb Temperature",
             x = "Temperature",
             y = "Total Energy Consumption") +
        theme_minimal()
    } else if (input$plot_type == "Heating Fuel Bar Chart") {
      # Heating Fuel Bar Chart
      filtered_data_heating <- data[data$in.heating_fuel != "None", ]
      
      ggplot(filtered_data_heating, aes(x = in.heating_fuel, y = day_total_energy, fill = in.heating_fuel)) +
        geom_bar(stat = "identity") +
        labs(title = "Total Energy Consumption by Heating Fuel",
             x = "Heating Fuel",
             y = "Total Energy Consumption (kWh)") +
        scale_y_continuous(labels = scales::comma) +  # Use commas for thousands separator
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
