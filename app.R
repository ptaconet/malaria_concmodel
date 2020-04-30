#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(tidygraph)
require(ggraph)
require(purrr)
require(dplyr)

nodes <- readRDS("nodes.rds")
edges <- readRDS("edges.rds")

# convert to tidygraph object
graph <- tidygraph::tbl_graph(nodes=nodes,edges=edges)
# gr <- graph %>%
#     ggraph(layout = "kk") +
#     geom_edge_diagonal(color = "gray", alpha = 0.4, arrow = arrow(length = unit(3, 'mm'))) +
#     geom_node_point(aes(color = L2, size = poids)) +
#     geom_node_text(aes(label = name, color = L2), size = 3, repel = TRUE) +
#     theme_graph()  +
#     labs(title = "Agressivité des vecteurs",
#          subtitle = "Modèle conceptuel")

b<-nodes %>% 
    arrange(L2,name) %>%
    group_split(L2) %>%
    rev() %>%
    map(~.$name)

names(b) <- rev(sort(unique(nodes$L2)))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    pageWithSidebar(
        headerPanel('Modèle conceptuel traits phénotypiques vecteurs paludisme'),
        sidebarPanel(
            #selectInput('nod', 'Noeud', nodes$name),
            # use regions as option groups
            selectizeInput('nod', 'Noeud', choices = b, selected = "Agressivité (vec.aé.)"),
            selectInput('mod', 'Mode', c("est influencé par" = "in","influence" = "out","est en contact avec" = "all")),
            numericInput('ord', "Ordre", 5, min = 1, max = 10)
        ),
        mainPanel(
            plotOutput('plot1')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Combine the selected variables into a new data frame
    subgraph <- reactive({
        nod <- which(nodes == input$nod)
        mod <- input$mod
        ord <- input$ord
        
        graph %>% 
            morph(to_local_neighborhood, nod, order = ord, mode = mod) %>%
            mutate(selected_node = TRUE) %>%
            activate(edges) %>%
            mutate(selected_edge = TRUE) %>%
            unmorph() %>%
            activate(nodes) %>%
            mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
            activate(edges) %>%
            mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
            arrange(selected_edge)
        
        
    })
    
    output$plot1 <- renderPlot({
        subgraph() %>%
            ggraph(layout = "kk") +
            geom_edge_diagonal(aes(alpha = selected_edge), color = "gray", arrow = arrow(length = unit(3, 'mm'))) +
            geom_node_point(aes(color = L2, alpha = selected_node), size = 3) +
            geom_node_text(aes(label = name, color = L2, alpha = selected_node), size = 3, repel = TRUE) +
            theme_graph()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
