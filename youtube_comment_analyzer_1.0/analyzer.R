# library(rsconnect)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(vosonSML)
library(lubridate)
library(DT)
library(dplyr)
library(KoNLP)
library(stringr)
library(ggplot2)
library(wordcloud2)
library(arules)
library(arulesViz)
library(igraph)
library(visNetwork)




# load
source("dictionary.R", encoding = "utf-8")
source("function.R", encoding = "utf-8")




# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel(em("Youtube Comment Analyzer")),

    # Application title
    titlePanel("View Control"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            searchInput(
                inputId = "in_url", 
                label = "Youtube Url:", 
                placeholder = "Enter a youtube url", 
                btnSearch = icon("search"), 
                btnReset = icon("remove"), 
                width = "100%"
            ),
            hr(),
            radioButtons(inputId = "in_chart_radio",
                         label = "Select Unit",
                         choices = c("All", "Year", "Month"),
                         selected = "All",
                         inline = T),
            dateRangeInput(inputId = "in_date_range",
                           label = "Date Range"),
            conditionalPanel(condition = "input.in_chart_radio == 'Year'",
                             sliderTextInput(
                                 inputId = "in_date_range_year", 
                                 label = "Year Range", 
                                 grid = TRUE, 
                                 force_edges = TRUE,
                                 choices = c("Strongly disagree",
                                             "Disagree", "Neither agree nor disagree", 
                                             "Agree", "Strongly agree"),
                             )
            ),
            conditionalPanel(condition = "input.in_chart_radio == 'Month'",
                             sliderTextInput(
                                 inputId = "in_date_range_month", 
                                 label = "Month Range", 
                                 grid = TRUE, 
                                 force_edges = TRUE,
                                 choices = c("Strongly disagree",
                                             "Disagree", "Neither agree nor disagree", 
                                             "Agree", "Strongly agree")
                             )
            ),
            hr(),
            radioButtons(inputId = "in_word_mode",
                         label = "Select Mode",
                         choices = c("Change", "Delete"),
                         inline = T),
            conditionalPanel(condition = "input.in_word_mode == 'Change'",
                             splitLayout(
                                 textInput(inputId = "in_filter_from",
                                           label = "Word Change"),
                                 h4("→"),
                                 textInput(inputId = "in_filter_to",
                                           label = ""),
                                 cellWidths = c("45%", "8%", "45%"),
                                 cellArgs = list(style = "vertical-align: middle")
                                 ),
                                 actionButton(inputId = "word_change", 
                                              label = "Change")
                             ),
            conditionalPanel(condition = "input.in_word_mode == 'Delete'",
                             splitLayout(
                                 textInput(inputId = "in_delete_word",
                                           label = "Word Delete"),
                                 actionButton(inputId = "word_delete", 
                                              label = "Delete"),
                                 cellWidths = c("71%", "35%"),
                                 cellArgs = list(style = "vertical-align: middle")
                             )
            ),
            hr(),
            conditionalPanel(condition = "input.view == 2",
                             h5("Chart Controller"),
                             sliderInput(inputId = "in_chart_max_word",
                                         label = "Maximum Words Number:",
                                         min = 1,
                                         max = 10,
                                         value = 5),
                             sliderInput(inputId = "in_chart_min_word",
                                         label = "Word Min Frequency:",
                                         min = 1,
                                         max = 10,
                                         value = 5)
            ),
            conditionalPanel(condition = "input.view == 3",
                             h5("Wordcloud Controller"),
                             sliderInput(inputId = "in_wordcloud_max_word",
                                         label = "Maximum Words Number:",
                                         min = 1,
                                         max = 5,
                                         value = 10),
                             sliderInput(inputId = "in_wordcloud_min_word",
                                         label = "Word Min Frequency:",
                                         min = 1,
                                         max = 10,
                                         value = 5)
            ),
            conditionalPanel(condition = "input.view == 4",
                             h5("Trend Controller"),
                             checkboxInput(inputId = "in_trend_accumulation",
                                           label = "Accumulation"),
                             sliderInput(inputId = "in_trend_max_word",
                                         label = "Word Max:",
                                         min = 1,
                                         max = 20,
                                         value = 10)
            ),
            conditionalPanel(condition = "input.view == 5",
                             h5("Aprirori Controller"),
                             checkboxInput(inputId = "in_apriori_hierarchical",
                                           label = "Hierarchical"),
                             sliderInput(inputId = "in_apriori_supp",
                                         label = "Supp Range:",
                                         min = 0.03,
                                         max = 1,
                                         value = 0.05,
                                         step = 0.01),
                             sliderInput(inputId = "in_apriori_conf",
                                         label = "Conf Range:",
                                         min = 0.01,
                                         max = 1,
                                         value = 0.05,
                                         step = 0.01)
            )
        ), 

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                id = "view",
                tabPanel("Comment", value = 1, tabsetPanel(id = "Trend_View",
                                                           tabPanel("Comment", value = 1.1, dataTableOutput("out_Comment")),
                                                           tabPanel("Reply", value = 1.2, dataTableOutput("out_Reply")),
                                                           tabPanel("Reply Network", value = 1.3, visNetworkOutput("out_Reply_network"))
                                                          )
                         ),
                tabPanel("Chart", value = 2, tabsetPanel(id = "Chart_View",
                                                         tabPanel("Chart", value = 2.1, plotly::plotlyOutput("out_Chart")),
                                                         tabPanel("Table", value = 2.2, dataTableOutput("out_DT_Chart"))
                                                         )
                         ),
                tabPanel("Wordcloud", value = 3, tabsetPanel(id = "Chart_View",
                                                             tabPanel("Wordcloud", value = 3.1, wordcloud2Output("out_Wordcloud")),
                                                             tabPanel("Table", value = 3.2, dataTableOutput("out_DT_Wordcloud"))
                                                             )
                         ),
                tabPanel("Trend", value = 4, tabsetPanel(id = "Trend_View",
                                                             tabPanel("Trend", value = 4.1, plotly::plotlyOutput("out_Trend")),
                                                             tabPanel("Table", value = 4.2, dataTableOutput("out_DT_Trend"))
                                                            )
                ),
                tabPanel("Apropri", value = 5, tabsetPanel(id = "Apriori_View",
                                                           tabPanel("Network Graph", value = 5.1, visNetworkOutput("out_Apriori")),
                                                           tabPanel("Table", value = 5.2, dataTableOutput("out_DT_Apriori"))
                                                           )
                         )
            )
        )
    )
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    dictionary()
    
    value <<- reactiveValues(text = data.frame(comment = "No comment has been submitted yet"))
    
    
    observeEvent(input$in_chart_radio, {
        shinyjs::toggleState(id = "in_chart_radio",
                             condition = input$in_chart_radio == "All")
    })
    
    # crawling & date range
    observeEvent(input$in_url, {
        
        value$comment = withProgress(message = str_c(c("댓글을 수집 중입니다", "잠시만 기다려 주세요"), sep = "\n"), {
            get_youtube_comment(input$in_url)})
        
        
        if(is.null(value$comment) == F){
        value$comment_extracted = withProgress(message = str_c(c("댓글을 분석 중입니다", "잠시만 기다려 주세요"), sep = "\n"), {
            cbind(data.table::data.table(get_comment_extracted(value$comment), str_sub(value$comment$PublishTime, 1, 10)) %>% 
                      rename(Comment = V1, PublishTime = V2))
        })

        
        updateDateRangeInput(session, 
                             inputId = "in_date_range",
                             start = ymd(str_sub(min(value$comment$PublishTime), 1, 10)),
                             min = ymd(str_sub(min(value$comment$PublishTime), 1, 10)),
                             end = ymd(str_sub(max(value$comment$PublishTime), 1, 10)),
                             max = ymd(str_sub(max(value$comment$PublishTime), 1, 10)))
        
        updateSliderInput(session, 
                          inputId = "in_chart_min_word",
                          max = max(table(unlist(value$comment_extracted$Comment))),
                          value = round(max(table(unlist(value$comment_extracted$Comment)))/2))
        

        updateSliderInput(session, 
                          inputId = "in_wordcloud_min_word",
                          max = max(table(unlist(value$comment_extracted$Comment))),
                          value = round(max(table(unlist(value$comment_extracted$Comment)))/2))
        }
    })
    
    
    
    
    # chart max value
    observeEvent(input$in_chart_min_word, {

        updateSliderInput(session, 
                          inputId = "in_chart_max_word",
                          max = length(table(unlist(value$comment_extracted$Comment))[table(unlist(value$comment_extracted$Comment)) > input$in_chart_min_word]),
                          value = max(table(unlist(value$comment_extracted$Comment))))
    })
    
    
    
    
    # wordcloud max value
    observeEvent(input$in_wordcloud_min_word, {

        updateSliderInput(session, 
                          inputId = "in_wordcloud_max_word",
                          max = length(table(unlist(value$comment_extracted$Comment))[table(unlist(value$comment_extracted$Comment)) > input$in_wordcloud_min_word]),
                          value = max(table(unlist(value$comment_extracted$Comment))))
    })
    
    
    
    
    # word change
    observeEvent(input$word_change, {
        value$comment_extracted$Comment = word_filter2(value$comment_extracted$Comment, 
                                                       input$in_filter_from, input$in_filter_to)
        updateTextInput(session, "in_filter_from", value = "")
        updateTextInput(session, "in_filter_to", value = "")
    })
    
    
    
    
    # word delete
    observeEvent(input$word_delete, {
        value$comment_extracted$Comment = word_delete(value$comment_extracted$Comment, 
                                                      input$in_delete_word)
        updateTextInput(session, "in_delete_word", value = "")
    })
    
    
    
    
    # data table
    observeEvent(input$in_date_range, {
        
        if(is.null(value$comment) == F){
            # comment
            output$out_Comment = renderDataTable({
                datatable((value$comment %>% filter(between(ymd(str_sub(PublishTime, 1, 10)), 
                                                            ymd(input$in_date_range[[1]]),
                                                            ymd(input$in_date_range[[2]]))) %>% 
                               select("User", "Comment", "LikeCount")), options = list(
                                   initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().container()).css({'background-color': '#EEEEEE', 'color': '#000'});",
                                       "}")))
            })
            
            
            # data reply table
            output$out_Reply = renderDataTable({
                datatable(get_comment_reply(value$comment), 
                          options = list(
                              initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().container()).css({'background-color': '#EEEEEE', 'color': '#000'});",
                                  "}")))
            })
            
            
            # data reply network
            output$out_Reply_network = renderVisNetwork({
                
                visNetwork(
                    nodes = data.frame(
                        id = as.numeric(as.factor(unique(get_comment_reply(value$comment) %>% select(-Comment, -Reply) %>% unlist()))),
                        title = as.factor(unique(get_comment_reply(value$comment) %>% select(-Comment, -Reply) %>% unlist())),
                        data.frame(name = as.numeric(as.factor(unique(get_comment_reply(value$comment) %>% select(-Comment, -Reply) %>% unlist()))), 
                                   label = as.factor(unique(get_comment_reply(value$comment) %>% select(-Comment, -Reply) %>% unlist())))
                    ), 
                    edges = left_join(get_comment_reply(value$comment) %>% select(1,2), 
                                      data.frame(
                                          name = as.numeric(
                                              as.factor(
                                                  unique(
                                                      get_comment_reply(value$comment) %>% 
                                                          select(-Comment, -Reply) %>% 
                                                          unlist()))), 
                                          label = as.factor(
                                              unique(
                                                  get_comment_reply(value$comment) %>% 
                                                      select(-Comment, -Reply) %>% 
                                                      unlist()))) %>% 
                                          rename(Replier = label), by = "Replier") %>% 
                        left_join(data.frame(
                            name = as.numeric(
                                as.factor(
                                    unique(
                                        get_comment_reply(value$comment) %>% 
                                            select(-Comment, -Reply) %>% 
                                            unlist()))), 
                            label = as.factor(
                                unique(
                                    get_comment_reply(value$comment) %>% 
                                        select(-Comment, -Reply) %>% 
                                        unlist()))) %>% 
                                rename(Commenter = label), by = "Commenter") %>% 
                        select(3,4) %>% 
                        rename(from = name.x, to = name.y)
                ) %>% 
                    visIgraphLayout() %>% 
                    visNodes(
                        shape = "dot",
                        color = list(
                            background = "#0085AF",
                            border = "#013848",
                            highlight = "#FF8000"
                        ),
                        font = list(color = "white"),
                        shadow = list(enabled = T, size = 10)
                    ) %>% 
                    visEdges(shadow = FALSE,
                             color = list(color = "#99CCFF", highlight = "#C62F4B"),
                             arrows ="to") %>%
                    visOptions(highlightNearest = list(enabled = T, degree = 3, hover = T),
                               nodesIdSelection = TRUE) %>% 
                    visInteraction(navigationButtons = TRUE)
            })
            } else {
                output$out_Comment = renderDataTable({
                    datatable(value$text, options = list(
                        initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().container()).css({'background-color': '#EEEEEE', 'color': '#000'});",
                            "}")))})
            }
        
    })
    
    
    
    
    # trend
    observeEvent({
        input$in_filter_from
        input$in_filter_to
        input$in_delete_word
        input$in_trend_max_word
        input$in_trend_accumulation}, {
            
            if(input$in_trend_accumulation == T){
                output$out_Trend = plotly::renderPlotly({
                    plotly::ggplotly(
                        ggplot(get_ts_info(value$comment_extracted,
                                           value$comment_extracted$Comment,
                                           value$comment_extracted$PublishTime,
                                           min(value$comment_extracted$PublishTime),
                                           max(value$comment_extracted$PublishTime),
                                           input$in_trend_max_word,
                                           input$in_filter_from,
                                           input$in_filter_to,
                                           input$in_delete_word),
                               aes(x = Days, y = Freq_acc, color = Word)) +
                            geom_line() +
                            scale_x_date(date_labels = "%Y-%m-%d") +
                            ylab("누적 빈도") +
                            labs(color = "단어")) %>%
                            plotly::layout(xaxis = list(rangeslider = list(type = "date")))
                })
            } else {
                output$out_Trend = plotly::renderPlotly({
                    plotly::ggplotly(
                        ggplot(get_ts_info(value$comment_extracted,
                                           value$comment_extracted$Comment,
                                           value$comment_extracted$PublishTime,
                                           min(value$comment_extracted$PublishTime),
                                           max(value$comment_extracted$PublishTime),
                                           input$in_trend_max_word,
                                           input$in_filter_from,
                                           input$in_filter_to,
                                           input$in_delete_word),
                               aes(x = Days, y = Freq, color = Word)) +
                    geom_line() +
                    scale_x_date(date_labels = "%Y-%m-%d") +
                    ylab("빈도") +
                    labs(color = "단어")) %>%
                        plotly::layout(xaxis = list(rangeslider = list(type = "date")))
                })
            }
            
            
            
            
            # Trend Table
            output$out_DT_Trend =  renderDataTable({
                datatable(get_ts_table(value$comment_extracted), options = list(
                    initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().container()).css({'background-color': '#EEEEEE', 'color': '#000'});",
                        "}")))})
            
            })

    
    
    
    # chart & wordcloud & apriori
    observeEvent({
        input$in_date_range
        input$in_chart_min_word
        input$in_chart_max_word
        input$in_wordcloud_min_word
        input$in_wordcloud_max_word
        input$in_apriori_supp
        input$in_apriori_conf
        input$in_apriori_hierarchical}, {
        
        # chart
        output$out_Chart = plotly::renderPlotly({
            plotly::ggplotly(
                ggplot(get_comment_dataframe(value$comment_extracted$Comment, 
                                             value$comment_extracted$PublishTime,
                                             ymd(input$in_date_range[[1]]),
                                             ymd(input$in_date_range[[2]]),
                                             input$in_chart_max_word,
                                             input$in_filter_from,
                                             input$in_filter_to,
                                             input$in_delete_word) %>%  
                       filter(input$in_chart_min_word <= Freq), 
                   aes(x = reorder(Word, Freq), y = Freq, fill = Word)) +
                geom_col() + coord_flip() +
                theme(legend.position = "none") + ylab("빈도") + xlab("단어"))
        })
        
        output$out_DT_Chart = renderDataTable({
            datatable(get_comment_dataframe(value$comment_extracted$Comment,
                                            value$comment_extracted$PublishTime,
                                            ymd(input$in_date_range[[1]]),
                                            ymd(input$in_date_range[[2]]),
                                            input$in_chart_max_word,
                                            input$in_filter_from,
                                            input$in_filter_to,
                                            input$in_delete_word), 
                      options = list(
                          initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().container()).css({'background-color': '#EEEEEE', 'color': '#000'});",
                              "}")))
        })
        
        # wordcloud
        output$out_Wordcloud = renderWordcloud2({
            wordcloud2(get_comment_dataframe(value$comment_extracted$Comment,
                                             value$comment_extracted$PublishTime,
                                             ymd(input$in_date_range[[1]]),
                                             ymd(input$in_date_range[[2]]),
                                             input$in_wordcloud_max_word,
                                             input$in_filter_from,
                                             input$in_filter_to,
                                             input$in_delete_word) %>%  
                filter(input$in_wordcloud_min_word <= Freq))
        })
        
        
        output$out_DT_Wordcloud = renderDataTable({
            datatable(get_comment_dataframe(value$comment_extracted$Comment,
                                            value$comment_extracted$PublishTime,
                                            ymd(input$in_date_range[[1]]),
                                            ymd(input$in_date_range[[2]]),
                                            input$in_wordcloud_max_word,
                                            input$in_filter_from,
                                            input$in_filter_to,
                                            input$in_delete_word), 
                      options = list(
                          initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().container()).css({'background-color': '#EEEEEE', 'color': '#000'});",
                                  "}")))
            })
        
        # apriori
        output$out_Apriori = renderVisNetwork({
            
            ig_df = get_comment_rule(value$comment_extracted$Comment,
                                     value$comment_extracted$PublishTime,
                                     ymd(input$in_date_range[[1]]),
                                     ymd(input$in_date_range[[2]]),
                                     input$in_filter_from,
                                     input$in_filter_to,
                                     input$in_delete_word,
                                     input$in_apriori_supp,
                                     input$in_apriori_conf,
                                     table = F)
            
            # render network
            visNetwork(
                nodes = data.frame(
                    id = ig_df$vertices$name[1:sum(str_detect(ig_df$vertices$name, "assoc", negate = T))],
                    value = ig_df$vertices$lift[1:sum(str_detect(ig_df$vertices$name, "assoc", negate = T))],
                    title = ig_df$vertices$label[1:sum(str_detect(ig_df$vertices$name, "assoc", negate = T))],
                    ig_df$vertices %>% filter(between(row_number(), 1, sum(str_detect(ig_df$vertices$name, "assoc", negate = T))))
                ), 
                edges = left_join(ig_df$edges[1:(dim(ig_df$edges)[1]/2),] %>% rename(rul = to), 
                                  ig_df$edges[-1:-(dim(ig_df$edges)[1]/2),] %>% rename(rul = from), 
                                  by = "rul") %>% 
                    left_join(ig_df$vertices[-1:-sum(str_detect(ig_df$vertices$name, "assoc", negate = T)),] %>% 
                                  rename(rul = name), by = "rul") %>% select(-rul, -label, -count, -order) %>% rename(value = lift)
            ) %>% 
                visIgraphLayout() %>% 
                visNodes(
                    shape = "dot",
                    color = list(
                        background = "#0085AF",
                        border = "#013848",
                        highlight = "#FF8000"
                    ),
                    font = list(color = "white"),
                    shadow = list(enabled = T, size = 10)
                ) %>% 
                visEdges(shadow = FALSE,
                         color = list(color = "#99CCFF", highlight = "#C62F4B"),
                         arrows ="both") %>%
                visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T),
                           nodesIdSelection = TRUE) %>% 
                visInteraction(navigationButtons = TRUE) %>% 
                visHierarchicalLayout(enabled = input$in_apriori_hierarchical)
        })
        
        output$out_DT_Apriori = renderDataTable(inspectDT(get_comment_rule(value$comment_extracted$Comment,
                                                                           value$comment_extracted$PublishTime,
                                                                           ymd(input$in_date_range[[1]]),
                                                                           ymd(input$in_date_range[[2]]),
                                                                           input$in_filter_from,
                                                                           input$in_filter_to,
                                                                           input$in_delete_word,
                                                                           input$in_apriori_supp,
                                                                           input$in_apriori_conf,
                                                                           table = T), 
                                                          options = list(
                                                              initComplete = JS(
                                                                  "function(settings, json) {",
                                                                  "$(this.api().table().container()).css({'background-color': '#EEEEEE', 'color': '#000'});",
                                                                  "}"))))
    })
}


# Run the application 
shinyApp(ui = ui, server = server)


# rsconnect::setAccountInfo(name='rluver',
#                           token='',
#                           secret='')
# 
# deployApp()


