library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(ggplot2)
library(ggpubr)
library(reshape2)
# 
# print(2-3)
header <- dashboardHeader(title = h3(strong('Check your R-Code')), titleWidth = '400px')
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
    useShinyjs(),
    
    
    fluidPage(
        fileInput(inputId = 'code', 
                  label = h3('Please Upload Your R-Code(s) (the ones you desire to be checked) :'), 
                  multiple = TRUE, accept = '.R', width = '100%'),
        
        box(title = strong('Coding Guidelines'), status = 'info', solidHeader = TRUE, 
            width = 12, collapsible = TRUE, collapsed = TRUE, 
            fluidPage(column(width = 6, 
                             withSpinner(imageOutput(outputId = 'naming', height = '100%'), type = 1),
                             hr(),
                             imageOutput(outputId = 'braces', height = '100%'),
                             hr(),
                             imageOutput(outputId = 'comments', height = '100%')
            ),
            column(width = 6, 
                   withSpinner(imageOutput(outputId = 'spacing', height = '100%'), type = 1),
                   hr(),
                   imageOutput(outputId = 'indentation', height = '100%')
            )
            )
        ),
        
        box(title = strong('Your Score'), status = 'primary', solidHeader = TRUE, width = 12,
            fluidPage(
                column(width = 4, 
                       div(img(imageOutput('gif0')), style="text-align: center;"),
                       shinyjs::hidden(valueBoxOutput('naming_score', width = 12)),
                       br(),
                       shinyjs::hidden(uiOutput('help_naming_consistency')),
                       br(),
                       shinyjs::hidden(uiOutput('help_naming_convention'))
                ),
                column(width = 4, 
                       div(img(imageOutput('gif')), style="text-align: center;"),
                       shinyjs::hidden(valueBoxOutput('comments_pct', width = 12)),
                       shinyjs::hidden(valueBoxOutput('comments_score', width = 12)),
                       shinyjs::hidden(uiOutput('help_comments')),
                       shinyjs::hidden(valueBoxOutput('inline_comments', width = 12)),
                       shinyjs::hidden(uiOutput('help_inline_comments'))
                ),
                column(width = 4, 
                       div(img(imageOutput('gif1')), style="text-align: center;"),
                       shinyjs::hidden(numericInput(inputId = 'line_length_num', label = '', min = 70, max = 120, step = 5, value = 80)),
                       shinyjs::hidden(valueBoxOutput('line_length_score', width = 12)),
                       shinyjs::hidden(valueBoxOutput('spacing_score', width = 12)),
                       shinyjs::hidden(valueBoxOutput('assignment_score', width = 12))
                )
            )
        ),
        shinyjs::hidden(div(id = 'individual_results',
                            # column(offset = 4, width = 8,
                            box(title = strong('Individual Script Score'), status = 'warning', solidHeader = TRUE, collapsible = TRUE, width = 12,
                                fluidPage(
                                    withSpinner(plotOutput('individual_plot', height = '700px'), 
                                                type = 3, color.background = 'white', color = 'orange')
                                )
                                #        )
                            ))
        ),
        shinyjs::hidden(div(id = 'reformat',
                            box(title = 'Code Reformat', status = 'success', solidHeader = TRUE, width = 12, #another inline comment
                                fluidPage(
                                    div(id = 'reformat_edit', 
                                        column(width = 8,
                                               column(width = 8,
                                                      column(width = 6,
                                                             checkboxInput(inputId = 'operator_spaces', label = h4(strong('Space around Operators')), value = TRUE),
                                                             checkboxInput(inputId = 'reflow_comments', label = h4(strong('Reflow Comments')), value = TRUE),
                                                             checkboxInput(inputId = 'reflow_code', label = h4(strong('Reflow Code')), value = TRUE),
                                                             numericInput(inputId = 'line_width', label = h4(strong('Line Width')), min = 60, max = 100, value = 80, step = 2, width = '50%')
                                                      ),
                                                      column(width = 6,
                                                             checkboxInput(inputId = 'keep_blank_lines', label = h4(strong('Keep Blank Lines')), value = TRUE),
                                                             checkboxInput(inputId = 'else_new_line', label = h4(strong('Else in New Line')), value = TRUE),
                                                             checkboxInput(inputId = 'brace_new_line', label = h4(strong('Braces in New Line')), value = FALSE)
                                                      )
                                               ),
                                               column(width = 4,
                                                      # uiOutput(outputId = 'filenames'),
                                                      checkboxGroupInput(inputId = 'file_to_reformat', 
                                                                         label = h3(strong('Select the files to Reformat :')), 
                                                                         choices = list(),
                                                                         selected = list()
                                                      ),
                                                      actionLink(inputId = 'clean_my_code', label = h4(strong('Click to Reformat selected Code(s)'))) 
                                               )
                                        ),
                                        column(width = 4,
                                               helpText(h4(strong('Dont Forget to Re-Indent the formatted codes, after downloading them.'))),
                                               helpText(h4(strong('To re-indent, open the code in R-Studio and select Code -> Reindent Lines.')))
                                        )
                                    ),
                                    div(img(imageOutput('gif_format')), style="text-align: center;"),
                                    div(id = 'after_edit', 
                                        column(width = 12,
                                               div(h2(strong('Dont Forget to Re-Indent the formatted codes, after downloading them.')), style = "color:blue"),
                                               div(h2('To Re-Indent the codes :'), style = 'color:blue'),
                                               code(h4(strong('* Open the Codes in R-Studio'))),
                                               #br(),
                                               code(h4(strong('* Select the Entire Code by Ctrl + A (for Windows users) or Cmd + A (for MAC users)'))),
                                               #br(),
                                               code(h4(strong("* Re-Indent by 'Code' > 'Reindent Lines' or `Ctrl + I` (for Windows users) or `Cmd + I` (for MAC users)."))),
                                               br(),
                                               hr(),
                                               downloadLink(outputId = 'download', label = h1('Download Re-formatted Codes.'))
                                        ))
                                )
                            )
        ))
    )
    
)

ui <- dashboardPage(header = header, 
                    sidebar = sidebar, 
                    body = body,
                    title = 'Check Your R-Code', skin = 'red')

