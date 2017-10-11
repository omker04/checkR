shinyServer(function(input, output, session){
    
    shinyjs::hide(id = 'gif')
    shinyjs::hide(id = 'gif0')
    shinyjs::hide(id = 'gif1')
    shinyjs::hide(id = 'gif_format')
    shinyjs::hide(id = 'download')
    shinyjs::hide(id = 'after_edit', anim = TRUE)
    ##### Get Best Practice Images ####
    output$naming <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        # filename <- normalizePath(paste('naming', '.png', sep=''))
        
        # Return a list containing the filename
        list(src = 'naming.png', width = '100%', height = '100%')
    }, deleteFile = FALSE)
    
    output$braces <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        # filename <- normalizePath(paste('braces', '.png', sep=''))
        
        # Return a list containing the filename
        list(src = 'braces.png', width = '100%', height = '100%')
    }, deleteFile = FALSE)
    
    output$comments <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        filename <- normalizePath(paste('comments', '.png', sep=''))
        
        # Return a list containing the filename
        list(src = filename, width = '100%', height = '100%')
    }, deleteFile = FALSE)
    
    output$spacing <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        filename <- normalizePath(paste('spacing', '.png', sep=''))
        
        # Return a list containing the filename
        list(src = filename, width = '100%', height = '100%')
    }, deleteFile = FALSE)
    
    output$indentation <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        filename <- normalizePath(paste('indentation', '.png', sep=''))
        
        # Return a list containing the filename
        list(src = filename, width = '100%', height = '100%')
    }, deleteFile = FALSE)
    
    ##### Read Uploaded Code(s) ####
    files <- reactiveValues(loaded_file = NULL, parsed_code = NULL, edited_file = NULL)
    scores <- reactiveValues(all = NULL)
    
    read_file <- observeEvent(input$code, {
        # files <- reactiveValues(loaded_file = NULL, parsed_code = NULL, edited_file = NULL)
        # scores <- reactiveValues(all = NULL)
        
        files$loaded_file <- NULL
        files$parsed_code <- NULL
        files$edited_file <- NULL
        scores$all <- NULL
        
        
        file_names <- input$code$datapath
        loaded_files <- sapply(file_names, function(x) 
            readLines(con = x)
        )
        parsed_codes <- sapply(file_names, function(x)
            parse(file = x)
        )
        
        if(length(file_names) == 1) {
            loaded_files <- list(loaded_files)
            names(loaded_files) <- input$code$name
            parsed_codes <- list(parsed_codes)
            names(parsed_codes) <- input$code$name
        } else {
            names(loaded_files) <- input$code$name
            names(parsed_codes) <- input$code$name
        }
        files$loaded_file <- loaded_files
        files$parsed_code <- parsed_codes
    })
    
    ##### Calculate Scores #####
    observeEvent(input$code, {
        shinyjs::hide(id = 'naming_score', anim = TRUE)
        shinyjs::hide(id = 'spacing_score', anim = TRUE)
        shinyjs::hide(id = 'line_length_score', anim = TRUE)
        shinyjs::hide(id = 'assignment_score', anim = TRUE)
        shinyjs::hide(id = 'comments_score', anim = TRUE)
        shinyjs::hide(id = 'inline_comments', anim = TRUE)
        shinyjs::hide(id = 'comments_pct', anim = TRUE)
        shinyjs::hide(id = 'help_comments', anim = TRUE)
        shinyjs::hide(id = 'help_inline_comments', anim = TRUE)
        shinyjs::hide(id = 'help_naming_convention', anim = TRUE)
        shinyjs::hide(id = 'help_naming_consistency', anim = TRUE)
        shinyjs::show(id = 'gif', anim = TRUE)
        shinyjs::hide(id = 'reformat', anim = TRUE)
        shinyjs::hide(id = 'individual_results', anim = TRUE)
        
        step <- 1 / (1 + length(files$loaded_file))
        withProgress(message = 'Evaluating Coding Standards for :', {
            scores$all <- sapply(names(files$loaded_file), function(x) {
                incProgress(amount = step, detail = x)
                
                loaded_file <- files$loaded_file[x]
                parsed_code <- files$parsed_code[x]
                
                while(is.list(loaded_file)) {
                    loaded_file <- loaded_file[[1]]
                }
                while(is.list(parsed_code)) {
                    parsed_code <- parsed_code[[1]]
                }
                
                all_scores <- code_score(loaded_file, parsed_code)
                return(all_scores)
            })
            scores$line_length <- sapply(names(files$loaded_file), function(x) {
                loaded_file <- files$loaded_file[x]
                while(is.list(loaded_file)) {
                    loaded_file <- loaded_file[[1]]
                }
                line_length <- length(loaded_file)
                return(line_length)
            })
            # printt(scores$line_length)
            scores$line_length <- scores$line_length / sum(scores$line_length)
            
            scores$aggregate <- scores$all %>% 
                as.data.frame() %>% 
                apply(., 1, function(x) 
                    round(sum(as.numeric(x) * scores$line_length, na.rm = TRUE), 2)
                )
        })
        
        
        updateCheckboxGroupInput(session, 
                                 inputId = 'file_to_reformat', 
                                 choices = input$code$name, 
                                 selected = input$code$name)
        
        shinyjs::show(id = 'naming_score', anim = TRUE)
        shinyjs::show(id = 'spacing_score', anim = TRUE)
        shinyjs::show(id = 'line_length_score', anim = TRUE)
        shinyjs::show(id = 'assignment_score', anim = TRUE)
        shinyjs::show(id = 'comments_score', anim = TRUE)
        shinyjs::show(id = 'inline_comments', anim = TRUE)
        shinyjs::show(id = 'help_comments', anim = TRUE)
        shinyjs::show(id = 'comments_pct', anim = TRUE)
        shinyjs::show(id = 'help_inline_comments', anim = TRUE)
        shinyjs::show(id = 'help_naming_convention', anim = TRUE)
        shinyjs::show(id = 'help_naming_consistency', anim = TRUE)
        shinyjs::hide(id = 'gif', anim = TRUE)
        
        shinyjs::show(id = 'reformat', anim = TRUE)
        shinyjs::show(id = 'reformat_edit', anim = TRUE)
        shinyjs::hide(id = 'gif_format', anim = TRUE)
        shinyjs::hide(id = 'after_edit', anim = TRUE)
    })
    
    ##### Show GIF #####
    output$gif <- renderImage({
        list(src = "walmartlabs_bolt.gif",
             contentType = 'image/gif'
        )
    }, deleteFile = FALSE)
    
    output$gif_format <- renderImage({
        list(src = "in_progress.gif",
             contentType = 'image/gif'
        )
    }, deleteFile = FALSE)
    
    ##### Show Scores ####
    output$spacing_score <- renderValueBox({
        valueBox(value = paste(scores$aggregate['space_score'], '%'),
                 subtitle = 'Spacing Score', 
                 color = get_icon_color(scores$aggregate['space_score'])[[2]], 
                 icon = get_icon_color(scores$aggregate['space_score'])[[1]], width = 12)
    })
    output$line_length_score <- renderValueBox({
        valueBox(value = paste(scores$aggregate['length_score'], '%'),
                 subtitle = 'Length Score', 
                 color = get_icon_color(scores$aggregate['length_score'])[[2]], 
                 icon = get_icon_color(scores$aggregate['length_score'])[[1]], width = 12)
    })
    output$assignment_score <- renderValueBox({
        valueBox(value = paste(scores$aggregate['assignment_score'], '%'),
                 subtitle = 'Assignment Score', 
                 color = get_icon_color(scores$aggregate['assignment_score'])[[2]], 
                 icon = get_icon_color(scores$aggregate['assignment_score'])[[1]], width = 12)
    })
    output$comments_score <- renderValueBox({
        valueBox(value = paste(scores$aggregate['pct_commented_code'], '%'),
                 subtitle = 'Commented / Un-Used Code', 
                 color = get_icon_color(100 - 2 * scores$aggregate['pct_commented_code'])[[2]], 
                 icon = get_icon_color(100 - 2 * scores$aggregate['pct_commented_code'])[[1]], width = 12)
    })
    output$inline_comments <- renderValueBox({
        valueBox(value = paste(scores$aggregate['pct_inline_comments'], '%'),
                 subtitle = 'Inline Comments', 
                 color = get_icon_color(5 * scores$aggregate['pct_inline_comments'])[[2]], 
                 icon = get_icon_color(5 * scores$aggregate['pct_inline_comments'])[[1]], width = 12)
    })
    output$naming_score <- renderValueBox({
        valueBox(value = paste(scores$aggregate['naming_aggregate_score'], '%'),
                 subtitle = 'Aggregate Naming Score', 
                 color = get_icon_color(scores$aggregate['naming_aggregate_score'])[[2]], 
                 icon = get_icon_color(scores$aggregate['naming_aggregate_score'])[[1]], width = 12)
    })
    output$comments_pct <- renderValueBox({
        valueBox(value = paste(scores$aggregate['pct_comments'], '%'),
                 subtitle = 'Commented Lines', 
                 color = get_icon_color(4 * scores$aggregate['pct_comments'])[[2]], 
                 icon = get_icon_color(4 * scores$aggregate['pct_comments'])[[1]], width = 12)
    })
    
    ##### Show Recommendations ####
    output$help_comments <- renderUI({
        helpText(
            commented_code_reco(get_icon_color(100 - 2 * scores$aggregate['pct_commented_code'])[[2]])
        )
    })
    output$help_inline_comments <- renderUI({
        helpText(
            inline_comments_reco(get_icon_color(5 * scores$aggregate['pct_inline_comments'])[[2]])
        )
    })
    output$help_naming_convention <-  renderUI({
        hr()  
        helpText(
            naming_convention(get_icon_color(scores$aggregate['naming_convention_score'])[[2]])
        )
    })
    output$help_naming_consistency <- renderUI({
        hr()
        helpText(
            naming_consistency(get_icon_color(scores$aggregate['naming_consistency_score'])[[2]])
        )
    })
    
    ##### Individual Scores ####
    observe({
        if(length(input$code$name) > 1) {
            shinyjs::show(id = 'individual_results')
        } else {
            shinyjs::hide(id = 'individual_results')
        }
    })
    
    output$individual_plot <- renderPlot({
        scores_df <- scores$all %>% 
            unlist() %>% 
            matrix(., nrow = 9) %>% 
            data.frame() %>% 
            set_colnames(input$code$name) %>% 
            mutate('score_criterion' = c('pct_inline_comments',
                                         'pct_comments',
                                         'pct_commented_code',
                                         'length_score',
                                         'space_score',
                                         'assignment_score',
                                         'naming_consistency_score',
                                         'naming_convention_score',
                                         'naming_aggregate_score'))
        scores_df[is.na(scores_df)] <- 0
        scores_df$aggregate <- scores$aggregate
        scores$df <- melt(scores_df, id.vars = 'score_criterion') %>% 
            set_colnames(c('score_criterion', 'script_name', 'score'))
        
        plot <- ggdotchart(scores$df, x = "score_criterion", y = "score",
                           color = "script_name",                                # Color by groups
                           #sorting = "descending",                       # Sort value in descending order
                           rotate = TRUE,                                # Rotate vertically
                           dot.size = 5,                                 # Large dot size
                           #y.text.col = FALSE,                            # Color y text by groups
                           ggtheme = theme_classic(),                        # ggplot2 theme
                           font.label = list(size = 9, style = 'bold', color = 'black')
        ) +
            theme_cleveland()
        
        return(plot)
    })
    
    ##### Re-format #####
    # output$filenames <- renderUI({
    #     checkboxGroupInput(inputId = 'file_to_reformat', 
    #                        label = h3(strong('Select the files to Reformat :')), 
    #                        choices = input$code$name,
    #                        selected = input$code$name
    #     )
    # })
    
    
    reformat_code <- eventReactive(input$clean_my_code, {
        files_to_reformat <- input$file_to_reformat
        print(files_to_reformat)
        
        step <- 1 / (1 + length(files_to_reformat))
        withProgress(message = 'Re-formating Code for', value = 0, {
            edited_file <- sapply(files_to_reformat, function(x) {
                incProgress(amount = step, detail = x)
                keep_blank_lines <- input$keep_blank_lines
                reflow_comments <- input$reflow_comments
                line_width <<- input$line_width
                else_new_line <- input$else_new_line
                brace_new_line <- input$brace_new_line
                operator_spaces <- input$operator_spaces
                reflow_codes <- input$reflow_code
                
                z <- files$loaded_file[x][[1]]
                z <- enable_operator_space(
                    get_blank_lines(
                        else_in_new_line(
                            brace_in_new_line(z, brace_new_line), 
                            else_new_line), 
                        keep_blank_lines),
                    operator_spaces)
                # whatever <<- z
                # print(z)
                if(reflow_codes) {
                    if (operator_spaces) {
                        z <- full_code_reflow(z)
                    } else {
                        z <- enable_operator_space(
                            full_code_reflow(
                                enable_operator_space(z, TRUE)
                            ), FALSE)
                        
                    }
                }
                if(reflow_comments) {
                    z <- comments_reflow(z)
                }
                return(z)
            })
        })
        
        return(edited_file)
    })
    
    observeEvent(input$clean_my_code, {
        shinyjs::show(id = 'gif_format', anim = TRUE)
        shinyjs::hide(id = 'reformat_edit', anim = TRUE)
        delay(500, {
            files_to_reformat <- input$file_to_reformat
            
            step <- 1 / (1 + length(files_to_reformat))
            # withProgress(message = 'Re-formating Code for', value = 0, {
            #     files$edited_file <- sapply(files_to_reformat, function(x) {
            #         incProgress(amount = step, detail = x)
            #         keep_blank_lines <- input$keep_blank_lines
            #         reflow_comments <- input$reflow_comments
            #         line_width <<- input$line_width
            #         else_new_line <- input$else_new_line
            #         brace_new_line <- input$brace_new_line
            #         operator_spaces <- input$operator_spaces
            #         reflow_codes <- input$reflow_code
            # 
            #         z <- files$loaded_file[x][[1]]
            #         z <- enable_operator_space(
            #             get_blank_lines(
            #                 else_in_new_line(
            #                     brace_in_new_line(z, brace_new_line),
            #                     else_new_line),
            #                 keep_blank_lines),
            #             operator_spaces)
            #         # whatever <<- z
            #         # print(z)
            #         if(reflow_codes) {
            #             z <- full_code_reflow(z)
            #         }
            #         if(reflow_comments) {
            #             z <- comments_reflow(z)
            #         }
            #         return(z)
            #     })
            # })
            
            files$edited_file <- reformat_code()
            shinyjs::show(id = 'after_edit', anim = TRUE)
            shinyjs::show(id = 'download', anim = TRUE)
            shinyjs::hide(id = 'gif_format', anim = TRUE)
        })
    })
    
    
    
    
    
    ##### Download ####
    output$download <- downloadHandler(
        filename = 'Reformatted_R_Codes.zip',
        content = function(folderName){
            tmpdir <- tempdir()
            setwd(tempdir())
            fileNames <- gsub(pattern = '.r$', replacement = '_edited.R', x = 
                gsub(pattern = '.R$', replacement = '_edited.R', x = input$file_to_reformat)
            )
            print(fileNames)
            if(length(fileNames) == 1) {
                writeLines(text = files$edited_file, 
                           con = fileNames)
            } else {
                
                for(i in 1:length(fileNames)){
                    print(files$edited_file[input$file_to_reformat[i]])
                    print(files$edited_file[[i]])
                    writeLines(text = files$edited_file[[i]], 
                               con = fileNames[i])
                }
            }
            zip(zipfile = folderName, files = fileNames)
        },
        contentType = "application/zip"
    )
})
