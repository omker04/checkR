# pkgs_reqd <- c('dplyr', 'stringr', 'stringi', 'magrittr', 'reshape2', 'ggpubr', 'data.table',
#                      'shiny', 'shinydashboard', 'shinyjs', 'shinyBS', 'shinycssloaders', 'ggplot2')
# installed_pkgs <- installed.packages()
# pkgs_to_install <- setdiff(pkgs_reqd, installed_pkgs)
# sapply(pkgs_to_install, function(x) install.packages(x, dependencies = TRUE))
# sapply(pkgs_reqd, function(x) library(x, character.only = TRUE))

library(dplyr)
library(stringr)
library(stringi)
library(magrittr)
library(reshape2)
library(ggpubr)


library(dplyr)
library(stringr)
library(stringi)
library(magrittr)
library(data.table)


##### Get Function and Variable Names ####
get_function_names <- function(parsed_code) {
    function_search_list <- c("=function",
                              "<-function",
                              "<-\\.",
                              "=\\.",
                              "<-reactive\\(",
                              "=reactive\\(")
    function_names <- sapply(function_search_list, function(x) {
        index = which(!is.na(str_match(
            string = gsub(" ",
                          "", as.expression(parsed_code)),
            pattern = x
        )))
        function_name <- lapply(strsplit(gsub(" ", "",
                                              parsed_code[index]), x), function(y)
                                                  y[1]) %>%
            unlist()
        return(function_name)
    })
    function_names %<>% unlist()
    names(function_names) <- NULL
    
    operator <- c("\\(", "\\[", "\\{", "\\$", "#", "\\)", '\\\\n', '\\"', "\\'", '\\,', '\\=')
    to_remove <- sapply(operator, function(x) {
        index <- which(!is.na(str_match(function_names,
                                        x)))
        return(index)
    }) %>% unlist()
    if (length(to_remove) != 0)
        function_names <- unique(function_names[-to_remove])
    
    return(gsub(' ', '', function_names))
}
get_all_function_names <- function(parsed_code) {
    main_body_functions <- get_function_names(parsed_code)
    substrings <- str_extract_all(parsed_code, "[^{}]+")
    sub_functions <- lapply(substrings, function(x) {
        char_x <- paste(x)
        func_x <- sapply(1:length(char_x), function(y)
            tryCatch({
                parsed_subset <- trimws(scan(
                    text = char_x[y],
                    what = "character",
                    sep = "\n"
                ), "left")
                function_names <- get_function_names(parsed_subset)
                return(function_names)
            }, error = function(cond) {
                return(NA)
            })) %>% 
            unlist() %>% na.omit() %>% unique()
    }) %>% unlist() %>% unique()
    all_functions <- c(main_body_functions, sub_functions)
    operators <- c("if",
                   "while",
                   "else",
                   "ifelse",
                   "for",
                   "dowhile",
                   "try",
                   "tryCatch")
    all_functions <- unique(all_functions[!all_functions %in%
                                              operators])
    return(all_functions)
}

get_variable_names <- function(parsed_code) {
    variable_search_list <- c("=", "<-", "<<-", "%<>%",
                              "input\\$", "output\\$")
    variable_names <- sapply(variable_search_list, function(x) {
        index <- which(!is.na(str_match(
            string = as.expression(parsed_code),
            pattern = x
        )))
        variable_name <- parsed_code[index]
        cleaned_name <- lapply(strsplit(gsub(" ", " ",
                                             variable_name), x), function(y)
                                                 y[1]) %>%
            unlist()
        return(cleaned_name)
    }) %>% unlist()
    if(!is.null(variable_names)){
        variable_names %>% strsplit(., split = " ") %>%
            lapply(., function(x)
                x[1]) %>% unlist()
        names(variable_names) <- NULL
        
        shiny_variables <- c("input\\$", "output\\$")
        for (x in shiny_variables) {
            index <- which(!is.na(str_match(variable_names,
                                            x)))
            variable_names[index] <- gsub(x, "", variable_names[index])
        }
        
        operator <- c("\\(", "\\[", "\\{", "\\$", "#", "\\)", '\\\\n', '\\"', "\\'", '\\,', '\\=')
        to_remove <- sapply(operator, function(x) {
            index <- which(!is.na(str_match(variable_names,
                                            x)))
            return(index)
        }) %>% unlist()
        if (length(to_remove) != 0)
            variable_names <- unique(variable_names[-to_remove])
        
        function_names <- get_function_names(parsed_code)
        if (!is.null(function_names) | length(function_names) !=
            0) {
            variable_names <- variable_names[!variable_names %in%
                                                 get_function_names(parsed_code)]
        }
        
        variable_names <- unique(variable_names[!variable_names %in%
                                                    c("", " ")])
        variable_names <- variable_names[!
                                             sapply(variable_names, function(x) 
                                                 strsplit(x, '') %>% 
                                                     unlist() %in% c(letters, LETTERS) %>% 
                                                     as.numeric() %>% paste0(collapse = '')
                                             ) %>% unlist() %>% 
                                             substr(., 1, 2) %in% c('00', '01', '0')
                                         ]
        return(gsub(' ', '', variable_names))
    } else {
        return(character(0))
    }
}
get_all_variable_names <- function(parsed_code) {
    main_body_variables <- get_variable_names(parsed_code)
    substrings <- str_extract_all(parsed_code, "[^{}]+")
    sub_variables <- lapply(substrings, function(x) {
        char_x <- paste(x)
        var_x <- sapply(1:length(char_x), function(y)
            tryCatch({
                parsed_subset <- trimws(scan(
                    text = char_x[y],
                    what = "character",
                    sep = "\n"
                ), "left")
                variable_names <- get_variable_names(parsed_subset)
                return(variable_names)
            }, error = function(cond) {
                return(NA)
            })) %>% 
            unlist() %>% na.omit() %>% unique()
    }) %>% unlist() %>% unique()
    all_variables <- c(main_body_variables, sub_variables)
    operators <- c("if",
                   "while",
                   "else",
                   "ifelse",
                   "for",
                   "dowhile",
                   "try",
                   "tryCatch")
    all_variables <- unique(all_variables[!all_variables %in%
                                              operators])
    return(all_variables)
}

##### Comented Code Check ####
whichComments <- function(loaded_file) {
    decision <- sapply(loaded_file, function(x) {
        y <- gsub(" ", "", x)
        if (strtrim(y, 1) == "#") {
            y <- "Comment"
        } else {
            y <- x
        }
        return(y)
    })
    return(which(decision == "Comment"))
}

mask_inline_comments <- function(code) {
    masked_code <- sapply(code, function(x) {
        text_single <- stri_extract_all_regex(x, '(?<=\').*?(?=\')') %>% unlist()
        text_double <- stri_extract_all_regex(x, '(?<=\").*?(?=\")') %>% unlist()
        text_single <- text_single[seq(1, length(text_single), 2)]
        text_double <- text_double[seq(1, length(text_double), 2)]
        text <- c(text_single, text_double)
        
        is_comment <- grep(pattern = '#', x = text)
        if(length(is_comment) > 0) {
            for(i in text[is_comment])
                x <- gsub(pattern = i, replacement = 'hAsHed', x = x, fixed = TRUE)
        }
        
        y <- sub(
            pattern = " + $",
            replacement = "",
            x = sub(
                pattern = "\\#.*",
                replacement = "",
                x = x
            )
        )
        
        if(length(is_comment) > 0) {
            for(i in text[is_comment])
                y <- gsub(pattern = 'hAsHed', replacement = i, x = y, fixed = TRUE)
        }
        
        return(y)
    })
    names(masked_code) <- NULL
    return(masked_code)
}

##### Check Length #####
get_length_score <- function(masked_code, line_wd) {
    each_line_length <- nchar(masked_code)
    length_marks <- 100 * sum(each_line_length < line_wd) / length(each_line_length)
    return(length_marks)
}

##### Check Spaces ####
get_operator_score <- function(masked_code) {
    operator <- list(c("if", "right"),
                     c("while", "right"),
                     c("for", "right"),
                     c("else", "both"))
    symbol <- list(
        c("<-", "both"),
        c("=", "both"),
        c("\\+", "both"),
        c("\\-", "both"),
        c("\\*", "both"),
        c("\\/", "both"),
        c("\\&", "left"),
        c("\\|", "left"),
        c("\\,", "right"),
        c("\\{", "left"),
        c("\\}", "right")
    )
    pad_objects <- function(object) {
        if (object[2] == "left")
            pad = paste0(" ", object[1])
        if (object[2] == "right")
            pad = paste0(object[1], " ")
        if (object[2] == "both")
            pad = paste0(" ", object[1], " ")
        return(pad)
    }
    
    only_operator <- lapply(c(operator, symbol), function(x)
        sum(!is.na(
            str_match(string = masked_code,
                      pattern = x[1])
        ))) %>% unlist()
    only_operator[8] <- only_operator[8] - only_operator[5]
    spaced_operator <- lapply(c(operator, symbol), function(x)
        sum(!is.na(
            str_match(string = masked_code,
                      pattern = pad_objects(x))
        ))) %>% unlist()
    only_operator <- only_operator - spaced_operator
    used <- which((spaced_operator + only_operator !=
                       0))
    spacing <- spaced_operator[used] / (spaced_operator +
                                            only_operator)[used]
    weights <- (spaced_operator + only_operator)[used] / sum((spaced_operator +
                                                                  only_operator)[used])
    spacing_score <- sum(spacing * weights)
    spacing_score <- spacing_score + 
        runif(1, 0, (1 - spacing_score) / 2)
    return(100 * spacing_score)
}


##### Assignment Check ####
get_assignment_score <- function(parsed_code, variable_names) {
    variable_equals <- c(paste0(variable_names, "="))
    variable_assign <- c(paste0(variable_names, "<-"))
    
    whole_code <- paste0(gsub(' ', '', parsed_code), collapse = ' ')
    count_mtx <- sapply(variable_names, function(x) {
        splits <- strsplit(whole_code, x) %>% unlist() %>% substr(., 1, 1)
        count_equals <- sum(!is.na(str_match(splits, '\\=')))
        count_assign <- sum(!is.na(str_match(splits, '\\<')))
        return(c(count_assign, count_equals))
    })
    Sys.time()
    
    assignment_score <- 100 * sum(count_mtx[1, ]) / sum(count_mtx)
    return(assignment_score)
}

##### Naming Consistency Check ####

get_naming_consistency_score <- function(parsed_code, variable_names) {
    split_by_underscore <- strsplit(variable_names, '_') %>% unlist()
    split_by_dot <- strsplit(variable_names, '\\.') %>% unlist()
    
    check_uppercase_underscore <- lapply(split_by_underscore, function(x) {
        strsplit(x, '') %>% 
            unlist() %in% 
            LETTERS %>% 
            as.numeric() %>% 
            paste0(collapse = '')
    }) %>% unlist()
    
    check_uppercase_dot <- lapply(split_by_dot, function(x) {
        strsplit(x, '') %>% 
            unlist() %in% 
            LETTERS %>% 
            as.numeric() %>% 
            paste0(collapse = '')
    }) %>% unlist()
    
    variable_names_one_word <- variable_names[variable_names %>% str_match(., '_') %>% is.na() %>% which()] %>% unique()
    check_variables <- lapply(variable_names_one_word, function(x) {
        strsplit(x, '') %>% 
            unlist() %in% 
            LETTERS %>% 
            as.numeric() %>% 
            paste0(collapse = '')
    }) %>% unlist()
    check_all_variables <- lapply(variable_names, function(x) {
        strsplit(x, '') %>% 
            unlist() %in% 
            LETTERS %>% 
            as.numeric() %>% 
            paste0(collapse = '')
    }) %>% unlist()
    
    uppercase_underscore <- sum(('10' == check_uppercase_underscore %>% substr(., 1, 2)) %>% sum(), 
                                sum(!is.na(str_match(string = check_uppercase_underscore, pattern = '010'))))
    underscore_consistency_score <- 100 * (length(check_uppercase_underscore) - uppercase_underscore) / length(check_uppercase_underscore)
    underscore_consistency_score <- max(underscore_consistency_score, 100 - underscore_consistency_score)
    
    uppercase <- sum(('10' == check_variables %>% substr(., 1, 2)) %>% sum(), 
                     sum(!is.na(str_match(string = check_variables, pattern = '010'))))
    consistency_score <- max(100, 100 * (length(check_variables) - uppercase * 1.15) / length(variable_names_one_word))
    consistency_score <- max(100 - consistency_score, consistency_score)
    ultimate_score <- mean(c(underscore_consistency_score, consistency_score), na.rm = TRUE)
    
    if (prod(check_uppercase_dot == check_all_variables) != 1) {
        uppercase_dot <- sum(('10' == check_uppercase_dot %>% substr(., 1, 2)) %>% sum(), 
                             sum(!is.na(str_match(string = check_uppercase_dot, pattern = '010'))))
        dot_consistency_score <- 50 * (length(check_uppercase_dot) - uppercase_dot) / length(check_uppercase_dot)
        
        ultimate_score <- mean(c(underscore_consistency_score, consistency_score, dot_consistency_score))
    }
    
    numeric_names <- variable_names[- (str_match(variable_names, '[0123456789]') %>% is.na() %>% which())]
    numeric_names_decode <- gsub('[0123456789]', '', numeric_names)
    numeric_score <- 1 - length(unique(numeric_names_decode)) / length(numeric_names)
    
    singleton_names <- (nchar(variable_names) == 1) %>% which()
    singleton_score <- length(singleton_names) / length(variable_names)
    bad_names <- (variable_names %in% c('df', 'mat', 'mtx', 'plot', 'list', 'img', 'image', 'gif')) %>% which()
    bad_name_score <- length(bad_names) / length(variable_names)
    
    convention_score <- 1 - mean(c(numeric_score, singleton_score, bad_name_score), na.rm = TRUE)
    
    final_naming_score <- ultimate_score * convention_score
    
    return(list('consistency_score' = ultimate_score,
                'naming_score' = convention_score,
                'aggregate_score' = final_naming_score))
}

##### ALL SCORES #####
code_score <- function(loaded_file, parsed_code, line_wd = 80) {
    comment_ind <- whichComments(loaded_file)
    comments <- loaded_file[comment_ind]
    if(length(comment_ind) > 0) {
        code <- loaded_file[-whichComments(loaded_file)]
    } else {
        code <- loaded_file
    }
    masked_code <- mask_inline_comments(code)
    
    pct_inline_comments <- 100 * sum(!is.na(
        str_match(string = code, pattern = "#")
    )) / length(code)
    
    uncommented_comments <- trimws(gsub(
        pattern = "#",
        replacement = "",
        x = comments
    ))
    uncommented_comments <-
        uncommented_comments[uncommented_comments != ""]
    
    commented_variables <- get_all_variable_names(uncommented_comments)
    commented_functions <- get_all_function_names(uncommented_comments)
    
    pct_commented_code <- 110 * length(
        c(commented_functions, commented_variables)
    ) / length(uncommented_comments)
    
    length_score <- get_length_score(masked_code, line_wd)
    
    space_score <- get_operator_score(parsed_code)
    
    all_variables <- c(get_all_function_names(parsed_code),
                       get_all_variable_names(parsed_code))
    
    assignment_score <- get_assignment_score(parsed_code, all_variables)
    
    naming_score <- get_naming_consistency_score(parsed_code, all_variables)
    
    all_scores <- list(
        'pct_inline_comments' = pct_inline_comments,
        'pct_comments' = 100 * length(comments) / length(c(comments, code)),
        'pct_commented_code' = pct_commented_code,
        'length_score' = length_score,
        'space_score' = space_score,
        'assignment_score' = assignment_score,
        'naming_consistency_score' = naming_score$consistency_score,
        'naming_convention_score' = naming_score$naming_score * 100,
        'naming_aggregate_score' = naming_score$aggregate_score
    )
    
    return(all_scores)
}


get_icon_color <- function(value) {
    if(!is.na(value)){
        if(value < 20) {
            my_icon <- icon('thumbs-down')
            my_color <- 'red'
        } else {
            if(value < 40) {
                my_icon <- icon('thumbs-o-down')
                my_color <- 'orange'
            } else {
                if(value < 60) {
                    my_icon <- icon('thumbs-o-up')
                    my_color <- 'yellow'
                } else {
                    if(value < 80) {
                        my_icon <- icon('thumbs-up')
                        my_color <- 'teal'
                    } else {
                        my_icon <- icon('hand-peace-o')
                        my_color <- 'green'
                    }
                }
            }
        }
        return(list(my_icon, my_color))
    } else {
        my_color <- 'maroon'
        my_icon <- icon('bug')
        return(list(my_icon, my_color))
    }
}

inline_comments_reco <- function(color) {
    if(color %in% c('red', 'orange')) {
        reco <- 'It is advised to add short inline comments in your code, 
        to enable better readability and understanding of the codes.'
    }
    if(color %in% c('yellow', 'teal')) {
        reco <- 'You are doing good with the inline comments. 
        A bit more explanation would help the readers and imply more reusability.'
    }
    if(color == 'green') {
        reco <- 'Great Job Buddy!! Way to Go !!'
    }
    return(reco)
    }

commented_code_reco <- function(color) {
    if(color %in% c('red', 'orange')) {
        reco <- 'You have a lot of codes, that have been commented out.'
    }
    if(color %in% c('yellow', 'teal')) {
        reco <- 'You should reduce commenting out your unused codes a bit more.'
    }
    if(color == 'green') {
        reco <- 'Great Job Buddy!! Way to Go !!'
    }
    return(reco)
}

naming_consistency <- function(color) {
    if(color %in% c('red', 'orange')) {
        reco <- 'It is advised to be consistent with your way of nomenclature, throughout the code.
        This enables better readibility and understandability of the code.'
    }
    if(color %in% c('yellow', 'teal')) {
        reco <- 'It is advised to be a bit more consistent with your way of nomenclature, throughout the code.
        This enables better readibility and understandability of the code.'
    }
    if(color == 'green') {
        reco <- 'You have done a Great Job with Naming Consistency !! Way to Go !!'
    }
    return(reco)
}

naming_convention <- function(color) {
    if(color %in% c('red', 'orange')) {
        reco <- 'The variables in your code, must be named in a way that the reader 
        understands what type of result the variable holds. Using names like data_1, data_2 etc. are absolutely discouraged.'
    }
    if(color %in% c('yellow', 'teal')) {
        reco <- 'The variables in your code, must be named in a way that the reader 
        understands what type of result the variable holds. Reduce the usage of names such as data_1, data_2 etc.'
    }
    if(color == 'green') {
        reco <- 'You have done a Great Job with Naming Convention!! Way to Go !!'
    }
    return(reco)
}

source('format_code.R')