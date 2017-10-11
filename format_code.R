# source <- 'global.R'
# keep_blank_lines <- TRUE
# keep_comments <- TRUE
# reflow_comments <- TRUE
# line_width <- 80
# else_new_line <- FALSE
# brace_new_line <- FALSE
# operator_spaces <- TRUE
# reflow_codes <- TRUE
# 
# loaded_file <- readLines(source)

get_blank_lines <- function(loaded_file, keep_blank_lines) {
    trimmed_code <- trimws(loaded_file)
    blank <- which(trimmed_code == '')
    
    if (!keep_blank_lines) {
        loaded_file <- loaded_file[-blank]
    }
    return(loaded_file)
}

reflow <- function(text, text_pad, text_break, text_split_spaced) {
    text_split <- ifelse(text_split_spaced == ' ', text_split_spaced, trimws(text_split_spaced))
    text <- trimws(text)
    if(nchar(text_pad) >= 70) {
        reflowed_line <- paste0(
            text_pad, 
            gsub(pattern = text_split,
                              replacement = paste(text_split, '\n', text_pad),
                              x = text)
        )
    } else {
        line_break <- strsplit(text, text_split) %>%
            lapply(., function(x) nchar(x) + 1) %>%
            unlist() %>%
            cumsum() %>%
            cut(., breaks = seq(1, nchar(text) + text_break, text_break)) %>%
            as.numeric()
        if (length(line_break) > 1 & 
            uniqueN(line_break) == 1 & 
            ! 1 %in% unique(line_break)) {
            line_break[1] <- 1
        }
        reflowed_line <- strsplit(text, text_split) %>%
            unlist() %>%
            split(., line_break) %>%
            lapply(., function(x)
                paste0(c(text_pad, paste0(x, collapse = text_split), '\n'), collapse = ' ')
            ) %>%
            unlist() %>%
            paste0(., collapse = '') %>%
            substr(., 1, nchar(.) - 2)
        reflowed_line <- gsub(pattern = text_split,
                              replacement = gsub(
                                  pattern = '\\', 
                                  replacement = '', 
                                  x = text_split, 
                                  fixed = TRUE),
                              x = reflowed_line, fixed = TRUE) %>% 
            gsub(pattern = ' \n', 
                 replacement = paste0(text_split, ' \n'), 
                 x = .)
        
        # split_size <- nchar(text_split_spaced)
        # last_chars_text <- text %>% strsplit(., '') %>% unlist() %>% tail(split_size) %>% paste0(., collapse = '')
        # last_chars_reflow <- reflowed_line %>% strsplit(., '') %>% unlist() %>% tail(split_size) %>% paste0(., collapse = '')
        # if (last_chars_text %like% text_split &
        #     trimws(last_chars_text) != trimws(last_chars_reflow)) {
        #     reflowed_line <- paste0(reflowed_line, last_chars_text)
        # }
    }
    new_line_length <- reflowed_line %>% 
        gsub(text_pad, '', .) %>% 
        strsplit(., '\n') %>% 
        unlist() %>% 
        nchar()
    
    if(sum(new_line_length < 0.05 * line_width) > 0) {
        which_small <- which(new_line_length < 0.05 * line_width)
        new_reflow <- reflowed_line %>% 
            gsub(text_pad, '', .) %>% 
            strsplit(., '\n') %>% 
            unlist()
        for (i in which_small)
            new_reflow[i - 1] <- paste(new_reflow[i - 1], new_reflow[i])
        reflowed_line <- paste0(
            text_pad, 
            new_reflow[-which_small] %>% 
            paste(., collapse = paste0('\n', text_pad))
        )
    }
    return(reflowed_line)
}

comment_reflow <- function(long_comment) {
    first_letter <- which(!(strsplit(long_comment, '') %>% unlist()) %in% c(' ', '#')) %>% head(1)
    comment_pad <- substr(long_comment, 1, first_letter - 1)
    comment_text <- substr(long_comment, first_letter, nchar(long_comment))
    text_break <- line_width - first_letter
    reflowed_comment <- reflow(text = comment_text, 
                               text_pad = comment_pad, 
                               text_break = text_break, 
                               text_split_spaced = ' ')
    # line_break <- strsplit(comment_text, ' ') %>% 
    #     lapply(., function(x) nchar(x) + 1) %>% 
    #     unlist() %>% 
    #     cumsum() %>% 
    #     cut(., breaks = seq(1, nchar(comment_text) + text_break, text_break)) %>% 
    #     as.numeric()
    # reflowed_comment <- strsplit(comment_text, ' ') %>% 
    #     unlist() %>% 
    #     split(., line_break) %>% 
    #     lapply(., function(x) 
    #         paste0(c(comment_pad, x, '\n'), collapse = ' ')
    #     ) %>% 
    #     unlist() %>% 
    #     paste0(., collapse = '') %>% 
    #     substr(., 1, nchar(.) - 2)
    return(reflowed_comment)
}

comments_reflow <- function(loaded_file) {
    comment_line <- whichComments(loaded_file)
    comments <- comment_line %>% names()
    long_comments <- comments[nchar(comments) > 80]
    if (length(long_comments) != 0) {
        new_comments <- sapply(long_comments, function(x) comment_reflow(x))
        loaded_file[comment_line[nchar(comments) > 80]] <- new_comments
        
        new_code <- loaded_file %>% 
            strsplit(., split = '\n') %>% 
            lapply(., function(x) {
                if(length(x) == 0) {
                    return('')
                } else {
                    return(x)
                }
            }) %>% 
            unlist()
        
        return(new_code)
    } else {
        return(loaded_file)
    }
}

else_in_new_line <- function(loaded_file, else_new_line) {
    else_line_nbr <- which(loaded_file %like% 'else')
    else_lines <- loaded_file[else_line_nbr]
    first_letter <- lapply(strsplit(else_lines, ''), function(x)
        which(!x %in% c(' ', '#')) %>% head(1)
    ) %>% unlist()
    # if(first_letter == '#') {
    #     return(loaded_file)
    # } else {
    else_pad <- substr(loaded_file[else_line_nbr], 1, first_letter - 1) %>% 
        set_names(., else_line_nbr)
    else_lines %<>% 
        set_names(., else_line_nbr) %>% 
        gsub(' ', '', .)
    new_line_else <- names(else_lines)[else_lines %in% 'else{'] %>% as.numeric()
    same_line_else <- names(else_lines)[else_lines %in% '}else{'] %>% as.numeric()
    if(else_new_line) {
        if(length(same_line_else) != 0) {
        loaded_file[same_line_else] <- paste(else_pad[as.character(same_line_else)], '}\n', 
                                             else_pad[as.character(same_line_else)], 'else {')
        }
    } else {
        if(length(new_line_else) != 0) {
        loaded_file[new_line_else] <- paste(else_pad[as.character(new_line_else)], '} else {')
        loaded_file <- loaded_file[-c(new_line_else - 1)]
        }
    }
    # return(loaded_file)
    new_code <- loaded_file %>% 
        strsplit(., split = '\n') %>% 
        lapply(., function(x) {
            if(length(x) == 0) {
                return('')
            } else {
                return(x)
            }
        }) %>% 
        unlist()
    
    return(new_code)
}

brace_in_new_line <- function(loaded_file, brace_new_line) {
    brace_line_nbr <- which(loaded_file %like% "\\{")
    brace_lines <- loaded_file[brace_line_nbr]
    first_letter <- lapply(strsplit(brace_lines, ''), function(x)
        which(!x %in% c(' ', '#')) %>% head(1)
    ) %>% unlist()
    # if(first_letter == '#') {
    #     return(loaded_file)
    # } else {
    brace_pad <- substr(loaded_file[brace_line_nbr], 1, first_letter - 1) %>% 
        set_names(., brace_line_nbr)
    brace_lines %<>% 
        set_names(., brace_line_nbr) %>% 
        trimws()
    new_line_brace <- names(brace_lines)[brace_lines == '{'] %>% as.numeric()
    same_line_brace <- names(brace_lines)[brace_lines != '{'] %>% as.numeric()
    if(brace_new_line) {
        if(length(same_line_brace) != 0) {
        spaced_text <- brace_lines[as.character(same_line_brace)]
        no_space_text <- trimws(spaced_text)
        pre_brace_text <- substr(no_space_text, 1, nchar(no_space_text) - 1)
        loaded_file[same_line_brace] <- paste(brace_pad[as.character(same_line_brace)], 
                                              pre_brace_text, '\n', 
                                              brace_pad[as.character(same_line_brace)], '{', sep = '')
        }
    } else {
        if(length(new_line_brace) != 0) {
            previous_line <- loaded_file[new_line_brace - 1]
            loaded_file[new_line_brace] <- paste0(previous_line, '{')
            loaded_file <- loaded_file[-c(new_line_brace - 1)]
        }
    }
    # return(loaded_file)
    new_code <- loaded_file %>% 
        strsplit(., split = '\n') %>% 
        lapply(., function(x) {
            if(length(x) == 0) {
                return('')
            } else {
                return(x)
            }
        }) %>% 
        unlist()
    
    return(new_code)
}

enable_operator_space <- function(loaded_file, operator_spaces) {
    operator <- list(c("if\\(", "middle"),
                     c("while\\(", "middle"),
                     c("for\\(", "middle"),
                     c("else\\{", "middle"))
    symbol <- list(
        c("<-", "both"),
        c("=", "both"),
        c('==', 'both'),
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
        if (object[2] == "middle") {
            n <- nchar(object[1])
            pad <- paste0(substr(object[1], 1, n-2), ' ', substr(object[1], n-1, n))
        }
            
        return(pad)
    }
    # pad_operator <- function(operator) {
    #     pad = paste0(operator, collapse = ' ')
    #     return(pad)
    # }
    
    text_single <- stri_extract_all_regex(loaded_file, '(?<=\').*?(?=\')') %>% 
        sapply(., function(x) {
            return(x[seq(1, length(x), 2)])
        })
    text_double <- stri_extract_all_regex(loaded_file, '(?<=\").*?(?=\")') %>% 
        sapply(., function(x) {
            return(x[seq(1, length(x), 2)])
        })
    texts <- sapply(1:length(loaded_file), function(x) return(c(text_single[[x]], text_double[[x]]))) %>% 
        set_names(., 1:length(loaded_file)) %>% 
        sapply(., function(x) {
            if(prod(is.na(x)) == 0) {
                return(x[!is.na(x)])
            }
        })
    all_texts <- texts %>% unlist() %>% unique()
    
    
    
    for(i in 1:length(c(symbol, operator))) {
        
        symbol_presence <- which(!is.na(stri_match(texts, regex = c(symbol, operator)[[i]][1])))
        #sapply(symbol_presence, function(x) {
        for (x in symbol_presence) {
            text_symbol <- which(!is.na(stri_match(texts[[x]], regex = c(symbol, operator)[[i]][1])))
            for(j in text_symbol) {
                loaded_file[x] <- gsub(pattern = texts[[x]][j], 
                                       replacement = paste('qUotEd_sTRinG', j, sep = ' '), 
                                       x = loaded_file[x], fixed = TRUE)
            }
        }
        #})
        
        loaded_file <- gsub(pattern = pad_objects(c(symbol, operator)[[i]]), 
                            replacement = c(symbol, operator)[[i]][1], 
                            x = loaded_file)
        # loaded_file <- gsub(pattern = pad_operator(operator[[i]]), 
        #                     replacement = paste0(operator[[i]], collapse = ''),  
        #                     x = loaded_file)
        
        if(operator_spaces) {
            loaded_file <- gsub(pattern = c(symbol, operator)[[i]][1], 
                                replacement = pad_objects(c(symbol, operator)[[i]]), 
                                x = loaded_file)
            # loaded_file <- gsub(pattern = paste0(operator[[i]], collapse = ''), 
            #                     replacement = pad_operator(operator[[i]]), 
            #                     x = loaded_file)
        }
        
        for (x in symbol_presence) {
            text_symbol <- which(!is.na(stri_match(texts[[x]], regex = c(symbol, operator)[[i]][1])))
            for (j in text_symbol) {
                loaded_file[x] <- gsub(pattern = paste('qUotEd_sTRinG', j, sep = ' '), 
                                       replacement = texts[[x]][j], 
                                       x = loaded_file[x], fixed = TRUE)
            }
        }
    }
    loaded_file <- gsub(pattern = '< \\-', replacement = '<-', x = loaded_file)
    loaded_file <- gsub(pattern = '=  =', replacement = '==', x = loaded_file)
    loaded_file <- gsub(pattern = '\\& \\&', replacement = '\\&\\&', x = loaded_file)
    loaded_file <- gsub(pattern = '\\| \\|', replacement = '\\|\\|', x = loaded_file)
    loaded_file <- gsub(pattern = '\\*  \\*', replacement = '\\*\\*', x = loaded_file)
    loaded_file <- gsub(pattern = '\\! \\=', replacement = '\\!\\=', x = loaded_file)
    loaded_file <- gsub(pattern = '> =', replacement = '>=', x = loaded_file)
    loaded_file <- gsub(pattern = '< =', replacement = '<=', x = loaded_file)
    
    # return(loaded_file)
    new_code <- loaded_file %>% 
        strsplit(., split = '\n') %>% 
        lapply(., function(x) {
            if(length(x) == 0) {
                return('')
            } else {
                return(x)
            }
        }) %>% 
        unlist()
    
    return(new_code)
}


code_reflow <- function(long_line) {
    first_letter <- which(!(strsplit(long_line, '') %>% unlist()) %in% c(' ', '#')) %>% head(1)
    line_pad <- substr(long_line, 1, first_letter - 1)
    line_text <- substr(long_line, first_letter, nchar(long_line))
    text_break <- line_width - first_letter
    
    reflowed_line_by_comma <- reflow(text = gsub(',', ',\\@\\@', line_text), 
                                     text_pad = line_pad, 
                                     text_break = text_break, 
                                     text_split_spaced = '\\@\\@') %>% 
        gsub('\\@\\@', '', .)
    
    if (reflowed_line_by_comma != long_line) {
        line_text <- reflowed_line_by_comma %>% 
            substr(., first_letter, nchar(.)) %>% 
            gsub(pattern = '\n',
                 replacement = '\\@\\@\n',
                 x = .)
    }
    nbr_open <- which(!is.na(str_match(string = strsplit(line_text, '') %>% 
                                           unlist(), pattern = '\\(')))
    nbr_close <- which(!is.na(str_match(string = strsplit(line_text, '') %>% 
                                            unlist(), pattern = '\\)')))
    
    
    if (length(nbr_open) > 0 | length(nbr_close) > 0) {
        if (length(nbr_open) >= length(nbr_close)) {
            diff <- length(nbr_open) - length(nbr_close)
            edited_line_text <- strsplit(line_text, '') %>% 
                unlist()
            if (diff == 0) {
                diff <- length(nbr_open)
            }
            edited_line_text[nbr_open[1:diff]] <- '(@@'
            edited_line_text <- paste0(edited_line_text, collapse = '')
            
            reflowed_line_by_par <- reflow(text = edited_line_text, 
                                           text_pad = line_pad, 
                                           text_break = text_break, 
                                           text_split_spaced = '\\@\\@') %>% 
                gsub(pattern = '\\@\\@', 
                     replacement = '', 
                     x = .)
            
        } else {
            reflowed_line_by_par <- reflowed_line_by_comma
        }
    } else {
        reflowed_line_by_par <- reflowed_line_by_comma
    } 
    
    # if (reflowed_line_by_par == long_line) {
    #     operators <- c('=', '\\+', '\\-', '\\*', '\\/', '\\& ', '\\| ')
    #     for (i in operators) {
    #         line_text <- gsub(pattern = i, replacement = paste0(i, '\\@\\@'), x = line_text)
    #     }
    #     reflowed_line_by_par <- reflow(text = line_text, 
    #                                    text_pad = line_pad, 
    #                                    text_break = text_break, 
    #                                    text_split_spaced = '\\@\\@') %>% 
    #         gsub(pattern = '\\@\\@', replacement = '', x = .) 
    # }
    
    reflowed_code <- reflowed_line_by_par %>% 
        gsub(pattern = 'cOMmA_bReAk', 
             replacement = '\n', x = .)
    
    return(reflowed_code)
}

full_code_reflow <- function(loaded_file) {
    comment_ind <- whichComments(loaded_file)
    code <- loaded_file[-comment_ind]
    long_code <- code[nchar(code) > 80]
    inline_comments <- which(!is.na(str_match(string = long_code, pattern = '#')))
    inline_comment_text <- sapply(inline_comments, function(x) {
        first_letter <- which(long_code[x] %>% strsplit(., split = '') %>% unlist() == '#')[1]
        return(substr(long_code[x], first_letter, nchar(long_code[x])))
    }) %>% set_names(., inline_comments)
    loaded_file[which(loaded_file %in% long_code)[inline_comments]] <- paste(
        inline_comment_text, 
        mask_inline_comments(loaded_file[which(loaded_file %in% long_code)][inline_comments]),
        sep = '\n')
    masked_code <- loaded_file %>% 
        strsplit(., split = '\n') %>% 
        lapply(., function(x) {
            if(length(x) == 0) {
                return('')
            } else {
                return(x)
            }
        }) %>% 
        unlist()
    
    comment_ind <- whichComments(masked_code)
    masked_long_code <- masked_code[-comment_ind]
    masked_long_code <- masked_long_code[nchar(masked_long_code) > 80]
    
    if (length(masked_long_code) == 0) {
        return(loaded_file)
    } else {
        new_code <- sapply(masked_long_code, function(x) code_reflow(x))
        
        
        masked_code[which(masked_code %in% masked_long_code)] <- new_code
        
        edited_code <- masked_code %>% 
            strsplit(., split = '\n') %>% 
            lapply(., function(x) {
                if(length(x) == 0) {
                    return('')
                } else {
                    return(x)
                }
            }) %>% 
            unlist()
        return(edited_code)
    }
}


