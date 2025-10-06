## F1: table1 with p=value ----
# Create an environment to hold the test counter and mapping
auto_number_env <- new.env()
auto_number_env$test_counter <- 1
auto_number_env$test_mapping <- c()
auto_number_env$footnotes <- list()
#auto_number_env$test_list <- list()

# Function to add a test to the footnotes with auto-numbering
#add_footnote <- function(test_name) {
#        existing_numbers <- as.numeric(gsub("^(\\d+):.*", "\\1", names(auto_number_env$footnotes)))
#        next_number <- if (length(existing_numbers) > 0) max(existing_numbers, na.rm = TRUE) + 1 else 1
#        if (!test_name %in% names(auto_number_env$footnotes)) {
#                auto_number_env$footnotes[[test_name]] <- paste0(next_number, ": ", test_name)
#        }
#}

# Function to get the footnote text
get_footnote_text <- function() {
        paste((paste0(1:length(names(auto_number_env$test_mapping)),":",names(auto_number_env$test_mapping))), collapse = ", ")
}

reset_footnotes <- function() {
        auto_number_env$footnotes <<- list()
}


# Function to format the p-value with asterisks and subscript for test number
format_p_value <- function(p, test_name) {
        asterisks <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
        subscript <- get_test_subscript(test_name)
        #p_formatted <- format.pval(p, digits = 3, eps = 0.001)
        p_formatted <- round(p, digits = 3)
        p_formatted <- ifelse(p < 0.001, "<0.001", ifelse(p < 0.01, "<0.01", ifelse(p < 0.05, p_formatted, ifelse(p >= 0.05, p_formatted, ""))))
        return(paste0(p_formatted, asterisks, "<sub>", subscript, "</sub>"))
}

# Function to retrieve the subscript number for a given test
get_test_subscript <- function(test_name) {
        if (test_name %in% names(auto_number_env$test_mapping)) {
                return(auto_number_env$test_mapping[[test_name]])
        } else {
                current_subscript <- auto_number_env$test_counter
                auto_number_env$test_mapping[[test_name]] <- current_subscript
                #add_footnote(test_name)
                auto_number_env$test_counter <- auto_number_env$test_counter + 1
                return(current_subscript)
        }
}


reset_counter <- function() {
        auto_number_env$test_counter <<- 1
        auto_number_env$test_mapping <<- c()
        auto_number_env$test_list <<- list()
}

pvalue <- function(x, ...) {
        tryCatch({
                # Access the global environment for test counter, mapping, and list
                test_mapping <- get("test_mapping", envir = auto_number_env)
                n_obs <- length(x)
                
                # Remove "overall" group
                x <- x[names(x) != "Overall"]
                # Construct vectors of data y, and groups (strata) g
                y <- unlist(x)
                g <- factor(rep(names(x), times = sapply(x, length)))
                n_obs <- length(y)  # Number of observations
                
                # If y is not numeric, perform categorical test
                if (!is.numeric(y)) {
                        # Create a contingency table
                        cont_table <- table(y, g)
                        
                        # Check conditions for Fisher's exact test
                        expected <- chisq.test(cont_table)$expected
                        if (any(expected < 5) || n_obs < 30) {
                                # Use Fisher's exact test for small samples or low expected frequencies
                                test_result <- fisher.test(cont_table, simulate.p.value = TRUE)
                                p <- test_result$p.value
                                test_name <- "Fisher's exact test"
                        } else {
                                # Use chi-squared test for larger samples
                                test_result <- chisq.test(cont_table)
                                p <- test_result$p.value
                                test_name <- "Chi-squared test"
                        }
                } else {
                        # Check for normality within each group
                        normality_p_values <- sapply(x, function(group) {
                                if (is.numeric(group) && length(unique(group)) > 1) {
                                        tryCatch({
                                                shapiro.test(group)$p.value
                                        }, error = function(e) {
                                                NA  # Return NA if Shapiro test fails
                                        })
                                } else {
                                        NA  # Return NA for non-numeric groups or groups with all identical values
                                }
                        })
                        
                        # Determine if data is normally distributed in all groups
                        all_normal <- all(normality_p_values >= 0.05, na.rm = TRUE)
                        
                        if (length(unique(g)) == 2) {
                                # If there are exactly two groups
                                if (all_normal) {
                                        # Use t-test if data is normally distributed
                                        p <- t.test(y ~ g)$p.value
                                        test_name <- "t-test"
                                } else {
                                        # Use Wilcoxon test if data is not normally distributed
                                        p <- wilcox.test(y ~ g)$p.value
                                        test_name <- "Wilcoxon test"
                                }
                        } else {
                                # If there are more than two groups
                                if (all_normal) {
                                        # Use ANOVA if data is normally distributed
                                        p <- anova(lm(y ~ g))$'Pr(>F)'[1]
                                        test_name <- "ANOVA"
                                } else {
                                        # Use Kruskal-Wallis test if data is not normally distributed
                                        p <- kruskal.test(y ~ g)$p.value
                                        test_name <- "Kruskal-Wallis test"
                                }
                        }
                }
                
                formatted_p_value <- format_p_value(p, test_name)
                return(formatted_p_value)
        }, error = function(e) {
                # If any error occurs, return NA
                return(NA)
        })
}



# F2_render cc]ont snf prop ----
rndr.strat <- function(label, n, ...) {
        ifelse(n==0, label, render.strat.default(label, n, ...))
}

render.contCK <- function(x, dig=2) {
        c('', 
          `Mean (SD)` = sprintf("%s (%s)", format(round(mean(x, na.rm=TRUE), dig), nsmall = dig) ,
                                format(round(sd(x, na.rm=TRUE),dig), nsmall = dig)),
          `Median [IQR]` = sprintf("%s [%s, %s]",  format(round(median(x, na.rm=TRUE),dig), nsmall = dig), 
                                   format(round(quantile(x, 0.25, na.rm=TRUE),dig), nsmall = dig), 
                                   format(round(quantile(x, 0.75, na.rm=TRUE),dig), nsmall = dig)),
          `Min, Max` = sprintf("%s, %s", format(round(min(x, na.rm=TRUE),dig), nsmall = dig), 
                               format(round(max(x, na.rm=TRUE),dig), nsmall = dig))
        )
}


render.propAC  <- function(x) { 
        sub('.', '.', c("", sapply(stats.default(x), 
                                   function(y) with(y, sprintf("%d (%0.2f %%)", FREQ, PCT)))), fixed = TRUE) 
}

#get_test_list <- function() {
#        test_list <- get("test_list", envir = auto_number_env)
#       return(test_list)
#}

#showfTAB <- function(model){
#        print(model)
#        get_test_list()
#}


create_var_label_list <- function(data, variables) {
        var_label_list <- list()
        for (var_name in names(variables)) {
                if (is.character(variables[[var_name]])) {
                        # ถ้าเป็น string ให้ใช้เป็น label
                        var_label_list[[var_name]] <- variables[[var_name]]
                } else if (is.null(variables[[var_name]])) {
                        # ถ้าเป็น NULL ให้ใช้ชื่อตัวแปรเป็น label
                        var_label_list[[var_name]] <- var_name
                } else {
                        # กรณีอื่นๆ ให้ใช้ชื่อตัวแปรเป็น label
                        var_label_list[[var_name]] <- var_name
                }
        }
        return(var_label_list)
}


if (!require(table1)) install.packages("table1")
library(table1)


if (!require(table1)) install.packages("table1")
library(table1)


newtab1S <- function(dataset, split_group = NULL, split_lab = NULL, variables = NULL, tcaption = NULL) {
        # Ensure dataset is a data.frame
        dataset <- as.data.frame(dataset)
        
        # If variables is NULL, create it automatically from the dataset columns
        if (is.null(variables)) {
                variables <- setNames(as.list(names(dataset)), names(dataset))
        } else {
                # Ensure that all specified variables exist in the dataset
                missing_vars <- setdiff(names(variables), names(dataset))
                if (length(missing_vars) > 0) {
                        stop(paste("The following variables are not found in the dataset:", 
                                   paste(missing_vars, collapse = ", ")))
                }
        }
        
        # If tcaption is NULL, create a default caption
        if (is.null(tcaption)) {
                tcaption <- paste0('Table 1: Descriptive Statistics (N=', nrow(dataset), ")")
        }
        
        # สร้าง data frame ที่มีการจัดการ label ที่เหมาะสม
        processed_dataset <- dataset
        
        # Create var_label_list แบบปรับปรุง - ลบ # ออกจาก label
        var_label_list <- list()
        for (var_name in names(variables)) {
                # ตรวจสอบว่ามี label ใน attribute ของตัวแปรหรือไม่
                attr_label <- attr(dataset[[var_name]], "label")
                
                if (!is.null(attr_label)) {
                        # ถ้ามี label ใน attribute ให้ลบเครื่องหมาย # ออกก่อนใช้
                        clean_label <- gsub("#", "", attr_label)
                        var_label_list[[var_name]] <- clean_label
                        # กำหนด label ที่ได้รับการแก้ไขแล้วให้กับตัวแปรในชุดข้อมูลที่ประมวลผล
                        attr(processed_dataset[[var_name]], "label") <- clean_label
                } else if (is.character(variables[[var_name]]) && !is.null(variables[[var_name]])) {
                        # ถ้าเป็น string ให้ใช้เป็น label
                        var_label_list[[var_name]] <- variables[[var_name]]
                } else {
                        # กรณีอื่นๆ ให้ใช้ชื่อตัวแปรเป็น label
                        var_label_list[[var_name]] <- var_name
                }
        }
        
        # Check if split_group is provided and exists in the dataset
        if (!is.null(split_group) && split_group %in% names(dataset)) {
                # Case with split
                labels <- list(
                        variables = var_label_list,
                        groups = list("", split_lab, "")
                )
                
                split_data <- split(processed_dataset, processed_dataset[[split_group]])
                number_of_groups <- length(unique(processed_dataset[[split_group]]))
                
                table1(c(list(Overall = processed_dataset), split_data), 
                       labels = labels,
                       groupspan = c(1, number_of_groups, 1),
                       extra.col = list(`P-value` = pvalue),
                       extra.col.pos = number_of_groups + 2,
                       render.continuous = render.contCK,
                       render.categorical = render.propAC,
                       caption = tcaption,
                       footnote = get_footnote_text()
                )
        } else {
                # Case without split (descriptive only)
                formula <- as.formula(paste("~", paste(names(var_label_list), collapse = " + ")))
                
                # กำหนด label ให้กับตัวแปรก่อนส่งไปยัง table1
                for (var in names(var_label_list)) {
                        label(processed_dataset[[var]]) <- var_label_list[[var]]
                }
                
                table1(formula,
                       data = processed_dataset,
                       render.continuous = render.contCK,
                       render.categorical = render.propAC,
                       caption = tcaption)
        }
}