boxplot_with_outliers <- function(dt, column, group = NULL, by = NULL, uid = "uid", outlier_font_size = 4,
                                  save = FALSE, filename = NULL, width = 18, height = 12, dpi = 300,
                                  use_prism = FALSE) {
        # Ensure dt is a data.table
        setDT(dt)
        
        # Calculate outliers
        calculate_outliers <- function(x) {
                q1 <- quantile(x, 0.25, na.rm = TRUE)
                q3 <- quantile(x, 0.75, na.rm = TRUE)
                iqr <- q3 - q1
                lower_bound <- q1 - 1.5 * iqr
                upper_bound <- q3 + 1.5 * iqr
                x < lower_bound | x > upper_bound
        }
        
        # Prepare grouping variables
        group_vars <- c(group, by)
        group_vars <- group_vars[!is.null(group_vars)]
        
        # Calculate outliers
        dt[, outlier := calculate_outliers(get(column)), by = group_vars]
        
        # Get outliers with their actual uid
        outliers <- dt[outlier == TRUE, 
                       lapply(.SD, function(x) x),
                       by = group_vars, 
                       .SDcols = c(uid, column)]
        
        # Create the plot
        if (!is.null(group)) {
                p <- ggplot(dt, aes(x = .data[[group]], y = .data[[column]])) +
                        geom_boxplot(outlier.shape = NA) +  # Hide default outliers
                        geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
                        labs(title = paste("Boxplot of", column, "by", group),
                             x = group,
                             y = column)
        } else {
                # Add a dummy x variable when there's no group
                dt[, dummy_x := ""]
                p <- ggplot(dt, aes(x = dummy_x, y = .data[[column]])) +
                        geom_boxplot(outlier.shape = NA) +  # Hide default outliers
                        geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
                        labs(title = paste("Boxplot of", column),
                             y = column) +
                        theme(axis.title.x = element_blank(),
                              axis.text.x = element_blank(),
                              axis.ticks.x = element_blank())
        }
        
        # Apply theme
        if (use_prism) {
                p <- p + theme_prism() +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1))
        } else {
                p <- p + theme_minimal() +
                        theme(plot.title = element_text(size = 16, face = "bold"),
                              axis.title = element_text(size = 14, face = "bold"),
                              axis.text = element_text(size = 12),
                              legend.title = element_text(size = 12),
                              legend.text = element_text(size = 10),
                              strip.text = element_text(size = 12, face = "bold"))
        }
        
        # Add outlier labels with increased font size
        if (nrow(outliers) > 0) {
                if (!is.null(group)) {
                        p <- p + geom_text(data = outliers, 
                                           aes(x = .data[[group]], y = .data[[column]], label = .data[[uid]]), 
                                           hjust = -0.3, 
                                           vjust = 0.5, 
                                           size = outlier_font_size,
                                           color = "red",
                                           fontface = "bold")
                } else {
                        p <- p + geom_text(data = outliers, 
                                           aes(x = "1", y = .data[[column]], label = .data[[uid]]), 
                                           hjust = -0.3, 
                                           vjust = 0.5, 
                                           size = outlier_font_size,
                                           color = "red",
                                           fontface = "bold")
                }
        }
        
        # Add faceting if 'by' is provided
        if (!is.null(by)) {
                p <- p + facet_wrap(vars(.data[[by]]), scales = "free_y", ncol = 2)
        }
        
        # Save the plot if requested
        if (save) {
                if (is.null(filename)) {
                        filename <- paste0("boxplot_", column, ifelse(!is.null(group), paste0("_by_", group), ""), ".png")
                }
                ggsave(filename, p, width = width, height = height, dpi = dpi)
                cat("Plot saved as", filename, "\n")
        }
        
        return(p)
}