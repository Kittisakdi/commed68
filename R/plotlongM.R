plotlongM <- function(dataset, x_var, time_var, group_var, ylim = NULL, 
                      graph_title = NULL, xlab = NULL, ylab = NULL,
                      color_palette = NULL,
                      graph = TRUE, table = TRUE,
                      save_plot = FALSE, file_name = "plot.png", 
                      width = 10, height = 6, dpi = 300,
                      ci = "95", base_family = NULL,
                      title_size = 12, line_size = 1.2,
                      axis_line_size = 1.0, 
                      axis_text_size = 10,
                      axis_title_size = 12,
                      point_size = 3.5,
                      legend_title_size = 10,    # ขนาดชื่อ legend
                      legend_text_size = 9,      # ขนาดข้อความใน legend
                      legend_name = NULL) {      # เพิ่มพารามิเตอร์สำหรับชื่อ legend
        # Ensure dataset is a data.table
        setDT(dataset)
        if (is.null(color_palette)) {
                n_groups <- uniqueN(dataset[[group_var]])
                color_palette <- scales::hue_pal()(n_groups)
        }
        
        # ตั้งค่าชื่อ legend ถ้าไม่ได้ระบุ
        if (is.null(legend_name)) {
                legend_name <- group_var
        }
        
        # Create default labels if not provided
        if (is.null(graph_title)) {
                graph_title <- paste("Mean", if(ci != "none") "95% CI of" else "of", x_var, "by", time_var, "and", group_var)
        }
        if (is.null(xlab)) {
                xlab <- time_var
        }
        if (is.null(ylab)) {
                ylab <- x_var
        }
        
        # Prepare data
        plot_data <- dataset[, .(
                mean = mean(get(x_var), na.rm = TRUE),
                sd = sd(get(x_var), na.rm = TRUE),
                n = .N
        ), by = c(time_var, group_var)]
        
        # Calculate CI only if ci != "none"
        if (ci != "none") {
                plot_data[, `:=`(
                        se = sd / sqrt(n),
                        ci_lower = mean - qt(0.975, n-1) * sd / sqrt(n),
                        ci_upper = mean + qt(0.975, n-1) * sd / sqrt(n)
                )]
        }
        
        # Create the plot if graph = TRUE
        if (graph) {
                p <- ggplot(plot_data, aes(x = get(time_var), y = mean, color = get(group_var), group = get(group_var))) +
                        geom_line(size = line_size) +
                        geom_point(size = point_size) +
                        scale_color_manual(values = color_palette) +
                        labs(title = graph_title, x = xlab, y = ylab, color = legend_name) + # ใช้ legend_name แทน group_var
                        theme_prism(base_family = base_family) +
                        theme(
                                plot.title = element_text(hjust = 0.5, face = "bold", size = title_size),
                                legend.position = "bottom",
                                panel.grid.minor = element_blank(),
                                # เพิ่มความหนาของเส้นแกน X และ Y
                                axis.line = element_line(size = axis_line_size),
                                axis.ticks = element_line(size = axis_line_size),
                                # ปรับขนาด font ของแกน
                                axis.text = element_text(size = axis_text_size),
                                axis.title = element_text(size = axis_title_size, face = "bold"),
                                # ปรับขนาด legend
                                legend.title = element_text(size = legend_title_size, face = "bold"),
                                legend.text = element_text(size = legend_text_size),
                                legend.key.size = unit(1.2, "lines")  # ขนาดของ key ใน legend
                        ) +
                        scale_x_continuous(breaks = unique(plot_data[[time_var]]))
                
                # Add error bars only if ci != "none"
                if (ci != "none") {
                        p <- p + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1)
                }
                
                # Apply ylim based on the input
                if (!is.null(ylim)) {
                        p <- p + coord_cartesian(ylim = ylim)
                } else if (ci != "none") {
                        p <- p + coord_cartesian(ylim = c(min(plot_data$ci_lower) * 0.9, max(plot_data$ci_upper) * 1.1))
                } else {
                        p <- p + coord_cartesian(ylim = c(min(plot_data$mean) * 0.9, max(plot_data$mean) * 1.1))
                }
                
                print(p)
                
                # Save the plot if save_plot = TRUE
                if (save_plot) {
                        path_parts <- strsplit(file_name, "/")[[1]]
                        dir_path <- paste(path_parts[-length(path_parts)], collapse = "/")
                        
                        if (dir_path != "" && !dir.exists(dir_path)) {
                                dir.create(dir_path, recursive = TRUE)
                                cat("Created directory:", dir_path, "\n")
                        }
                        
                        ggsave(filename = file_name, plot = p, width = width, height = height, dpi = dpi)
                        cat("Plot saved as", file_name, "\n")
                }
        }
        
        # Prepare and show the result table if table = TRUE
        if (table) {
                result_table <- plot_data[order(get(time_var), get(group_var))]
                if (ci == "none") {
                        setnames(result_table, c(time_var, group_var, "mean", "sd", "n"))
                } else {
                        setnames(result_table, c(time_var, group_var, "mean", "sd", "n", "se", "ci_lower", "ci_upper"))
                }
                print(result_table)
        }
        
        # Return both the plot and the table invisibly
        invisible(list(plot = if(graph) p else NULL, table = if(table) result_table else NULL))
}