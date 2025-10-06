library(data.table)
library(ggplot2)
library(ggpubr)
library(rempsyc)
library(ggprism)

NDplot <- function(data, continuous_var, nominal_var=NULL, export=FALSE, file='ND_test.pdf'){
        if(!is.data.table(data)) {
                setDT(data)
        }
        
        data <- na.omit(data, cols = c(continuous_var, nominal_var))
        
        if(nrow(data) == 0) {
                stop("After removing NA values, no data remains. Please check your input data.")
        }
        
        if(is.null(nominal_var)){
                p1 <- ggplot(data, aes(x = .data[[continuous_var]])) + 
                        geom_histogram(aes(y = after_stat(density)), colour = "black", fill = "white", 
                                       bins = round(nrow(data)^(1/3) * 2)) +
                        geom_density(alpha = .2, fill = "#FF6666") + 
                        ggtitle('Histogram with ND curve') + 
                        theme_prism()
                
                stats <- boxplot.stats(data[[continuous_var]])$stats
                
                p2 <- ggplot(data, aes(x = .data[[continuous_var]])) + 
                        geom_boxplot(width=0.1) + coord_flip() + 
                        annotate(geom="text", 
                                 x=round(stats,2), y=0.06, 
                                 label=round(stats,1),
                                 color="red") + ggtitle('Boxplot') + theme_prism() +
                        theme(axis.text.x = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.title.y = element_blank())
                
                shapiro_test <- shapiro.test(data[[continuous_var]])
                pSHO <- paste('Shapiro-Wilk Test\nP-value =', format.pval(shapiro_test$p, digits = 3))
                
                p3 <- ggqqplot(data[[continuous_var]]) +  
                        annotate(geom="text", 
                                 x=-Inf, y=Inf, 
                                 label= pSHO, 
                                 color="black",
                                 hjust = -0.1,
                                 vjust = 1.1) + 
                        ggtitle('QQ plot') + 
                        theme_prism()
        } else {
                data[[nominal_var]] <- as.factor(data[[nominal_var]])
                
                p1 <- ggplot(data, aes(x = .data[[continuous_var]])) + 
                        geom_histogram(aes(y = after_stat(density)), colour = "black", fill = "white", 
                                       bins = round(nrow(data)^(1/3) * 2)) +
                        geom_density(alpha = .2, fill = "#FF6666") +
                        facet_wrap(as.formula(paste0("~", nominal_var))) +
                        ggtitle('Histogram with ND curve') +
                        theme_prism() +
                        theme(
                                strip.text = element_text(size = 9),
                                plot.title = element_text(size = 10),
                                axis.title = element_text(size = 8),
                                axis.text = element_text(size = 7)
                        )
                
                stats <- data[, .(
                        Median = median(get(continuous_var)),
                        Q1 = quantile(get(continuous_var), 0.25),
                        Q3 = quantile(get(continuous_var), 0.75),
                        L1 = quantile(get(continuous_var), 0.75) + 1.5*IQR(get(continuous_var)),
                        L2 = quantile(get(continuous_var), 0.75) + 3*IQR(get(continuous_var)),
                        L3 = quantile(get(continuous_var), 0.25) - 1.5*IQR(get(continuous_var)),
                        L4 = quantile(get(continuous_var), 0.25) - 3*IQR(get(continuous_var))
                ), by = nominal_var]
                
                p2 <- ggplot(data, aes(x = .data[[nominal_var]], y = .data[[continuous_var]])) +
                        geom_boxplot() +
                        geom_text(data = stats, aes(x = .data[[nominal_var]], y = Median, label = round(Median,2)),
                                  vjust = -0.8, size = 2.5) +
                        geom_text(data = stats, aes(x = .data[[nominal_var]], y = Q1, label = round(Q1,2)),
                                  vjust = -0.8, size = 2.5)  +
                        geom_text(data = stats, aes(x = .data[[nominal_var]], y = Q3, label = round(Q3,2)),
                                  vjust = -0.8, size = 2.5) +
                        geom_text(data = stats, aes(x = .data[[nominal_var]], y = L1, label = round(L1,2)),
                                  vjust = -0.8, size = 2.5) +
                        geom_text(data = stats, aes(x = .data[[nominal_var]], y = L2, label = round(L2,2)),
                                  vjust = -0.8, size = 2.5) +
                        geom_text(data = stats, aes(x = .data[[nominal_var]], y = L3, label = round(L3,2)),
                                  vjust = -0.8, size = 2.5) +
                        geom_text(data = stats, aes(x = .data[[nominal_var]], y = L4, label = round(L4,2)),
                                  vjust = -0.8, size = 2.5) +
                        xlab(nominal_var) +
                        ylab(continuous_var) +
                        ggtitle('Boxplot') +
                        theme_prism() +
                        theme(
                                plot.title = element_text(size = 10),
                                axis.title = element_text(size = 8),
                                axis.text = element_text(size = 7)
                        ) 
                
                # QQ plot
                qq_data_list <- list()
                shapiro_results <- list()
                
                for(group in levels(data[[nominal_var]])) {
                        group_data <- data[get(nominal_var) == group, get(continuous_var)]
                        n <- length(group_data)
                        
                        # สร้าง theoretical quantiles
                        theoretical <- qnorm((1:n - 0.5) / n)
                        sample_q <- sort(group_data)
                        
                        # Shapiro test
                        shapiro_p <- format.pval(shapiro.test(group_data)$p, digits = 3)
                        
                        qq_data_list[[group]] <- data.table(
                                group = group,
                                theoretical = theoretical,
                                sample = sample_q
                        )
                        
                        shapiro_results[[group]] <- data.table(
                                group = group,
                                shapiro_p = shapiro_p
                        )
                }
                
                qq_combined <- rbindlist(qq_data_list)
                shapiro_combined <- rbindlist(shapiro_results)
                
                p3 <- ggplot(qq_combined, aes(x = theoretical, y = sample)) +
                        geom_point(size = 0.8, alpha = 0.7) + 
                        geom_qq_line(aes(sample = sample), alpha = 0.7) +
                        facet_wrap(~ group, scales = "free") +
                        geom_text(data = shapiro_combined,
                                  aes(x = -Inf, y = Inf, label = paste("p =", shapiro_p)),
                                  hjust = -0.05, vjust = 2.0, size = 3.5,
                                  inherit.aes = FALSE) +
                        labs(x = "Theoretical Quantiles", 
                             y = "Sample Quantiles",
                             title = "QQ plot") +
                        theme_prism() +  
                        theme(
                                strip.text = element_text(size = 10, margin = margin(3,3,3,3)),
                                plot.title = element_text(size = 10),
                                axis.title = element_text(size = 9),
                                axis.text = element_text(size = 8),
                                plot.margin = margin(10, 15, 10, 10),
                                panel.spacing = unit(0.3, "cm")
                        )
        }
        
        # Layout  
        if(is.null(nominal_var)) {
                asdf <- ggarrange(p1, p2, p3, labels = c("A", "B", "C"))
        } else {
                #  
                asdf <- ggarrange(p1, p2, p3, labels = c("A", "B", "C"), 
                                  ncol = 2, nrow = 2, 
                                  heights = c(1, 1))
        }
        
        # Update main title format
        if(is.null(nominal_var)) {
                main_title <- paste0("Normality test for '", continuous_var, "'")
        } else {
                main_title <- paste0("Normality test for '", continuous_var, "' by '", nominal_var, "'")
        }
        
        Tasdf <- annotate_figure(asdf, top = text_grob(main_title, 
                                                       color = "red", face = "bold", size = 14))
        print(Tasdf)
        cat('\n Output was shown \n \n')
        
        if(export){
                if(is.null(nominal_var)) {
                        ggsave(file, Tasdf, width = 297, height = 210, units = "mm", dpi = 300)  # landscape
                } else {
                        ggsave(file, Tasdf, width = 210, height = 297, units = "mm", dpi = 300)  # portrait
                }
                cat(file, ' @ ', getwd())
        }
}