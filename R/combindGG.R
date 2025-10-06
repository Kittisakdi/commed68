library(ggplot2)
library(ggpubr)
library(grid)
library(systemfonts)

combindGG <- function(plot_list, ncol, nrow, title, save_path = NULL, width = 15, height = 10, dpi = 300, scale = 1, base_family = "TH Sarabun New", title_size = 16, labels = "AUTO", label_size = 10) {
        # ตรวจสอบว่าฟอนต์ที่ต้องการมีอยู่หรือไม่
        available_fonts <- system_fonts()
        if (!(base_family %in% available_fonts$family)) {
                warning(paste("Font family '", base_family, "' not found. Using default font instead.", sep=""))
                base_family <- ""  # ใช้ฟอนต์เริ่มต้นของระบบ
        }
        
        # สร้าง arranged plot
        arr_plot <- ggarrange(plotlist = plot_list,
                              ncol = ncol, nrow = nrow,
                              labels = labels,  # ใช้ค่า labels ที่รับเข้ามา
                              common.legend = TRUE,
                              legend = "bottom",
                              align = "hv",
                              font.label = list(size = label_size * scale, color = "black", face = "bold", family = base_family))
        
        # เพิ่ม annotation และ title
        final_plot <- annotate_figure(arr_plot,
                                      top = text_grob(title, 
                                                      color = "red", 
                                                      face = "bold", 
                                                      size = title_size,
                                                      family = base_family,
                                                      vjust = 1))
        
        # แสดงผลลัพธ์
        print(final_plot)
        
        # บันทึกไฟล์ถ้ามีการระบุ path
        if (!is.null(save_path)) {
                # แยก path และ filename
                path_parts <- strsplit(save_path, "/")[[1]]
                dir_path <- paste(path_parts[-length(path_parts)], collapse = "/")
                file_name <- path_parts[length(path_parts)]
                
                # สร้างโฟลเดอร์ถ้ายังไม่มี
                if (dir_path != "" && !dir.exists(dir_path)) {
                        dir.create(dir_path, recursive = TRUE)
                        cat("Created directory:", dir_path, "\n")
                }
                
                tryCatch({
                        ggsave(filename = save_path, plot = final_plot, width = width, height = height, dpi = dpi, bg = "white", limitsize = FALSE)
                        cat("Plot saved successfully to:", save_path, "\n")
                }, error = function(e) {
                        cat("Error saving plot:", conditionMessage(e), "\n")
                })
        }
        
        # ส่งคืนค่า plot object
        return(final_plot)
}