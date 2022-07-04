
library(ggbeeswarm)

niebieskie <- c('#C6DBEF', '#ABCFE5', '#8DC0DD', '#6BAED6', '#4F9BCB', '#3787C0', '#2171B5', '#105BA4', '#08468B', '#08306B')


int_dot_plot_all_data <- RML_data_26_May_ENTREZ_NAs_filtered %>% dplyr::filter(Cell_Type != "S1", !Gene_Symbol %like% "mt-") %>% 
    
    dplyr::transmute(Ensembl_ID = Ensembl_ID,
                     Gene_Symbol = Gene_Symbol,
                     Cell_Type = Cell_Type,
                     Timepoint = Timepoint,
                     L2FC = signif(L2FC, 3),
                     L2FC_SE = signif(L2FC_SE, 3),
                     FDR = FDR,
                     Gene_Rank = signif(Gene_Rank, 3),
                     Biotype = Biotype,
                     p_cat = p_cat)

int_dot_plot_data <- RML_data_26_May_ENTREZ_NAs_filtered %>% dplyr::filter(FDR <= 0.1, Cell_Type != "S1", L2FC > -5, !Gene_Symbol %like% "mt-") %>% 
    
    dplyr::transmute(Ensembl_ID = Ensembl_ID,
                     Gene_Symbol = Gene_Symbol,
                     Cell_Type = Cell_Type,
                     Timepoint = Timepoint,
                     L2FC = signif(L2FC, 3),
                     L2FC_SE = signif(L2FC_SE, 3),
                     FDR = FDR,
                     Gene_Rank = signif(Gene_Rank, 3),
                     Biotype = Biotype,
                     p_cat = p_cat)
                                       
int_dot_plot_data_x_pts <- int_dot_plot_data
int_dot_plot_data_x_pts$xPoints <- as.numeric(gg$data[[1]]$x)
saveRDS(int_dot_plot_data_x_pts, "int_dot_plot_data_x_pts.rds")
saveRDS(int_dot_plot_all_data, "int_dot_plot_all_data.rds")

gg <- ggplot_build(g)

gg$data[[1]]$x

    
    g <- ggplot() +
        
  #   -------------------------------------------------------------------------------------------
    geom_quasirandom(data = int_dot_plot_data,
               aes(x = Cell_Type,
                   y = L2FC,
                   color = p_cat),
               size = 1,
               alpha = 0.5,
               inherit.aes = FALSE) +

        # geom_point(data = int_dot_plot_data_x_pts %>% dplyr::filter(Cell_Type == "Cx43"), 
        #                  aes(x = x_points, 
        #                      y = L2FC, 
        #                      color = p_cat), 
        #                  size=1, 
        #                  alpha = 0.5, 
        #                  inherit.aes = FALSE) +
        
    scale_colour_manual(values =  rev(niebieskie), drop = FALSE,
                        labels =  c(expression(paste("<",10^-8)),
                                    expression(paste(10^-8, "-", 10^-7)),
                                    expression(paste(10^-7, "-", 10^-6)),
                                    expression(paste(10^-6, "-", 10^-5)),
                                    expression(paste(10^-5, "-", 10^-4)),
                                    expression(paste(10^-4, "-", 10^-3)),
                                    expression(paste(10^-3, "-", 10^-2)),
                                    expression(paste(10^-2, "-", 10^-1)),
                                    expression(paste(10^-1, "-", 1))))  +
        
        guides(colour = guide_legend(title = bquote(atop("adjusted", "p-value")),
                                     override.aes = list(linetype = 1,
                                                         alpha = 0.7,
                                                         size = 16
                                     ))) +
        
        labs(y = bquote('Log'[2]*' Fold Change'),
             x = bquote(bold(''))) +
        scale_y_continuous(breaks = seq(-7, 7, 0.5)) +
        # scale_x_continuous("Cell Type", breaks = seq(1,6,1), labels = c("Cx43","Gad2","PV","","SST","vGluT2")) + 
        
        geom_abline(intercept=0, slope = 0, linetype="dashed",  color = "black", size=0.3) +
        theme(title = element_text(size = 18, face = "bold", lineheight = .8),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(size = 0.1),
              axis.title = element_text(size = 18),
              text = element_text(size = 12),
              strip.text.x = element_text(size = 18, face = "plain"),
              axis.text.x = element_text(size = 14, face = "plain"),
              axis.text.y = element_text(size = 14, face = "plain"),
              legend.text = element_text(angle = 0, hjust=0, size = 16),
              legend.title.align = 0.5,
              aspect.ratio = 1,
              legend.direction = 'vertical',
              legend.position = 'right',
              legend.key = element_rect(size = 4, fill = NA),
              legend.key.size = unit(0, 'lines'),
              legend.title = element_text(size = 20),
              legend.spacing = unit(0.0, 'cm')) +
        
        facet_wrap(~Timepoint, scales = "free")
    
    g