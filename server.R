################################### Scrapie Server ###################################################################################
# library(udunits2)
library(shiny)
library(shinysky)
library(data.table)
library(ggplot2)
# library(org.Mm.eg.db)
library(ggrepel)
library(DT)
library(tibble)
library(dplyr)
# library(gridExtra)
# library(grid)
# library(egg)
# library(clusterProfiler)
# library(plotly)
# library(RColorBrewer)
# library(enrichplot)
# library(matrixStats)
# library(pheatmap)


# ------ Loading Data Sets and Setting Factor levels ----------------------------------

# this function checks class and existence
extest <- function(x){
  if (exists(as.character(substitute(x)))){
    cat(as.character(substitute(x)), " ::: exists and it is of class ::: ", class(x), "\n")
  }else{
    cat(as.character(substitute(x)), " ::: DOES NOT exist! \n")
  }
}
int_dot_plot_all_data <- readRDS("int_dot_plot_all_data.rds")
int_dot_plot_data_x_pts <- readRDS("int_dot_plot_data_x_pts.rds")
# --- remove me ---------------
extest(int_dot_plot_data_x_pts)
extest(int_dot_plot_all_data)
# -----------------------------

# 
# new_cumulative_table_RML$Cell_Type <- factor(new_cumulative_table_RML$Cell_Type,
#                                              levels = c("Cx43", "vGluT2", "Gad2", "PV", "SST"))

# new_cumulative_table_S1 <- readRDS("new_cumulative_table_S1_rounded.rds")
# 
# # --- remove me ---------------
# extest(new_cumulative_table_S1)
# # -----------------
# 
# new_cumulative_table_S1$Cell_Type <- factor(new_cumulative_table_S1$Cell_Type,
#                                             levels = c("Cx43", "vGluT2", "Gad2", "PV", "SST"))

# biomart_data_of_all_genes <- readRDS("biomart_data_of_all_genes.rds")
# 
# # --- remove me ---------------
# extest(biomart_data_of_all_genes)
# # -----------------

# GO_all_sym <- readRDS("GO_all_sym.rds")
# 
# # --- remove me ---------------
# extest(GO_all_sym)
# # -----------------
# 
# all_ontology_named_list <- readRDS("all_ontology_named_list.rds")
# 
# # --- remove me ---------------
# extest(all_ontology_named_list)
# -----------------

# supersets <- readRDS("supersets.rds")


# --- remove me ---------------
extest(supersets)
# -----------------------------

melted_10w_CT <- readRDS("melted_10w_CT.rds")

melted_10w_CT$Cell_Type <- factor(melted_10w_CT$Cell_Type,
                                  levels = c("Cx43", "vGluT2", "Gad2", "PV", "SST", "S1"),
                                  labels = c("Cx43", "vGluT2", "Gad2", "PV", "SST", "Input") )
# --- remove me ---------------
extest(melted_10w_CT)
# -----------------------------
# 
# GSEA_all_res <- readRDS("GSEA_all_res_rounded.rds") 
# 
# # --- remove me ---------------
# extest(GSEA_all_res)
# -----------------------------


# --- remove me ---------------
# extest(enricher_all_res)
# -----------------------------

# enricher_all_res$Cell_Type <- factor(enricher_all_res$Cell_Type,
#                                  levels = c("Cx43", "vGluT2", "Gad2", "PV", "SST", "S1"))


#################################################################################################################

# new_cumulative_table_RML_disp <- new_cumulative_table_RML %>%
#   transmute(`Ensembl ID` = Ensembl_ID, `Symbol` = Gene_Symbol, `Cell Type` = Cell_Type,
#             `Timepoint` = Timepoint, `Log2FC` = signif(L2FC, 2), `SE`= signif(L2FC_SE, 2),
#             `FDR` = FDR, `Rank CT` = signif(Gene_Rank_Control, 4),
#             `Rank RML` = signif(Gene_Rank_RML, 4), `Biotype` = Biotype)

# --- remove me ---------------
# extest(new_cumulative_table_RML_disp)
# -----------------

myBreaks <- c(seq(min(int_dot_plot_data_x_pts$L2FC), 0, length.out=ceiling(98/2) + 1),
              seq(max(int_dot_plot_data_x_pts$L2FC)/98, max(int_dot_plot_data_x_pts$L2FC), length.out=floor(98/2)))

clrs_reds <- round(seq(255, 0, length.out = 50), 0) %>% {paste0("rgb(255,", ., ",", ., ")")}
clrs_blues <- round(seq(255, 0, length.out = 50), 0) %>% {paste0("rgb(", .,",", .,",255)")}
clrs <- append(rev(clrs_blues), clrs_reds)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# 
# cbPalette4 <- c("#E69F00","#E69F00",  "#56B4E9", "#56B4E9",  "#009E73",  "#009E73", "#F0E442", "#F0E442")
# 
# longPalette <- c("#E41A1C", "#C72A35", "#AB3A4E", "#8F4A68", "#735B81" ,"#566B9B", "#3A7BB4", "#3A85A8", "#3D8D96", "#419584", "#449D72", "#48A460",
#                  "#4CAD4E", "#56A354", "#629363" ,"#6E8371", "#7A7380", "#87638F", "#93539D", "#A25392", "#B35A77", "#C4625D","#D46A42", "#E57227",
#                  "#F67A0D", "#FF8904", "#FF9E0C" ,"#FFB314", "#FFC81D", "#FFDD25", "#FFF12D", "#F9F432" ,"#EBD930", "#DCBD2E", "#CDA12C" ,"#BF862B",
#                  "#B06A29", "#A9572E", "#B65E46", "#C3655F", "#D06C78", "#DE7390", "#EB7AA9", "#F581BE", "#E585B8" ,"#D689B1" ,"#C78DAB", "#B791A5",
#                  "#A8959F", "#999999")

niebieskie <- c('#C6DBEF', '#ABCFE5', '#8DC0DD', '#6BAED6', '#4F9BCB', '#3787C0', '#2171B5', '#105BA4', '#08468B', '#08306B')

# maxLFC <- ceiling(max(int_dot_plot_data_x_pts$L2FC))
# symbole <- as.vector(unique(int_dot_plot_data_x_pts$Gene_Symbol))

ensembl_vector_names <- as.vector(unique(melted_10w_CT$ens))
symbol_vector_names <- as.vector(unique(melted_10w_CT$symbol))

server <- function(input, output, session) {

updateSelectizeInput(session  = session,
                     inputId  = "RiboTag_Entry",
                     server   = TRUE,
                     choices  = symbol_vector_names,
                     selected = character(0),
                     options  = list(placeholder = 'eg. Gad1', onInitialize = I('function() { this.setValue(""); }')))

updateSelectizeInput(session   = session,
                      inputId  = "RiboTag_Entry_e",
                      server   = TRUE,
                      choices  = ensembl_vector_names,
                      selected = character(0),
                      options  = list(placeholder = 'eg. EMSMUS000...', onInitialize = I('function() { this.setValue(""); }')))
  
RiboTag_Entry_zmienna <- reactiveVal()

observeEvent(input$RiboTag_Entry, {
  RiboTag_Entry_zmienna(input$RiboTag_Entry)
}) # manual single gene selection (symbol) #

observeEvent(input$RiboTag_Entry_e, {
  RiboTag_Entry_zmienna(input$RiboTag_Entry_e)
}) # manual single gene selection (ensembl) #

# ------- generates one liner table used for plotting "RiboTagPlot" ---------------------
mini_tabelka_RiboTag <- reactive({
  
  if(input$search_key_mRNA == 1) {
    
    a <- melted_10w_CT %>% dplyr::filter(symbol == RiboTag_Entry_zmienna())
    }else{
      
    a <- melted_10w_CT %>% dplyr::filter(ens == RiboTag_Entry_zmienna())
    }
  # --- add rank to the single gene table for coloring
  a <- a %>% dplyr::mutate(rnk = case_when(prc_rnk <= 0.1 ~ "p10",
                                           prc_rnk <= 0.2 ~ "p20",
                                           prc_rnk <= 0.3 ~ "p30",
                                           prc_rnk <= 0.4 ~ "p40",
                                           prc_rnk <= 0.5 ~ "p50",
                                           prc_rnk <= 0.6 ~ "p60",
                                           prc_rnk <= 0.7 ~ "p70",
                                           prc_rnk <= 0.8 ~ "p80",
                                           prc_rnk <= 0.9 ~ "p90",
                                           prc_rnk <= 1 ~ "p100"))
  a$rnk <- factor(a$rnk, levels = paste0("p", seq(10,100,10)))
  
  a
})

# ------- generates one liner table used for plotting "RiboTagPlot_RML" ---------------------
mini_tabelka_RiboTag_RML <- reactive({
  
  if(input$search_key_mRNA == 1) {
  
  a <- int_dot_plot_all_data %>% dplyr::filter(Gene_Symbol == RiboTag_Entry_zmienna())
  
  }else{
    
  a <- int_dot_plot_all_data %>% dplyr::filter(Ensembl_ID == RiboTag_Entry_zmienna())

  }
 
 a
})

# -------- Plot TPM Single Gene ------------------

output$RiboTagPlot <- renderPlot({
  
   d <- mini_tabelka_RiboTag()
   
   if(nrow(d) != 0){
     
   g1 <-  ggplot(data = d, aes(x = Cell_Type, y = value, fill = rnk)) +
          geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6, colour = "grey40") +
          geom_text(aes(label = round(value, digits = 0)), vjust = -0.4, size = 6, color = "grey60", position = position_dodge(width = 0.8)) +
          ylim(0, ceiling(max(d$value)) + 0.05*max(d$value))
   
   }else{
     
     g1 <-  ggplot(data = melted_10w_CT %>% dplyr::group_by(Cell_Type) %>% dplyr::sample_n(size = 1),
                   aes(x = Cell_Type, y = value)) +
       geom_blank() 
   }

   g1 <- g1 + scale_fill_manual(name = "Rank", values = niebieskie, drop = FALSE,
                        labels = c(expression(paste(0, "-", 0.1)),
                                   expression(paste(0.1, "-", 0.2)),
                                   expression(paste(0.2, "-", 0.3)),
                                   expression(paste(0.3, "-", 0.4)),
                                   expression(paste(0.4, "-", 0.5)),
                                   expression(paste(0.5, "-", 0.6)),
                                   expression(paste(0.6, "-", 0.7)),
                                   expression(paste(0.7, "-", 0.8)),
                                   expression(paste(0.8, "-", 0.9)),
                                   expression(paste(0.9, "-", 1))))  +
     
    guides(fill = guide_legend(title = expression(atop("Gene", "rank")))) +
     # theme_black() +
    theme(title = element_text(size = 16, face = "plain"),
          plot.margin = unit(c(0.5,1,0.5,0.5), "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 20),
          legend.position = "right",
          text = element_text(size = 16),
          axis.text.x = element_text(size = 16, face = "plain"),
          axis.text.y = element_text(size = 16, face = "plain"),
          legend.title.align = 0.5,
          legend.title = element_text(size=20),
          legend.text = element_text(size=12),
          legend.key.size = unit(1, 'lines'),
          strip.text.x = element_text(size = 16, face = "plain")) +
     
    labs(title = paste("Selected gene:", RiboTag_Entry_zmienna()), 
         subtitle = "Absolute RNA expression level",
         y = 'Transcripts per million (TPM)')
   
   g1
   
}, bg="white", execOnResize = F)
   
output$RiboTagPlot_RML <- renderPlot({
  
 e <- mini_tabelka_RiboTag_RML()
 
 if(nrow(e) != 0){
   
   g2 <- ggplot(data = e, aes(x = Cell_Type, y = L2FC, fill = p_cat)) +
         geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6, colour = "grey50") +
         geom_errorbar(aes(ymin = L2FC-L2FC_SE, ymax = L2FC+L2FC_SE), alpha = 0.4,  size = 0.7, linetype = "solid", color = "black", width=.3, position = position_dodge(width = 0.8)) +
         geom_text(aes(label = round(L2FC, digits = 2)), vjust = ifelse(e$L2FC > 0, -1, 2), size = 5, color = "black")
     
 }else{
   
   g2 <- ggplot(data = int_dot_plot_all_data %>% dplyr::group_by(Cell_Type) %>% dplyr::sample_n(size = 1),
                aes(x = Cell_Type, y = L2FC)) +
                ylim(-3, 3) +
                geom_blank()
 }
     
   g2 <- g2 +   labs(title = paste("Selected gene:", RiboTag_Entry_zmienna()), 
                subtitle = "Fold change in diseased mice (RML) compared with controls",
                y = bquote('Log'[2]*'FC over Control')) +
     
     scale_fill_manual( name = "FDR", values = rev(niebieskie), drop = FALSE,
                              labels = c(expression(paste("<",10^-8)),
                                         expression(paste(10^-8, "-", 10^-7)),
                                         expression(paste(10^-7, "-", 10^-6)),
                                         expression(paste(10^-6, "-", 10^-5)),
                                         expression(paste(10^-5, "-", 10^-4)),
                                         expression(paste(10^-4, "-", 10^-3)),
                                         expression(paste(10^-3, "-", 10^-2)),
                                         expression(paste(10^-2, "-", 10^-1)),
                                         expression(paste(10^-1, "-", 1))))  +
     
     guides(fill = guide_legend(title = bquote(atop("adjusted", "p-value")))) +
     
     # theme_black() +
     
     theme(title = element_text(size = 16, face = "plain"),
           plot.margin = unit(c(0.5,0.5,0.5,0), "cm"),
           axis.title.x = element_blank(),
           axis.title.y = element_text(size = 20),
           legend.position = "right",
           text = element_text(size = 16),
           axis.text.x = element_text(size = 16, face = "plain"),
           axis.text.y = element_text(size = 16, face = "plain"),
           legend.title.align = 0.5,
           legend.title = element_text(size=20),
           legend.text = element_text(size=12),
           aspect.ratio = 0.9,
           strip.text.x = element_text(size = 16, face = "plain"),
           legend.key.size = unit(1, 'lines')
          # panel.background = element_rect(fill = "grey40", colour = "grey") # bg of the panel
     ) +
     
     geom_abline(intercept=0, slope = 0, linetype="dashed",  color = "black", size=0.5) +
     
     switch(as.character(input$fix_axis_single_plot),
            "FALSE" = {facet_formula <- as.formula(paste("~","Timepoint"))
            facet_wrap(facet_formula, scales = "free")},
            "TRUE" = {facet_formula <- as.formula(paste("~","Timepoint"))
            facet_wrap(facet_formula, scales = "fixed")}) 
   
   g2

}, bg="transparent", execOnResize = F) # Plot RML with error bars #


# --- DotPlot Interactive
#
# table for this plot is pre-made and only FDR subsetting is done interactively; maybe add other type of subsetting

dot_values <- reactiveValues()

d_FDR_dot_plot <- reactive({
  input$FDR_dot_plot
}) %>% debounce(1000)

FDR_selected_dot_table <- reactive({
  switch(as.character(input$wpi_18_include),
         "TRUE" = {  k <- int_dot_plot_data_x_pts %>% dplyr::filter(FDR < 10**d_FDR_dot_plot())},
         "FALSE"  = {  k <- int_dot_plot_data_x_pts %>% dplyr::filter(FDR < 10**d_FDR_dot_plot(), Timepoint == "10 Weeks")}) 
  dot_values$wybrane <- rep(T, nrow(k))
  k
})


output$DotPlotEnrichment <- renderPlot({
  
  unlabeled   <- FDR_selected_dot_table()[  dot_values$wybrane, , drop = FALSE]
  labeled     <- FDR_selected_dot_table()[ !dot_values$wybrane, , drop = FALSE]
  
  g <- ggplot() +
    
    #-------------------------------------------------------------------------------------------
    geom_point(data = unlabeled, 
               aes(x = xPoints, 
                   y = L2FC, 
                   color = p_cat), 
               size=2, 
               alpha = 0.6, 
               inherit.aes = FALSE) +
    
    geom_point(data = labeled, 
               aes(x = xPoints, 
                   y = L2FC, 
                   color = p_cat), 
               size=3, 
               shape = 21,
               fill = "green", 
               alpha = 1, 
               inherit.aes = FALSE,
               show.legend = FALSE) +
  
  geom_text_repel(data = labeled %>% top_n(100, wt = 1/FDR), 
                  aes(x = xPoints, 
                      y = L2FC,
                      label = Gene_Symbol),
                  size = 3,
                  force = 1,
                  show.legend = FALSE) +
  
    scale_colour_manual( values = rev(niebieskie), drop = FALSE,
                         labels = c(expression(paste("<",10^-8)),
                                  expression(paste(10^-8, "-", 10^-7)),
                                  expression(paste(10^-7, "-", 10^-6)),
                                  expression(paste(10^-6, "-", 10^-5)),
                                  expression(paste(10^-5, "-", 10^-4)),
                                  expression(paste(10^-4, "-", 10^-3)),
                                  expression(paste(10^-3, "-", 10^-2)),
                                  expression(paste(10^-2, "-", 10^-1)),
                                  expression(paste(10^-1, "-", 1))))  +
  
    guides(colour = guide_legend(title = "FDR"),
                                 override.aes = list(linetype = 1,
                                                     alpha = 0.7,
                                                     size = 12)
                                                     ) +
    
    labs(y = bquote('Log'[2]*' Fold Change'),
         x = bquote(bold(''))) +
    scale_y_continuous(breaks = seq(-7, 7, 0.5)) +
    scale_x_continuous("Cell Type", breaks = seq(1,5,1), labels = c("Cx43","Gad2","PV","SST","vGluT2")) + 
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
          aspect.ratio=1,
          legend.direction = 'vertical',
          legend.position = 'right',
          legend.key = element_rect(size = 4, fill = NA),
          legend.key.size = unit(0, 'lines'),
          legend.title = element_text(size = 20),
          legend.spacing = unit(0.0, 'cm')) +
    
    switch(as.character(input$fix_axis_dot_plot),
           "FALSE" = {facet_formula <- as.formula(paste("~","Timepoint"))
           facet_wrap(facet_formula, scales = "free")},
           "TRUE" = {facet_formula <- as.formula(paste("~","Timepoint"))
           facet_wrap(facet_formula, scales = "fixed")}) 
g
  
}, bg="white", execOnResize = F) 


brasz <- reactive({
  input$dot_plot_brush
})

observeEvent(input$dot_plot_click, {
  wybrane_punkty <- nearPoints(FDR_selected_dot_table(), input$dot_plot_click, xvar = "xPoints", yvar = "L2FC", allRows = TRUE)
  dot_values$wybrane <- xor(dot_values$wybrane, wybrane_punkty$selected_)
})

#
  observeEvent(brasz(), {
  wybrane_punkty <- brushedPoints(FDR_selected_dot_table(), brasz(), xvar = "xPoints", yvar = "L2FC", allRows = TRUE)
  dot_values$wybrane <- xor(dot_values$wybrane, wybrane_punkty$selected_)
   })

  observeEvent(input$dot_reset, {
  dot_values$wybrane <- rep(TRUE, nrow(FDR_selected_dot_table()))
  })
  
dot_table_to_render <- reactive({DT::datatable( FDR_selected_dot_table()[ !dot_values$wybrane, ] %>%
                                                  dplyr::transmute(`Ensembl ID` = Ensembl_ID,
                                                                   `Symbol` = Gene_Symbol,
                                                                   `Cell Type` = Cell_Type,
                                                                   `Timepoint` = Timepoint,
                                                                   `Log2FC` = signif(L2FC, 3),
                                                                   `SE`= signif(L2FC_SE, 3),
                                                                   `FDR` = signif(FDR, 3),
                                                                   `Gene Rank` = signif(Gene_Rank, 3),
                                                                   `Biotype` = Biotype),
                                             selection = "multiple",
                                             rownames = FALSE,
                                             style = "bootstrap",
                                             class = 'table-bordered table-condensed',
                                             filter = list(position = "top"),
                                             extensions = c('Buttons', 'Responsive'),
                                             options = list(
                                               dom = 'Bfrtip',
                                               buttons = c('csv', 'excel', 'pdf', 'print', I('colvis')),
                                               orderClasses = TRUE,
                                               searchHighlight = TRUE,
                                               lengthMenu = c(5, 30, 50),
                                               pageLength = 5)) %>%

    formatStyle(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                fontSize = "12px") %>%

    formatStyle(
      "Gene Rank",
      fontSize = "12px",
      background = styleColorBar(int_dot_plot_data_x_pts$Gene_Rank, 'lightblue'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center') %>%

    formatStyle(
      "Log2FC",
      fontSize = "12px",
      color = "black",
      backgroundColor = styleInterval(myBreaks, clrs)) %>%

    formatPercentage("Gene Rank", 2)
})

output$table_click_1 <- DT::renderDataTable(dot_table_to_render(), server = FALSE)
  
  observeEvent(input$nieaktywny, {
    stopApp()
  })
  
  session$onSessionEnded(function() {
    stopApp()
    
  })
}