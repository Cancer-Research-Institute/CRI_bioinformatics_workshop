# install.packages("readxl")
library(readxl)
library(dplyr)

# df <- read.csv("tables4.txt")
df <- read.csv("tables4.txt", sep = "\t")
# dfX <- read.table("tables4.txt", sep = '\t')
df <- read.table("tables4.txt", sep = '\t', header = TRUE)

xl <- read_excel("_Supplementary Tables.xlsx", sheet = "table_s4")

head(xl)
str(xl)
str(df)

table(xl$comparison)

select_data <- filter(xl, comparison == "ontxc2CombinationSDPD_V_baselineCombinationSDPD")

ggplot()
ggplot(select_data, aes())
ggplot(select_data, aes(x = log2FoldChange, y = pvalue))
ggplot(select_data, aes(x = log2FoldChange, y = pvalue)) +
  geom_point()
ggplot(select_data, aes(x = log2FoldChange, y = pvalue)) +
  geom_point() + 
  scale_y_reverse() # XX 
ggplot(select_data, aes(x = log2FoldChange, y = -pvalue)) +
  geom_point()
ggplot(select_data, aes(x = log2FoldChange, y = -log10(pvalue))) +
  geom_point()
ggplot(select_data, aes(x = log2FoldChange, y = -log10(pvalue))) +
  geom_point() +
  scale_x_continuous(limits = c(-10,10))

ggplot(select_data, aes(x = log2FoldChange, y = -log10(pvalue))) +
  geom_point() +
  scale_x_continuous(limits = c(-10,10)) +
  theme_bw()
ggplot(select_data, aes(x = log2FoldChange, y = -log10(pvalue))) +
  geom_point(alpha = 0.2) +
  scale_x_continuous(limits = c(-10,10)) +
  theme_bw()
ggplot(select_data, aes(x = log2FoldChange, y = -log10(pvalue))) +
  geom_point(alpha = 0.2) +
  scale_x_continuous(limits = c(-10,10)) +
  theme_bw()
table(select_data$sig)
ggplot(select_data, aes(x = log2FoldChange, y = -log10(pvalue), colour = sig)) +
  geom_point(alpha = 0.2) +
  scale_x_continuous(limits = c(-10,10)) +
  theme_bw()
ggplot(select_data, aes(x = log2FoldChange, y = -log10(pvalue),
                        colour = interaction(sig, dir_i))) +
  geom_point(alpha = 0.2) +
  scale_x_continuous(limits = c(-10,10)) +
  theme_bw() # XX

ggplot(select_data, aes(x = log2FoldChange, y = -log10(pvalue),
                        colour = dir_i, fill = sig, shape = dir_i)) +
  geom_point(alpha = 1) +
  scale_shape_manual(values = c(21,22)) +
  scale_x_continuous(limits = c(-10,10)) +
  theme_bw()

select_data_addcol <- select_data %>%
  mutate(group = ifelse(sig == "NA", NA, dir_i))
ggplot(select_data_addcol, aes(x = log2FoldChange, y = -log10(pvalue),
                        fill = group)) +
  geom_point(alpha = 1, shape = 21) +
  scale_x_continuous(limits = c(-10,10)) +
  theme_bw()
ggplot(select_data_addcol, aes(x = log2FoldChange, y = -log10(pvalue),
                               fill = group, color = group)) +
  geom_point(alpha = 0.5, shape = 21) +
  scale_x_continuous(limits = c(-10,10)) +
  scale_fill_manual(values = c('red3','dodgerblue','grey90'))+
  scale_colour_manual(values = c('black','black','grey90'))+
  theme_bw()

# install.packages("ggrepel")
genes <- c("CCL21", "CXCL13","CD8A","GZMB")
select_data_addcol_addlabel <- select_data_addcol %>%
  mutate(mylabel = ifelse(Gene %in% genes, Gene, NA))
ggplot(select_data_addcol_addlabel, aes(x = log2FoldChange, y = -log10(pvalue),
                               fill = group, color = group)) +
  geom_point(alpha = 0.5, shape = 21) +
  geom_label(aes(label = mylabel)) +
  scale_x_continuous(limits = c(-10,10)) +
  scale_fill_manual(values = c('red3','dodgerblue','grey90'))+
  scale_colour_manual(values = c('black','black','grey90'))+
  theme_bw()
ggplot(select_data_addcol_addlabel, aes(x = log2FoldChange, y = -log10(pvalue),
                                        fill = group, color = group)) +
  geom_point(alpha = 0.5, shape = 21) +
  geom_label(aes(label = mylabel), color = "black") +
  scale_x_continuous(limits = c(-10,10)) +
  scale_fill_manual(values = c('red3','dodgerblue','grey90'))+
  scale_colour_manual(values = c('black','black','grey90'))+
  theme_bw()

ggplot(select_data_addcol_addlabel, aes(x = log2FoldChange, y = -log10(pvalue),
                                        fill = group, color = group)) +
  geom_point(alpha = 0.5, shape = 21) +
  geom_label(aes(label = mylabel), fill = "white") +
  scale_x_continuous(limits = c(-10,10)) +
  scale_fill_manual(values = c('red3','dodgerblue','grey90'))+
  scale_colour_manual(values = c('black','black','grey90'))+
  theme_bw()

library(ggrepel)
ggplot(select_data_addcol_addlabel, aes(x = log2FoldChange, y = -log10(pvalue),
                                        fill = group, color = group)) +
  geom_point(alpha = 0.5, shape = 21) +
  geom_label_repel(aes(label = mylabel), fill = "white") +
  scale_x_continuous(limits = c(-10,10)) +
  scale_fill_manual(values = c('red3','dodgerblue','grey90'))+
  scale_colour_manual(values = c('black','black','grey90'))+
  theme_bw()

ggplot(select_data_addcol_addlabel, aes(x = log2FoldChange, y = -log10(pvalue),
                                        fill = group, color = group)) +
  geom_point(alpha = 0.5, shape = 21) +
  geom_label_repel(aes(label = mylabel), fill = "white") +
  scale_x_continuous(limits = c(-10,10)) +
  scale_fill_manual(values = c('red3','dodgerblue','grey90'))+
  scale_colour_manual(values = c('black','black','grey90'))+
  labs(x = "Log2 Fold Change", y = "-log10 (p value)",
       color = "significance group", fill = "other name") +
  theme_bw()
ggplot(select_data_addcol_addlabel, aes(x = log2FoldChange, y = -log10(pvalue),
                                        fill = group, color = group)) +
  geom_point(alpha = 0.5, shape = 21) +
  geom_label_repel(aes(label = mylabel), fill = "white") +
  scale_x_continuous(limits = c(-10,10)) +
  scale_fill_manual(values = c('red3','dodgerblue','grey90'),
                    labels = c("downregulated","upregulated","ns"))+
  scale_colour_manual(values = c('black','black','grey90'),
                      labels = c("downregulated","upregulated","ns"))+
  labs(x = "Log2 Fold Change", y = "-log10 (p value)",
       color = "significance group", fill = "other name") +
  theme_bw()
