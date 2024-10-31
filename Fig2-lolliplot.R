# Required libraries
library(tidyverse)
library(openxlsx)
library(trackViewer)
library(scales)

# Set working directory
setwd("/Dropbox/Trichophyton_indotineae/Data")

# Read data
data <- read.xlsx('data.xlsx', 3) 

# Set gene colour
genecols <- c("#51C6E6", "#FF8833")

#Sset colours for level of resistance : High, Medium, Low, No
snpcols <- c('#FF0000', '#98CE31', "#0599ba", '#acacac') 

# Set gene length 
genes <- data.frame(
        seqnames = "seq1",
        start = c(2117685, 941062),
        end = c(2123712, 942593),
        gene = c("ERG11B", "ERG1")
    ) %>%
    arrange(start) %>%
    # adjust/mutate gene coordinate and reduce the width of gene
    mutate(offset = start - lag(end, default = NA) - 1) %>% 
    mutate(start2 =ifelse(is.na(offset), start, start - offset + 1000 ), 
        end2 =ifelse(is.na(offset), end, end - offset + 1000 ) 
    )

# Extract 21 mutation coordinate position and put into a variable - pos
pos <- lapply(
        strsplit(trimws(unlist(data[1,]))," "), 
        function(x) ifelse(length(x)==1, x, x[2])
    ) %>% 
    unlist %>% 
    trimws %>% 
    gsub(' ', '', .) %>% 
    as.integer
pos <- pos[3:length(pos)]

# Extract gene names corresponding to SNPs
idx <- which(colnames(data) %in% genes$gene)
n <- c(idx , ncol(data)+1) - 2
n <- (n - lag(n))[2:length(n)]
SNP = data.frame(
        seqnames = "seq1",
        pos = pos,
        gene = rep(c("ERG11B", "ERG1"), n)
    ) %>% 
    left_join(., dplyr::select(genes, gene, offset) ) %>%
    mutate(pos2=ifelse(is.na(offset), pos, pos-offset+1)) %>%
    mutate(name = paste0("snp", 1:nrow(.)))

# Turn excel data Y,N into 1,0
snpsdata <- column_to_rownames(data[-1,]%>%
    remove_rownames, 'ID') 
snpsdata[snpsdata=='Y'] <- 1
snpsdata[is.na(snpsdata)] <- 0
snpsdata[, 2:ncol(snpsdata)] <- apply(
    snpsdata[, 2:ncol(snpsdata)], 
    2, 
    function(x) as.integer(x)
    )

# Create pie chart for the resistance data
colnames(snpsdata)[2:ncol(snpsdata)] <- SNP$name
snpsdata <- group_by(snpsdata, Resistance) %>% 
    reframe(across(.cols = everything(), list(sum),.names="{.col}")) %>% 
    column_to_rownames('Resistance')   %>% 
    t %>% 
    as.data.frame %>%
    dplyr::select(High, Medium, Low, No)
snpstat <- snpsdata %>% 
    apply(., 1, function(x) x*100/sum(x)) %>%
    t %>%as.data.frame

# Set names for amino acid and add to the dataframe
amino_acid <- c("Ser775Phe", "Ala882Thr", "Ile954Thr", "Asp1093Tyr", "Asp1093Gly", "Gly1095Arg", "Gly1095Glu", "Tyr1096His", "Tyr1096Ser", "Gly1097Cys", "Gly1097Ser", 
           "Thr448Ala", "Ser443Pro", "Tyr414His", "Phe397Leu", "Leu393Phe", "Leu393Ser")
SNP$amino_acid <- amino_acid

# Tidy data for plotting graph
sample.gr <- GRanges("seq1", IRanges(SNP$pos2, width=1, names=SNP$amino_acid))

mcols(sample.gr) <- cbind(mcols(sample.gr), 
        snpstat %>%
        setNames(paste0('value', 1:ncol(.)))
    )
sample.gr$score = NULL
sample.gr$label <- NULL
sample.gr$node.label.col <- NULL
sample.gr$color <- rep(list(snpcols), nrow(SNP))
sample.gr$border <- "gray30"
features <- GRanges("seq1", 
        IRanges(start=genes$start2,
            end=genes$end2,
            names=genes$gene
        )
    )
features$fill <- genecols
features$height <- 0.05

xaxis <- dplyr::select(genes,start2, end2) %>% 
    t %>% 
    as.vector
names(xaxis) <- dplyr::select(genes,start, end) %>%
    t %>% 
    as.vector
legend <- list(
    title = "Terbinafine resistance",
    labels=c('High', 'Medium', 'Low', 'No'), 
    col='black', 
    fill=snpcols
    )

# Adjust the size for the pie chart according to the frequency
sample.gr$cex=rescale(rowSums(snpsdata)/(nrow(data)-1), c(0.7, 1.3))


# Plot the lolliplot graph and save it as a pdf
pdf('lolliplot.pdf', width=8, height=4)
lolliplot(
  sample.gr,
  ylab = 'Amino acid change', 
  features,
  type = "pie",
  yaxis = FALSE,
  xaxis = xaxis,
  legend = legend
)

dev.off()

