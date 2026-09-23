###### ========== Essai d'Adele ======= #########

# Chargement des packages

pacman::p_load(tidyverse, text2map, text2map.corpora, quanteda, install = T)

# Import de la base de données

df <- load_corpus('corpus_pitchfork')

# names(df)
# table(df$genre)
# head(df$genre)

small_df <- df |> filter(genre == "Rap")

# Minimal text preprocessing
corp <- corpus (small_df, text_field = "review" , docid_field = "doc_id")
toks <- tokens (corp, remove_punct = TRUE) |>
tokens_tolower() |>
tokens_ngrams(n=1:2)
dfm_obj <- dfm(toks)
dfm_obj <- dfm_remove(dfm_obj, pattern = stopwords("en"))
dfm_obj <- dfm_select(dfm_obj, min_nchar = 3)
dfm_obj <- dfm_trim(dfm_obj, min_termfreq = 2)

stops <- stopwords("en")
dfm_obj <- dfm_remove (dfm_obj,
  pattern = c(paste0("^", stops, "_") ,
  paste0("_", stops, "$")),
valuetype = "regex")

# Apply TF - IDF weighting to downweight filler words
tfidf_dfm <- dfm_tfidf (dfm_obj)
# Convert weighted dfm to a standard matrix
mat <- as.matrix (tfidf_dfm)
mat <- mat[ , apply (mat , 2 , var) > 0]
# Run PCA ( centering and scaling TF -IDF scores )
pca_themes <- prcomp(mat, center = T, scale. = T )

# Inspect variance explained by each principal component
plot(pca_themes , type = "l" , main = "Scree Plot of Latent Dimensions")
# Looks at eigenvalues
eigenvalues <- pca_themes$sdev^2
variance_explained <- eigenvalues/sum (eigenvalues) * 100
variance_explained |> 
  round(2)

# Inspect term loadings for different components
pca_themes$rotation[,1] %>% 
sort(decreasing=T) %>%
head(10)

# Install and load an extra package
pacman::p_load(ggrepel, install=T)
# Extract document scores and word loadings
doc_df <- as.data.frame(pca_themes$x[,c(1,2)])
colnames(doc_df) <- c("PC1", "PC2")
doc_df$title <- rownames(doc_df)
word_df <- as.data.frame(pca_themes$rotation[,c(1,2)])
colnames(word_df) <- c("PC1", "PC2")
word_df$word <- rownames(word_df)

# Filter and categorize words by where they load high
threshold <- 0.00005
filtered_words <- word_df %>%
filter(abs(PC1) > threshold | abs(PC2) > threshold) %>%
mutate(
dim_type = case_when(
abs(PC1)>threshold & abs(PC2)>threshold ~ "Both Dims",
abs(PC1)>threshold ~ "PC1 Only",
abs(PC2)>threshold ~ "PC2 Only"
)
)

scale_factor <- max(abs(doc_df$PC1))/max(abs(filtered_words$PC1))*0.65
filtered_words <- filtered_words %>%
mutate(PC1_scaled = PC1*scale_factor,
PC2_scaled = PC2*scale_factor)

# Plot it
plot <- ggplot()+
geom_text(data = doc_df ,
aes(x = PC1, y = PC2, label = title),
color = "black", alpha = 0.5, size = 2.5) +
geom_text_repel(data = filtered_words,
aes(x = PC1_scaled, y = PC2_scaled, label = word , color = dim_type),
fontface = "bold" , size = 3.5 ,
box.padding = 0.35 ,
point.padding = 0.2 ,
max.overlaps = Inf )

plot + scale_color_manual(values = c(
"PC1 Only" = "#440154ff",
"PC2 Only" = "#2a788eff" ,
"Both Dims" = "#7ad151ff"
) ) +
labs(x = "Principal Component 1",
y = "Principal Component 2",
color = "High Loading On : ") +
theme_bw () +
theme (
panel.grid.minor = element_blank() ,
panel.grid.major = element_line (color = "gray90") ,
legend.position = "bottom"
)
