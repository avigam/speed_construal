##AG 12.11.20
#based on tutorial at:
#https://quanteda.io/articles/pkgdown/replication/text2vec.html
#modified to use Reuters corpus (see below)
#and comparing to concreteness norms from: 
# http://crr.ugent.be/archives/1330

#rm(list=ls())
#cat("\f")
#if(!is.null(dev.list())) dev.off()

####Setup####
library(quanteda)
library(text2vec)
library(tidyverse)

#Get Reuters 21578 corpus
# install.packages("tm.corpus.Reuters21578", repos = "http://datacube.wu.ac.at")
library(tm.corpus.Reuters21578)
data(Reuters21578)

####embeddings reuters corpus####
#convert from tm Vcorpus to quanteda corpus
reut_corp <- corpus(Reuters21578)
reut_toks <- tokens(reut_corp) #tokenize

#dfm for filtering out terms less common
feats <- dfm(reut_toks, verbose = TRUE) %>%
  dfm_trim(min_termfreq = 5) %>%
  featnames()

reut_toks <- tokens_select(reut_toks, feats, padding = TRUE)

#extract co-occurrences
reut_fcm <- fcm(reut_toks, context = "window", count = "weighted", weights = 1 / (1:5), tri = TRUE)

#word embeddings using glove (text2vec/rsparse)
glove <- GlobalVectors$new(rank = 50, x_max = 10)
wv_main <- glove$fit_transform(reut_fcm, n_iter = 10,
                               convergence_tol = 0.01, n_threads = 8)

#save matrix of embeddings ("word vectors")
wv_context <- glove$components
dim(wv_context)
word_vectors <- wv_main + t(wv_context)

#save the vector for "fast" so we can compare similarities to it
fast <- word_vectors["fast", , drop = FALSE]

#loading table with words, their concreteness level and a parts of speech label
concrete_tab <- read_csv(file.path('~','R','speed_construal','conc_m.csv'))
#extract the words which in concrete_tab and embeddings
# (excluding the word fast itself)
both <- intersect(rownames(word_vectors),concrete_tab$Word[concrete_tab$Word!='fast'])
#cool! ~10,000 words
#sample 100 words for faster run, and easier visual (see plot below)
sampledW <- both[sample(length(both),100,F)]

#save their embeddings vectors
both_W_vec <- word_vectors[both,,drop=F]
sampled_W_vec <- word_vectors[sampledW,,drop=F]

#extract the cosine similarity of the (100) words with 'fast'
cos_sim <- textstat_simil(x = as.dfm(sampled_W_vec), y = as.dfm(fast),
                          method = "cosine")
#save data frame with data to plot - words, similarities, concreteness norms
toPlot <- data.frame(y=cos_sim@x,x=concrete_tab$Conc.M[concrete_tab$Word%in%cos_sim@Dimnames[[1]]],word=cos_sim@Dimnames[[1]])

#scatter plot with concreteness on the x-axis and similarity to 'fast' on the y-axis
#plus a regression line of a simple linear regression for y~x
#also writing the parameters (b,r,r2) of the regression 
ggplot(toPlot,aes(x=x,y=y))+geom_point()+geom_smooth(method = 'lm',formula = 'y~x',se = T)+
  geom_label(aes(x=mean(x),y=mean(y)+0.15),label=paste("b = ",round(lm(y~x,toPlot)$coefficients['x'],3),"\nr = ",round(cor(toPlot$x,toPlot$y),3),"\nR2 = ",round(summary(lm(y~x,toPlot))$r.squared,3)))+
  labs(x='Concreteness',y="Cos. similarity to 'fast'")

#plotting the scatter plot with the words in their respective points
ggplot(toPlot)+geom_text(aes(x=x,y=y,label=word),size=3)+
  labs(x='Concreteness',y="Cos. similarity to 'fast'")

#now with all words
cos_sim_ext <- textstat_simil(x = as.dfm(both_W_vec), y = as.dfm(fast),
                              method = "cosine")
toPlot <- data.frame(y=cos_sim_ext@x,x=concrete_tab$Conc.M[concrete_tab$Word%in%cos_sim_ext@Dimnames[[1]]],word=cos_sim_ext@Dimnames[[1]])

#again plot
p1 <- ggplot(toPlot,aes(x=x,y=y))+geom_point()+geom_smooth(method = 'lm',formula = 'y~x',se = T)+
  geom_label(aes(x=mean(x),y=mean(y)+0.15),label=paste("b = ",round(lm(y~x,toPlot)$coefficients['x'],3),"\nr = ",round(cor(toPlot$x,toPlot$y),3),"\nR2 = ",round(summary(lm(y~x,toPlot))$r.squared,3)))+
  labs(x='Concreteness',y="Cos. similarity to 'fast'")

# png(file.path('~','R','speed_construal','all_w_scatter.PNG'),width = 478,height = 279)
p1
# dev.off()

#hmm very uncorrelated, but so far we included all words
#let's try only the nouns

#index of which words in the concreteness table are tagged as nouns
noun.ind <- concrete_tab$Dom_Pos=='Noun'

#again the words in common
both.N <- intersect(rownames(word_vectors),concrete_tab$Word[noun.ind])
#cool! ~5,000 nouns
#again sample 100 words
sampledW.N <- both.N[sample(length(both.N),100,F)]

#save embeddings of relevant words
both_W_vec.N <- word_vectors[both.N,,drop=F]
sampled_W_vec.N <- word_vectors[sampledW.N,,drop=F]

#similarities for 100w
cos_sim.N <- textstat_simil(x = as.dfm(sampled_W_vec.N), y = as.dfm(fast),
                          method = "cosine")
#plot
toPlot <- data.frame(y=cos_sim.N@x,x=concrete_tab$Conc.M[concrete_tab$Word%in%cos_sim.N@Dimnames[[1]]],word=cos_sim.N@Dimnames[[1]])

#scatter plot+regression
ggplot(toPlot,aes(x=x,y=y))+geom_point()+geom_smooth(method = 'lm',formula = 'y~x',se = T)+
  geom_label(aes(x=mean(x),y=mean(y)+0.15),label=paste("b = ",round(lm(y~x,toPlot)$coefficients['x'],3),"\nr = ",round(cor(toPlot$x,toPlot$y),3),"\nR2 = ",round(summary(lm(y~x,toPlot))$r.squared,3)))+
  labs(x='Concreteness',y="Cos. similarity to 'fast'")

#text
p2 <- ggplot(toPlot)+geom_text(aes(x=x,y=y,label=word),size=3)+
  labs(x='Concreteness',y="Cos. similarity to 'fast'")

# png(file.path('~','R','speed_construal','noun_samp_text.PNG'),width = 478,height = 279)
p2
# dev.off()

#now with all nouns
cos_sim_ext.N <- textstat_simil(x = as.dfm(both_W_vec.N), y = as.dfm(fast),
                              method = "cosine")
toPlot <- data.frame(y=cos_sim_ext.N@x,x=concrete_tab$Conc.M[concrete_tab$Word%in%cos_sim_ext.N@Dimnames[[1]]],word=cos_sim_ext.N@Dimnames[[1]])

#again scatter plot+regression
p3 <- ggplot(toPlot,aes(x=x,y=y))+geom_point()+geom_smooth(method = 'lm',formula = 'y~x',se = T)+
  geom_label(aes(x=mean(x),y=mean(y)+0.15),label=paste("b = ",round(lm(y~x,toPlot)$coefficients['x'],3),"\nr = ",round(cor(toPlot$x,toPlot$y),3),"\nR2 = ",round(summary(lm(y~x,toPlot))$r.squared,3)))+
  labs(x='Concreteness',y="Cos. similarity to 'fast'")

# png(file.path('~','R','speed_construal','noun_all_scatter.PNG'),width = 478,height = 279)
p3
# dev.off()

####embeddings wikipedia corpus####

wiki_corp <- quanteda.corpora::download(url = "https://www.dropbox.com/s/9mubqwpgls3qi9t/data_corpus_wiki.rds?dl=1")
wiki_toks <- tokens(wiki_corp)
feats <- dfm(wiki_toks, verbose = TRUE) %>%
  dfm_trim(min_termfreq = 5) %>%
  featnames()

wiki_toks <- tokens_select(wiki_toks, feats, padding = TRUE)

wiki_fcm <- fcm(wiki_toks, context = "window", count = "weighted", weights = 1 / (1:5), tri = TRUE)

glove <- GlobalVectors$new(rank = 50, x_max = 10)
wv_main <- glove$fit_transform(wiki_fcm, n_iter = 10,
                               convergence_tol = 0.01, n_threads = 8)

wv_context <- glove$components
dim(wv_context)
word_vectors <- wv_main + t(wv_context)
#save the vector for "fast" so we can compare similarities to it
fast <- word_vectors["fast", , drop = FALSE]

#loading table with words, their concreteness level and a parts of speech label
concrete_tab <- read_csv(file.path('~','R','speed_construal','conc_m.csv'))
#index of which words in the concreteness table are tagged as nouns
noun.ind <- concrete_tab$Dom_Pos=='Noun'

#again the words in common
both.N <- intersect(rownames(word_vectors),concrete_tab$Word[noun.ind])

both_W_vec.N <- word_vectors[both.N,,drop=F]

cos_sim_ext.N <- textstat_simil(x = as.dfm(both_W_vec.N), y = as.dfm(fast),
                                method = "cosine")
toPlot <- data.frame(y=cos_sim_ext.N@x,x=concrete_tab$Conc.M[concrete_tab$Word%in%cos_sim_ext.N@Dimnames[[1]]],word=cos_sim_ext.N@Dimnames[[1]])

#again scatter plot+regression
p4 <- ggplot(toPlot,aes(x=x,y=y))+geom_point()+geom_smooth(method = 'lm',formula = 'y~x',se = T)+
  geom_label(aes(x=mean(x),y=mean(y)+0.15),label=paste("b = ",round(lm(y~x,toPlot)$coefficients['x'],3),"\nr = ",round(cor(toPlot$x,toPlot$y),3),"\nR2 = ",round(summary(lm(y~x,toPlot))$r.squared,3)))+
  labs(x='Concreteness',y="Cos. similarity to 'fast'")

# png(file.path('~','R','speed_construal','wiki_noun_all_scatter.PNG'),width = 478,height = 279)
p4
# dev.off()
