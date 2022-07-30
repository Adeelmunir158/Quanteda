
install.packages("quanteda")
require(quanteda)
require(readtext)
require(dplyr) #Used for pipe operator
               #Ctrl+Shift+M for pipe operator

#====(1) Import & Read all files of a folder===========

#Set working directory to the folder containing 
#all text or pdf files

#Read all txt files
t1=readtext("*.txt")
t1
#Extracting text 
t2=t1$text

t2
#================(2) Corpus==================
#Convert to QUANTEDA Corpus
c=corpus(t1)
c
#----------(2.1) Summary of all files---------
summary(c)

# Token is word or sentence or alphbet

#Detailed summary
install.packages("quanteda.textstats")
require(quanteda.textstats)
textstat_summary(c)
textstat_readability(c)

#=======(3) Text cleaning using tokens========

t=tokens(c, remove_punct = T,
          remove_symbols = T,
          remove_numbers = T,
          remove_url = T,
          remove_separators = T)

t=tokens_tolower(t)
t=tokens_remove(t, pattern = stopwords("english"))
t
#Clean corpus created by OCR
t=tokens_select(t, 
                 c("[\\d-]", "[[:punct:]]", "^.{1,2}$"),
                 selection = "remove",
                 valuetype = "regex",
                 verbose = TRUE
                 )

#===========(4) Convert tokens to dfm (Document feature matrics)=========
d=dfm(t)
d
d=dfm_remove(d,stopwords("en"))
#ds=dfm_wordstem(d)
d
dfm_sort(d, decreasing = TRUE)

#Show only 10 entries
head(dfm_sort(d, decreasing = TRUE,margin = "both"),
              n = 10)


#-----------(4.1) Textstat Functions-----------

topfeatures(d,20)
require(quanteda.textstats)
textstat_frequency(d,30)

#count specific words in a corpus
mydict = dictionary(list(all_terms = c("rupees")))

tokens_select(t,mydict)
tokens_select(t,mydict) %>% dfm()


#--------------(4.2) Wordcloud-------------

install.packages("quanteda.textplots")
require(quanteda.textplots)

install.packages("RColorBrewer")
require(RColorBrewer)

textplot_wordcloud(d, 
                   min_count = 10,
                   rotation = .25,
                   color = brewer.pal(8, "Dark2"))

#---------(4.3) Comparison cloud-----------

textplot_wordcloud(d, 
                   comparison = T,
                   max_words = 300,
                   max_size = 7)

#------------(4.4) KeyWord In Context-----------

k=kwic(c,c("tamil","india"))
textplot_xray(k)

kwic(c, "india", 
     valuetype = "regex",8)

#---(4.5) Plot the dispersion of key word(s)---

textplot_xray(kwic(c, pattern = "tamil"),
              kwic(c, pattern = "india*"))


#-------------(4.4) Custom Dictionary----------
dict=dictionary(
  list(india=c("india","india's","indian"),
       country=c("country","countrymen","countries"),
       year=c("year","years"),
       numbers=c('one','two','three','four',
                 'five','six','seven','eight',
                 'nine','ten','eleven','twelve',
                 'thirteen','fourteen','fifteen')))

dfm(c, dictionary = dict)


#============(5) Sentiment Analysis===========
install.packages("quanteda.sentiment")
devtools::install_github("quanteda/quanteda.sentiment")
require(quanteda.sentiment)

textstat_polarity(t,dictionary = data_dictionary_LSD2015)


# AFINN
afinn <- read.delim(system.file("extdata/afinn/AFINN-111.txt", 
                                package = "quanteda.sentiment"),
                    header = FALSE, col.names = c("word", "valence"))

afinn
data_dictionary_afinn <- dictionary(list(afinn = afinn$word))

valence(data_dictionary_afinn) <- list(afinn = afinn$valence)

textstat_valence(t, data_dictionary_afinn, )

#Linguistic Inquiry and Word Count standalone software
install.packages("quanteda.dictionaries")
devtools::install_github("kbenoit/quanteda.dictionaries")
require(quanteda.dictionaries)
require(quanteda.sentiment) #Reqd for the data_dictionary_NRC
#liwcalike works only on tokens

liwcalike(t2)
q=liwcalike(t2, dictionary = data_dictionary_NRC)
q[,c(7:16)]


#=========(6) Watch 2 or 3 words together======

#-------------(6.1)Combine two words----------
require(quanteda.textstats)
text2=textstat_collocations(c, size=2,
                            min_count = 10)
text2

arrange(text2, desc(count))

# Three words
text3=textstat_collocations(c, size=3,
                            min_count = 5)

arrange(text3, desc(count))

#-------------(6.2)Combine three words----------

multiword=c("my dear countrymen",
            "mann ki baat",
            "ayushman bharat scheme",
            "modi ji-",
            "brothers and sisters",
            "nitesh gupta")

#Compound token is used for combining words
t=tokens(t2)
t=tokens_tolower(t)
tok_comp=tokens_compound(t, phrase(multiword))
tok_comp


text3.1=textstat_collocations(tok_comp, size=3,
                              min_count = 10)
arrange(text3.1, desc(count))


dict=dictionary(list(multiword))

tokens_lookup(tt,dictionary =  dict)
tt

#=====(7) Keyness, Similarity & Dissimilarity in Text====
d
ky=textstat_keyness(d, target = 1L)
ky
subset(ky, ky$chi2>10)
textplot_keyness(ky)


textstat_simil(d)
textstat_dist(d)
dis=textstat_dist(d)
h=hclust(as.dist(dis))
plot(h)


textstat_lexdiv(d)
