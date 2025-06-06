# Wymagane pakiety ----
library(tm)           # Przetwarzanie tekstu
library(SnowballC)    # Stemming
library(cluster)      # Klastrowanie
library(wordcloud)    # Chmury słów
library(factoextra)   # Wizualizacje klastrów
library(RColorBrewer) # Kolory
library(ggplot2)      # Wykresy
library(dplyr)        # Przetwarzanie danych
library(ggrepel)      # Dodawania etykiet w wykresach
library(DT)           # Interaktywne tabele



# Dane tekstowe ----

# Ustawienie ścieżki
#folder_path <- choose.dir(caption = "Wybierz folder z plikami tekstowymi")
#docs <- DirSource(directory = folder_path)
docs <- DirSource("C:\\Users\\szymo\\Documents\\WDR\\cleaned")

# Utwórz korpus dokumentów tekstowych
corpus <- VCorpus(docs)

# Korpus
inspect(corpus)


# 1. Przetwarzanie i oczyszczanie tekstu ----
# (Text Preprocessing and Text Cleaning)


# Normalizacja i usunięcie zbędnych znaków ----


# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))


# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))


# Usuń zbędne znaki lub pozostałości url, html itp.

# symbol @
corpus <- tm_map(corpus, toSpace, "@")

# symbol @ ze słowem (zazw. nazwa użytkownika)
corpus <- tm_map(corpus, toSpace, "@\\w+")

# linia pionowa
corpus <- tm_map(corpus, toSpace, "\\|")

# tabulatory
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

# CAŁY adres URL:
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

# http i https
corpus <- tm_map(corpus, toSpace, "http\\w*")

# tylko ukośnik odwrotny (np. po http)
corpus <- tm_map(corpus, toSpace, "/")

# pozostałość po re-tweecie
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

# inne pozostałości
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")


# Sprawdzenie
corpus[[1]][[1]][7:9]



corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
# corpus <- tm_map(corpus, removeWords, stopwords("english")) | polski nieobsługiwany
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)


# Sprawdzenie
corpus[[1]][[1]][7:9]

# usunięcie ewt. zbędnych nazw własnych
corpus <- tm_map(corpus, removeWords, custom_stopwords <- c(
  "ach", "aj", "albo", "bardzo", "bez", "bo", "być", "ci", "cię", "ciebie", "co", "czy", "daleko", "dla", "dlaczego",
  "dlatego", "do", "dobrze", "dokąd", "dość", "dużo", "dwa", "dwaj", "dwie", "dwoje", "dziś", "dzisiaj", "gdyby",
  "gdzie", "go", "ich", "ile", "im", "inny", "ja", "ją", "jak", "jakby", "jaki", "je", "jeden", "jedna", "jedno",
  "jego", "jej", "jemu", "jeśli", "jest", "jestem", "jeżeli", "już", "każdy", "kiedy", "kierunku", "kto", "ku",
  "lub", "ma", "mają", "mam", "mi", "mną", "mnie", "moi", "mój", "moja", "moje", "może", "mu", "my", "na", "nam",
  "nami", "nas", "nasi", "nasz", "nasza", "nasze", "natychmiast", "nią", "nic", "nich", "nie", "niego", "niej",
  "niemu", "nigdy", "nim", "nimi", "niż", "obok", "od", "około", "on", "ona", "one", "oni", "ono", "owszem", "po",
  "pod", "ponieważ", "przed", "przedtem", "są", "sam", "sama", "się", "skąd", "tak", "taki", "tam", "ten", "to",
  "tobą", "tobie", "tu", "tutaj", "twoi", "twój", "twoja", "twoje", "ty", "wam", "wami", "was", "wasi", "wasz",
  "wasza", "wasze", "we", "więc", "wszystko", "wtedy", "wy", "żaden", "zawsze", "że", "a", "aby", "acz",
  "aczkolwiek", "ale", "ależ", "aż", "bardziej", "bowiem", "by", "byli", "bynajmniej", "był", "była", "było",
  "były", "będzie", "będą", "cali", "cała", "cały", "cokolwiek", "coś", "czasami", "czasem", "czemu", "czyli",
  "gdy", "gdyż", "gdziekolwiek", "gdzieś", "i", "inna", "inne", "innych", "iż", "jakaś", "jakichś", "jakie",
  "jakiś", "jakiż", "jakkolwiek", "jako", "jakoś", "jednak", "jednakże", "jeszcze", "kilka", "kimś", "ktokolwiek",
  "ktoś", "która", "które", "którego", "której", "który", "których", "którym", "którzy", "lat", "lecz", "mimo",
  "między", "mogą", "moim", "możliwe", "można", "musi", "nad", "naszego", "naszych", "natomiast", "nawet", "no",
  "o", "oraz", "pan", "pana", "pani", "podczas", "pomimo", "ponad", "powinien", "powinna", "powinni", "powinno",
  "poza", "prawie", "przecież", "przede", "przez", "przy", "roku", "również", "sobie", "sobą", "sposób", "swoje",
  "ta", "taka", "takie", "także", "te", "tego", "tej", "teraz", "też", "totobą", "toteż", "trzeba", "twoim",
  "twym", "tych", "tylko", "tym", "u", "w", "według", "wiele", "wielu", "więcej", "wszyscy", "wszystkich",
  "wszystkie", "wszystkim", "właśnie", "z", "za", "zapewne", "zeznowu", "znów", "został", "żadna", "żadne",
  "żadnych", "żeby")) # lista stopwords z internetu
corpus <- tm_map(corpus, removeWords, custom_stopwords <- c("prostu", "jakieś", "naprawdę")) # uzupełnenie listy na podstawie wyników
corpus <- tm_map(corpus, stripWhitespace)

# Sprawdzenie
corpus[[1]][[1]][7:9]

# Stemming ---- | niezbyt się sprawdza dla tekstu po polsku


# Tokenizacja ----


# Macierze częstości TDM i DTM ---- | uzywamy TDM


# a) Funkcja TermDocumentMatrix() ----
# tokeny = wiersze, dokumenty = kolumny
tdm <- TermDocumentMatrix(corpus)
tdm
inspect(tdm)


tdm_m <- as.matrix(tdm)

# tdm_m[, 1:3] | za duzo slow zeby drukowac

# Można zapisać TDM w pliku .csv
# write.csv(tdm_m, file="TDM.csv")

# b) Funkcja DocumentTermMatrix() ----
# dokumenty = wiersze, tokeny = kolumny
dtm <- DocumentTermMatrix(corpus)
dtm
inspect(dtm)

dtm_m <- as.matrix(dtm)

# dtm_m[, 1:3] | jak wyżej

# Można zapisać DTM w pliku .csv
# write.csv(dtm_m, file="DTM.csv")

# 2. Zliczanie częstości słów ----
# (Word Frequency Count)

# Można zliczyć same częstości słów w macierzach
# dla TDM i DTM da to identyczny rezultat
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)

v2 <- sort(colSums(dtm_m), decreasing = TRUE)
dtm_df <- data.frame(word = names(v2), freq = v2)
head(dtm_df, 10)

# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura słów (globalna)
wordcloud(words = tdm_df$word, 
          freq = tdm_df$freq, 
          max.words = 30,
          min.freq = 7, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Wyświetl top 10
print(head(tdm_df, 10))

# 4. Inżynieria cech w modelu Bag of Words: ----
# Reprezentacja słów i dokumentów w przestrzeni wektorowej ----
# (Feature Engineering in vector-space BoW model)


# - podejście surowych częstości słów
# (częstość słowa = liczba wystąpień w dokumencie)
# (Raw Word Counts)



# Użyj utworzonej wcześniej macierzy DTM
dtm
dtm_m <- dtm_m[, apply(dtm_m, 2, function(col) length(unique(col)) > 1)] # bo inaczej wyskakuje error:
#Error in prcomp.default(data, scale = FALSE, center = FALSE) : 
#  cannot rescale a constant/zero column to unit variance
inspect(dtm)



# UCZENIE MASZYNOWE NIENADZOROWANE ----
# (Unsupervised Machine Learning)



# Klastrowanie k-średnich (k-means) ----


# Dobór liczby klastrów
# Metoda sylwetki (silhouette)
fviz_nbclust(t(dtm_m), kmeans, method = "silhouette") +
  labs(title = "Dobór liczby klastrów", subtitle = "Metoda sylwetki")



# Wykonaj klastrowanie kmeans
# (sprawdź wyniki dla k = 3,4,5)
set.seed(123) # ziarno losowe dla replikacji wyników



# a) Ustaw liczbę klastrów k = 2 ----
k <- 2 # ustaw liczbę klastrów


klastrowanie <- kmeans(dtm_m, centers = k)


# Wizualizacja klastrów
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastrów dokumentów")



# Interaktywna tabela z przypisaniem dokumentów i top 5 słów
# Dla każdego klastra: liczba dokumentów oraz top 5 słów
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokumentów = length(cluster_docs_idx),
    Top_5_słów = top_words,
    stringsAsFactors = FALSE
  )
})

# Połącz wszystko w ramkę danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokumentów z korpusu
document_names <- names(corpus)

# Tabela przypisania dokumentów do klastrów
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Dołączamy dane z podsumowania (JOIN po klastrze)
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Interaktywna tabela z pełnym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najczęstsze słowa i liczność klastrów",
          rownames = FALSE,
          options = list(pageLength = 10))




# Chmury słów dla każdego klastra
for (i in 1:k) {
  # znajdź indeksy dokumentów w danym klastrze
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  # nazwy plików odpowiadające dokumentom w tym klastrze
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  # generuj chmurę słów dla klastra
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 15, colors = brewer.pal(8, "Dark2"))
  title(paste("Chmura słów - Klaster", i))
}




# a) Przypisanie dokumentów do klastrów ----
document_names <- names(corpus)  # Nazwy dokumentów z korpusu
clusters <- klastrowanie$cluster  # Przypisanie dokumentów do klastrów

# Ramka danych: dokumenty i ich klastry
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podgląd
print(documents_clusters)


# a) Wizualizacja przypisania dokumentów do klastrów ----
ggplot(documents_clusters, aes(x = reorder(Dokument, Klaster), fill = Klaster)) +
  geom_bar(stat = "count", width = 0.7) +
  coord_flip() +
  labs(title = "Przypisanie dokumentów do klastrów",
       x = "Dokument",
       y = "Liczba wystąpień (powinna wynosić 1)",
       fill = "Klaster") +
  theme_minimal(base_size = 13)

