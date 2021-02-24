#now, readiblity measures. 
#going to do one for each congress, to make it easier to compare overtime, 
#plus avoid adding variables for now.

#inserting new dictionary function

# Textstat_readability_by_SoRelle. New function "dictionary"
textstat_readability_with_own_dict <- function(x,
                                               measure = "Flesch",
                                               remove_hyphens = TRUE,
                                               min_sentence_length = 1,
                                               max_sentence_length = 10000,
                                               intermediate = FALSE,
                                               dictionary=NULL, 
                                               ...) {
  UseMethod("textstat_readability_with_own_dict")
}

textstat_readability_with_own_dict.default <- function(x,
                                                       measure = "Flesch",
                                                       remove_hyphens = TRUE,
                                                       min_sentence_length = 1,
                                                       max_sentence_length = 10000,
                                                       intermediate = FALSE, ...) {
  stop(friendly_class_undefined_message(class(x), "textstat_readability"))
}

textstat_readability_with_own_dict.corpus <- function(x,
                                                      measure = "Flesch",
                                                      remove_hyphens = TRUE,
                                                      min_sentence_length = 1,
                                                      max_sentence_length = 10000,
                                                      intermediate = FALSE,
                                                      dictionary, 
                                                      ...) {
  
  
  measure_option <- c("ARI", "ARI.simple", "ARI.NRI",
                      "Bormuth", "Bormuth.MC", "Bormuth.GP",
                      "Coleman", "Coleman.C2",
                      "Coleman.Liau.ECP", "Coleman.Liau.grade", "Coleman.Liau.short",
                      "Dale.Chall", "Dale.Chall.old", "Dale.Chall.PSK",
                      "Danielson.Bryan", "Danielson.Bryan.2",
                      "Dickes.Steiwer", "DRP", "ELF", "Farr.Jenkins.Paterson",
                      "Flesch", "Flesch.PSK", "Flesch.Kincaid",
                      "FOG", "FOG.PSK", "FOG.NRI", "FORCAST", "FORCAST.RGL",
                      "Fucks", "Linsear.Write", "LIW",
                      "nWS", "nWS.2", "nWS.3", "nWS.4", "RIX",
                      "Scrabble",
                      "SMOG", "SMOG.C", "SMOG.simple", "SMOG.de",
                      "Spache", "Spache.old", "Strain",
                      "Traenkle.Bailer", "Traenkle.Bailer.2",
                      "Wheeler.Smith",
                      "meanSentenceLength",
                      "meanWordSyllables", "dictionary")
  accepted_measures <- c(measure_option, "Bormuth", "Coleman.Liau")
  
  if (measure[1] == "all") {
    measure <- measure_option
  } else {
    is_valid <- measure %in% accepted_measures
    if (!all(is_valid))
      stop("Invalid measure(s): ", measure[!is_valid])
  }
  
  if ("Bormuth" %in% measure) {
    measure[measure == "Bormuth"] <- "Bormuth.MC"
    measure <- unique(measure)
  }
  
  if ("Coleman.Liau" %in% measure) {
    measure[measure == "Coleman.Liau"] <- "Coleman.Liau.ECP"
    measure <- unique(measure)
  }
  
  x <- texts(x)
  if (!is.null(min_sentence_length) || !is.null(max_sentence_length)) {
    temp <- char_trim(x, "sentences",
                      min_ntoken = min_sentence_length,
                      max_ntoken = max_sentence_length)
    x[names(temp)] <- temp
    x[!names(x) %in% names(temp)] <- ""
  }
  
  # get sentence lengths - BEFORE lower-casing
  n_sent <- nsentence(x)
  
  # get the word length and syllable info for use in computing quantities
  x <- char_tolower(x)
  toks <- tokens(x, remove_punct = TRUE)
  
  # number of syllables
  n_syll <- nsyllable(toks)
  # replace any NAs with a single count (most of these will be numbers)
  n_syll <- lapply(n_syll, function(y) ifelse(is.na(y), 1, y))
  
  # lengths in characters of the words
  len_token <- lapply(toks, stringi::stri_length)
  
  # to avoid "no visible binding for global variable" CHECK NOTE
  textID <- W <- St <- C <- Sy <- W3Sy <- W2Sy <- W_1Sy <- W6C <- W7C <- Wlt3Sy <- W_wl.Dale.Chall <-
    W_wl.Spache <- ARI <- ARI.NRI <- ARI.simple <- Bormuth.MC <- Bormuth.GP <- Coleman <- Coleman.C2 <-
    Coleman.Liau.ECP <- Coleman.Liau.grade <- Coleman.Liau.short <- Dale.Chall <- Dale.Chall.old <-
    Dale.Chall.PSK <- Danielson.Bryan <- Danielson.Bryan.2 <- Dickes.Steiwer <- DRP <- ELF <-
    Farr.Jenkins.Paterson <- Flesch <- Flesch.PSK <- Flesch.Kincaid <- FOG <- FOG.PSK <- FOG.NRI <-
    FORCAST <- FORCAST.RGL <- Fucks <- Linsear.Write <- LIW <- nWS <- nWS.2 <- nWS.3 <- nWS.4 <-
    RIX <- SMOG <- SMOG.C <- SMOG.simple <- SMOG.de <- Spache <- Spache.old <- Strain <- Wheeler.Smith <-
    Bl <- Traenkle.Bailer <- Traenkle.Bailer.2 <- Bormuth <- Bormuth.MC.Temp <- Coleman.Liau.ECP.Temp <-
    Coleman.Liau <- meanSentenceLength <- meanWordSyllables <- NULL
  
  # common statistics required by (nearly all) indexes
  library(data.table)
  temp <- data.table(textID = names(x),
                     W = lengths(toks),  # number of words
                     St = n_sent,            # number of sentences
                     C =  vapply(len_token, sum, numeric(1)), # number of characters (letters)
                     Sy = vapply(n_syll, sum, numeric(1)),    # number of syllables
                     W3Sy =  vapply(n_syll, function(x) sum(x >= 3), numeric(1)),  # number words with >= 3 syllables
                     W2Sy =  vapply(n_syll, function(x) sum(x >= 2), numeric(1)),  # number words with >= 2 syllables
                     W_1Sy = vapply(n_syll, function(x) sum(x == 1), numeric(1)),  # number words with 1 syllable
                     W6C = vapply(len_token, function(x) sum(x >= 6), numeric(1)), # number of words with at least 6 letters
                     W7C = vapply(len_token, function(x) sum(x >= 7), numeric(1))) # number of words with at least 7 letters
  
  temp[, Wlt3Sy := W - W3Sy]   # number of words with less than three syllables
  
  # look up D-C words if needed
  
  
  if (any(c("Dale.Chall", "Dale.Chall.old", "Dale.Chall.PSK", "Bormuth.MC", "Bormuth.GP", "DRP") %in% measure)) {
    temp[, W_wl.Dale.Chall := lengths(tokens_remove(toks,
                                                    pattern = quanteda::data_char_wordlists$dalechall,
                                                    valuetype = "fixed",
                                                    case_insensitive = TRUE))]
  }
  
  # look up D-C words if needed
  if ("dictionary" %in% measure) {
    temp[, W_wl.dictionary := lengths(tokens_remove(toks,
                                                    pattern = c(quanteda::data_char_wordlists$dalechall, dictionary),
                                                    valuetype = "fixed",
                                                    case_insensitive = TRUE))]
  }
  
  if ("ARI" %in% measure)
    temp[, ARI := 0.5 * W / St + 4.71 * C / W - 21.43]
  
  if ("ARI.NRI" %in% measure)
    temp[, ARI.NRI := 0.4 * W / St + 6 * C / W - 27.4]
  
  if ("ARI.simple" %in% measure)
    temp[, ARI.simple := W / St + 9 * C / W]
  
  if ("Bormuth.MC" %in% measure) {
    temp[, Bormuth.MC := 0.886593 - (0.08364 * C / W) + 0.161911 * (W_wl.Dale.Chall / W) ^ 3 -
           0.21401 * (W / St) + 0.000577 * (W / St) ^ 2 - 0.000005 * (W / St) ^ 3]
  }
  if ("Bormuth.GP" %in% measure) {
    CCS <- 35 # Cloze criterion score, percent as integer
    temp[, Bormuth.MC.Temp := 0.886593 - (0.08364 * C / W) + 0.161911 *
           (W_wl.Dale.Chall / W) ^ 3 - 0.21401 * (W / St) + 0.000577 *
           (W / St) ^ 2 - 0.000005 * (W / St) ^ 3]
    temp[, Bormuth.GP := 4.275 +
           12.881 * Bormuth.MC.Temp -
           (34.934 * Bormuth.MC.Temp^2) +
           (20.388 * Bormuth.MC.Temp^3) +
           (26.194 * C - 2.046 * CCS ^ 2) - (11.767 * CCS ^ 3) -
           (44.285 * Bormuth.MC.Temp * CCS) +
           (97.620 * (Bormuth.MC.Temp * CCS)^2) -
           (59.538 * (Bormuth.MC.Temp * CCS)^3)]
    temp[, Bormuth.MC.Temp := NULL]
  }
  
  if ("Coleman" %in% measure)
    temp[, Coleman := 1.29 * (100 * W_1Sy / W) - 38.45]
  
  if ("Coleman.C2" %in% measure)
    temp[, Coleman.C2 := 1.16 * (100 * W_1Sy / W) + 1.48 * (100 * St / W) - 37.95]
  
  ## cannot compute Coleman.C3, Coleman.C4 without knowing the number of pronouns or prepositions
  
  if ("Coleman.Liau.ECP" %in% measure)
    temp[, Coleman.Liau.ECP   := 141.8401 - 0.214590 * (100 * C / W) + 1.079812 * (100 * St / W)]
  
  if ("Coleman.Liau.grade" %in% measure) {
    temp[, Coleman.Liau.ECP.Temp   := 141.8401 - 0.214590 * (100 * C / W) + 1.079812 * (100 * St / W)]
    temp[, Coleman.Liau.grade := -27.4004 * Coleman.Liau.ECP.Temp / 100 + 23.06395]
    temp[, Coleman.Liau.ECP.Temp   := NULL]
  }
  
  if ("Coleman.Liau.short" %in% measure)
    temp[, Coleman.Liau.short := 5.88 * C / W - 29.6 * St / W - 15.8]
  
  if ("Dale.Chall" %in% measure) {
    temp[, Dale.Chall := 64 - 0.95 * 100 * W_wl.Dale.Chall / W - 0.69 * W / St]
  }
  
  if ("dictionary" %in% measure) {
    temp[, dictionary := 64 - 0.95 * 100 * W_wl.dictionary / W - 0.69 * W / St]
  }
  
  
  if ("Dale.Chall.old" %in% measure) {
    DC_constant <- NULL
    temp[, DC_constant := ((W_wl.Dale.Chall / W) > .05) * 3.6365]
    temp[, Dale.Chall.old := 0.1579 * 100 * W_wl.Dale.Chall / W + 0.0496 * W / St + DC_constant]
    temp[, DC_constant := NULL]
  }
  
  # Powers-Sumner-Kearl (1958) variation
  if ("Dale.Chall.PSK" %in% measure)
    temp[, Dale.Chall.PSK := 0.1155 * 100 * W_wl.Dale.Chall / W + 0.0596 * W / St + 3.2672]
  
  if ("Danielson.Bryan" %in% measure) {
    temp[, Bl := W - 1]  # could be more accurate if count spaces
    temp[, Danielson.Bryan := (1.0364 * C / Bl) + (0.0194 * C / St) - 0.6059]
    temp[, Bl := NULL]
  }
  
  if ("Danielson.Bryan.2" %in% measure) {
    temp[, Bl := W - 1]  # could be more accurate if count spaces
    temp[, Danielson.Bryan.2 := 131.059 - (10.364 * C / Bl) + (0.0194 * C / St)]
    temp[, Bl := NULL]
  }
  
  if ("Dickes.Steiwer" %in% measure) {
    TTR <- textstat_lexdiv(dfm(x, verbose = FALSE), measure = "TTR")$TTR
    temp[, Dickes.Steiwer := 235.95993 - (73.021 * C / W) - (12.56438 * W / St) - (50.03293 * TTR)]
  }
  
  if ("DRP" %in% measure) {
    temp[, Bormuth.MC.Temp := 0.886593 - (0.08364 * C / W) + 
           0.161911 * (W_wl.Dale.Chall / W) ^ 3 -
           0.21401 * (W / St) +
           0.000577 * (W / St) ^ 2 - 0.000005 * (W / St) ^ 3]
    temp[, DRP := (1 - Bormuth.MC.Temp) * 100]
    temp[, Bormuth.MC.Temp := NULL]
  }
  
  if ("ELF" %in% measure)
    temp[, ELF := W2Sy / St]
  
  if ("Farr.Jenkins.Paterson" %in% measure)
    temp[, Farr.Jenkins.Paterson := -31.517 - 1.015 * W / St + 1.599 * W_1Sy / W]
  
  if ("Flesch" %in% measure)
    temp[, Flesch := 206.835 - 1.015 * W / St - 84.6 * Sy / W]
  
  if ("Flesch.PSK" %in% measure)
    temp[, Flesch.PSK := 0.0778 * W / St + 4.55 * Sy / W - 2.2029]
  
  if ("Flesch.Kincaid" %in% measure)
    temp[, Flesch.Kincaid := 0.39 * W / St + 11.8 * Sy / W - 15.59]
  
  if ("meanSentenceLength" %in% measure)
    temp[, meanSentenceLength := W / St]
  
  if ("meanWordSyllables" %in% measure)
    temp[, meanWordSyllables := Sy / W]
  
  if ("FOG" %in% measure)
    temp[, FOG := 0.4 * (W / St + 100 * W3Sy / W)]
  # If the text was POS-tagged accordingly, proper nouns and combinations of only easy words
  # will not be counted as hard words, and the syllables of verbs ending in "-ed", "-es" or
  # "-ing" will be counted without these suffixes.
  
  if ("FOG.PSK" %in% measure)
    temp[, FOG.PSK := 3.0680 * ( 0.0877 * W / St ) + (0.0984 * 100 * W3Sy / W )]
  
  if ("FOG.NRI" %in% measure)
    temp[, FOG.NRI := ((( Wlt3Sy + 3 * W3Sy ) / (100 * St / W)) - 3) / 2]
  
  if ("FORCAST" %in% measure)
    temp[, FORCAST := 20 - (W_1Sy * 150 / W) / 10]
  
  if ("FORCAST.RGL" %in% measure)
    temp[, FORCAST.RGL := 20.43 - 0.11 * W_1Sy * 150 / W]
  
  if ("Fucks" %in% measure)
    temp[, Fucks := C / W * W / St]
  
  if ("Linsear.Write" %in% measure)
    temp[, Linsear.Write := ((100 - (100 * Wlt3Sy) / W) + (3 * 100 * W3Sy / W)) / (100 * St / W)]
  
  if ("LIW" %in% measure)
    temp[, LIW := (W / St) + (100 * W7C) / W]
  
  if ("nWS" %in% measure)
    temp[, nWS := 19.35 * W3Sy / W + 0.1672 * W / St + 12.97 * W6C / W - 3.27 * W_1Sy / W - 0.875]
  
  if ("nWS.2" %in% measure)
    temp[, nWS.2 := 20.07 * W3Sy / W + 0.1682 * W / St + 13.73 * W6C / W - 2.779]
  
  if ("nWS.3" %in% measure)
    temp[, nWS.3 := 29.63 * W3Sy / W + 0.1905 * W / St - 1.1144]
  
  if ("nWS.4" %in% measure)
    temp[, nWS.4 := 27.44 * W3Sy / W + 0.2656 * W / St - 1.693]
  
  if ("RIX" %in% measure)
    temp[, RIX := W7C / St]
  
  if ("SMOG" %in% measure)
    temp[, SMOG := 1.043 * sqrt(W3Sy * 30 / St) + 3.1291]
  
  if ("SMOG.C" %in% measure)
    temp[, SMOG.C := 0.9986 * sqrt(W3Sy * 30 / St + 5) + 2.8795]
  
  if ("SMOG.simple" %in% measure)
    temp[, SMOG.simple := sqrt(W3Sy * 30 / St) + 3]
  
  if ("SMOG.de" %in% measure)
    temp[, SMOG.de := sqrt(W3Sy * 30 / St) - 2]
  
  if (any(c("Spache", "Spache.old") %in% measure)) {
    # number of words which are not in the Spache word list
    temp[, W_wl.Spache := lengths(tokens_remove(toks,
                                                pattern = quanteda::data_char_wordlists$spache,
                                                valuetype = "fixed",
                                                case_insensitive = TRUE))]
  }
  
  if ("Spache" %in% measure)
    temp[, Spache := 0.121 * W / St + 0.082 * (100 * W_wl.Spache / W) + 0.659]
  
  if ("Spache.old" %in% measure)
    temp[, Spache.old := 0.141 * W / St + 0.086 * (100 * W_wl.Spache / W) + 0.839]
  
  if (any(c("Spache", "Spache.old") %in% measure)) temp[, W_wl.Spache := NULL]
  
  if ("Strain" %in% measure)
    temp[, Strain := Sy * 1 / (St / 3) / 10]
  
  if ("Traenkle.Bailer" %in% measure) {
    Wprep <- vapply(toks, function(x) sum(x %in% prepositions), numeric(1))  # English prepositions
    Wconj <- vapply(toks, function(x) sum(x %in% conjunctions), numeric(1))  # English conjunctions
    temp[, Traenkle.Bailer := 224.6814 - (79.8304 * C / W) - (12.24032 * W / St) - (1.292857 * 100 * Wprep / W)]
  }
  
  if ("Traenkle.Bailer.2" %in% measure) {
    Wprep <- vapply(toks, function(x) sum(x %in% prepositions), numeric(1))  # English prepositions
    Wconj <- vapply(toks, function(x) sum(x %in% conjunctions), numeric(1))  # English conjunctions
    temp[, Traenkle.Bailer.2 := 234.1063 - (96.11069 * C / W) - (2.05444 * 100 * Wprep / W) - (1.02805 * 100 * Wconj / W)]
  }
  
  #     if ("TRI" %in% measure) {
  #         Ptn <- lengths(tokens(x, remove_punct = FALSE)) - lengths(toks)
  #         Frg <- NA  # foreign words -- cannot compute without a dictionary
  #         temp[, TRI := (0.449 * W_1Sy) - (2.467 * Ptn) - (0.937 * Frg) - 14.417]
  #     }
  
  if ("Wheeler.Smith" %in% measure)
    temp[, Wheeler.Smith := W / St * (10 * W2Sy) / W]
  
  Scrabble <- NULL
  if ("Scrabble" %in% measure)
    temp[, Scrabble := nscrabble(x, mean)]
  
  result <- data.frame(document = names(x), stringsAsFactors = FALSE)
  
  
  # if intermediate is desired, add intermediate quantities to output
  if (intermediate)
    measure <- c(measure, names(temp)[names(temp) %in%
                                        c(c("W", "St", "C", "Sy", "W3Sy", "W2Sy", "W_1Sy",
                                            "W6C", "W7C", "Wlt3Sy", "W_wl.Dale.Chall", "W_wl.Spache", "dictionary"))])
  
  result <- cbind(result, as.data.frame(temp[, measure, with = FALSE]))
  
  # make any NA or NaN into NA (for #1976)
  result[is.na(result)] <- NA
  
  class(result) <- c("readability", "textstat", "data.frame")
  rownames(result) <- NULL # as.character(seq_len(nrow(result)))
  return(result)
}


textstat_readability.character <- function(x,
                                           measure = "Flesch",
                                           remove_hyphens = TRUE,
                                           min_sentence_length = 1,
                                           max_sentence_length = 10000, ...) {
  
  textstat_readability(corpus(x), measure, remove_hyphens,
                       min_sentence_length, max_sentence_length, ...)
}

conjunctions <- c("for", "and", "nor", "but", "or", "yet", "so")
prepositions <- c("a", "abaft", "abeam", "aboard", "about", "above", "absent",
                  "across", "afore", "after", "against", "along", "alongside",
                  "amid", "amidst", "among", "amongst", "an", "anenst", "apropos",
                  "apud", "around", "as", "aside", "astride", "at", "athwart", "atop",
                  "barring", "before", "behind", "below", "beneath", "beside", "besides",
                  "between", "beyond", "but", "by", "chez", "circa", "ca", "c",
                  "concerning", "despite", "down", "during", "except",
                  "excluding", "failing", "following", "for", "forenenst", "from",
                  "given", "in", "including", "inside", "into",
                  "like", "mid", "midst", "minus", "modulo", "near", "next",
                  "notwithstanding", "o'", "of", "off", "on", "onto",
                  "opposite", "out", "outside", "over", "pace", "past", "per", "plus",
                  "pro", "qua", "regarding", "round", "sans", "save", "since", "than",
                  "through", "thru", "throughout", "thruout", "times", "to", "toward",
                  "towards", "under", "underneath", "unlike", "until", "unto", "up",
                  "upon", "versus", "vs", "v", "via", "vis-a-vis", "with", "within",
                  "without", "worth")

dalechalldict <- read.csv("dcwordlist.csv")

#now adding dictionaries from text frequency.

dictmeasure109 <- textstat_readability_with_own_dict(
  cong109_corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong109dict, dalechalldict)
summary(dictmeasure109)

#110
dictmeasure110 <- textstat_readability_with_own_dict(
  cong110_corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong110dict, dalechalldict)
summary(dictmeasure110)

#111
dictmeasure111 <- textstat_readability_with_own_dict(
  cong111_corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong111dict, dalechalldict)

#112
dictmeasure112 <- textstat_readability_with_own_dict(
  cong112_corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong112dict, dalechalldict)

#113
dictmeasure113 <- textstat_readability_with_own_dict(
  cong113_corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong113dict, dalechalldict)

#114
dictmeasure114 <- textstat_readability_with_own_dict(
  cong114_corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong114dict, dalechalldict)

#115
dictmeasure115 <- textstat_readability_with_own_dict(
  cong115_corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong115dict, dalechalldict)

#boxplots
#first bind the measures to compare
head(dictmeasure110)
dictmeasure110 <-dictmeasure110 %>% 
  mutate(congsession = "110")

dictmeasure109 <- dictmeasure109 %>%
  mutate(congsession = "109")

dictmeasure111 <- dictmeasure111 %>%
  mutate(congsession = "111")

dictmeasure112 <- dictmeasure112 %>%
  mutate(congsession = "112")

dictmeasure113 <- dictmeasure113 %>%
  mutate(congsession = "113")

dictmeasure114 <- dictmeasure114 %>%
  mutate(congsession = "114")

dictmeasure115 <- dictmeasure115 %>%
  mutate(congsession = "115")

dictmeasuresfull <- rbind(dictmeasure109, dictmeasure110, dictmeasure111,
                               dictmeasure112, dictmeasure113, dictmeasure114,
                               dictmeasure115)
head(dictmeasuresfull)

ggplot(dictmeasuresfull, aes(x=congsession, y=dictionary, fill=congsession)) + 
  geom_boxplot()+
  labs(title="Complexity of Documents",x="Congressional Session", y = "Dictionary Method") +
  theme_classic()+
  scale_fill_brewer(palette="BuGn")+
  theme(legend.position="none")

###measuring by document type now
#109
dictmeasure109CHRG <- textstat_readability_with_own_dict(
  CHRG109corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong109dict, dalechalldict)
dictmeasure109CHRG <- dictmeasure109CHRG %>%
  mutate(congsession = "109", doctype = "CHRG")

dictmeasure109CREC <- textstat_readability_with_own_dict(
  CREC109corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong109dict, dalechalldict)
dictmeasure109CREC <- dictmeasure109CREC %>%
  mutate(congsession = "109", doctype = "CREC")

#110
dictmeasure110CHRG <- textstat_readability_with_own_dict(
  CHRG110corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong110dict, dalechalldict)
dictmeasure110CHRG <- dictmeasure110CHRG %>%
  mutate(congsession = "110", doctype = "CHRG")

dictmeasure110CREC <- textstat_readability_with_own_dict(
  CREC110corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong110dict, dalechalldict)
dictmeasure110CREC <- dictmeasure110CREC %>%
  mutate(congsession = "110", doctype = "CREC")

#111
dictmeasure111CHRG <- textstat_readability_with_own_dict(
  CHRG111corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong111dict, dalechalldict)
dictmeasure111CHRG <- dictmeasure111CHRG %>%
  mutate(congsession = "111", doctype = "CHRG")

dictmeasure111CREC <- textstat_readability_with_own_dict(
  CREC111corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong111dict, dalechalldict)
dictmeasure111CREC <- dictmeasure111CREC %>%
  mutate(congsession = "111", doctype = "CREC")

#112
dictmeasure112CHRG <- textstat_readability_with_own_dict(
  CHRG112corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong112dict, dalechalldict)
dictmeasure112CHRG <- dictmeasure112CHRG %>%
  mutate(congsession = "112", doctype = "CHRG")

dictmeasure112CREC <- textstat_readability_with_own_dict(
  CREC112corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong112dict, dalechalldict)
dictmeasure112CREC <- dictmeasure112CREC %>%
  mutate(congsession = "112", doctype = "CREC")

#113
dictmeasure113CHRG <- textstat_readability_with_own_dict(
  CHRG113corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong109dict, dalechalldict)
dictmeasure113CHRG <- dictmeasure113CHRG %>%
  mutate(congsession = "113", doctype = "CHRG")

dictmeasure113CREC <- textstat_readability_with_own_dict(
  CREC113corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong113dict, dalechalldict)
dictmeasure113CREC <- dictmeasure113CREC %>%
  mutate(congsession = "113", doctype = "CREC")

#114
dictmeasure114CHRG <- textstat_readability_with_own_dict(
  CHRG114corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong114dict, dalechalldict)
dictmeasure114CHRG <- dictmeasure114CHRG %>%
  mutate(congsession = "114", doctype = "CHRG")

dictmeasure114CREC <- textstat_readability_with_own_dict(
  CREC114corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong114dict, dalechalldict)
dictmeasure114CREC <- dictmeasure109CREC %>%
  mutate(congsession = "114", doctype = "CREC")

#115
dictmeasure115CHRG <- textstat_readability_with_own_dict(
  CHRG115corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong115dict, dalechalldict)
dictmeasure115CHRG <- dictmeasure115CHRG %>%
  mutate(congsession = "115", doctype = "CHRG")

dictmeasure115CREC <- textstat_readability_with_own_dict(
  CREC115corp,
  measure = c("dictionary"),
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE, 
  dictionary=congress, cong115dict, dalechalldict)
dictmeasure115CREC <- dictmeasure115CREC %>%
  mutate(congsession = "115", doctype = "CREC")

##plot it
dictmeasuresbydoc <- rbind(dictmeasure109CHRG, dictmeasure109CREC, 
                           dictmeasure110CHRG, dictmeasure110CREC,
                           dictmeasure111CHRG, dictmeasure111CREC,
                           dictmeasure112CHRG, dictmeasure112CREC,
                           dictmeasure113CHRG, dictmeasure113CREC,
                           dictmeasure114CHRG, dictmeasure114CREC,
                           dictmeasure115CHRG, dictmeasure115CREC)
head(dictmeasuresbydoc)

ggplot(dictmeasuresbydoc, aes(x=congsession, y=dictionary, fill=doctype)) + 
  geom_boxplot()+
  labs(title="Complexity of Documents",x="Congressional Session", y = "Dictionary Method") +
  theme_classic()+
  scale_fill_brewer(palette="BuGn")+
  labs(fill = "Document Type")

write_csv(dictmeasuresbydoc, "dictmeasuresbydoc2.csv")
data<-read.csv("dictmeasuresbydoc2.csv")
head(data)
#Flesch

#109th
Flesch109 <- textstat_readability(
  cong109_corp,
  measure = "Flesch",
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE)

#110th
Flesch110 <- textstat_readability(
  cong110_corp,
  measure = "Flesch",
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE)

#111th
Flesch111 <- textstat_readability(
  cong111_corp,
  measure = "Flesch",
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE)

#112th
Flesch112 <- textstat_readability(
  cong112_corp,
  measure = "Flesch",
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE)

#113th
Flesch113 <- textstat_readability(
  cong113_corp,
  measure = "Flesch",
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE)


#114th
Flesch114 <- textstat_readability(
  cong114_corp,
  measure = "Flesch",
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE)

#115th
Flesch115 <- textstat_readability(
  cong115_corp,
  measure = "Flesch",
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE)


#NOW to do the same the with Dale-Chall readibility

#109th
DaleChall109 <- textstat_readability(
  cong109_corp,
  measure = "Dale.Chall",
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE)

#110th
DaleChall110 <- textstat_readability(
  cong110_corp,
  measure = "Dale.Chall",
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE)

#111th
DaleChall111 <- textstat_readability(
  cong111_corp,
  measure = "Dale.Chall",
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE)

#112th
DaleChall112 <- textstat_readability(
  cong112_corp,
  measure = "Dale.Chall",
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE)

#113th
DaleChall113 <- textstat_readability(
  cong113_corp,
  measure = "Dale.Chall",
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE)


#114th
DaleChall114 <- textstat_readability(
  cong114_corp,
  measure = "Dale.Chall",
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE)


#115th
DaleChall115 <- textstat_readability(
  cong115_corp,
  measure = "Dale.Chall",
  remove_hyphens = FALSE,
  min_sentence_length = 1,
  max_sentence_length = 10000,
  intermediate = FALSE)

#creating a dataframe to compare the scores

head(DaleChall109)
head(dictmeasure109)
head(Flesch109)

#creating dataframe to save
df109 <- merge(Flesch109, DaleChall109, by= "document")
df110 <- merge (Flesch110, DaleChall110, by="document")
df111 <- merge(Flesch111, DaleChall111, by="document")
df112 <- merge(Flesch112, DaleChall112, by="document")
df113 <- merge(Flesch113, DaleChall113, by="document")
df114 <- merge(Flesch114, DaleChall114, by="document")
df115 <- merge(Flesch115, DaleChall115, by="document")
flesch_dccomparison <- rbind(df109, df110, df111, df112, df113, df114, df115)
write_csv(flesch_dccomparison, "flesch_dccompare.csv")

#just going to do dictionary and dale chall comparison

#109
df109compare <- merge(dictmeasure109, DaleChall109, by="document")

head(df109compare)
df109compare <- select (df109compare,-c(congsession))
df109compare <- merge(df109compare, DaleChallCHRG109, by="document")

#reshaping
df109melted <- reshape2::melt(df109compare, id.var='document')
head(df109melted)
tail(df109melted)

#ggplot
ggplot(df109melted, aes(x =document, y = value, colour = variable, group=variable)) +
  geom_line(aes(size=variable))+
  scale_colour_manual(values = c("#00441b", "#99d8c9"))+
  scale_size_manual(values=c(1, .5))+
  xlab("Document")+
  ylab("Complexity Score")+
  ggtitle("109th Congress Documents, Complexity Score")+
  theme(legend.title = element_blank())+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  
#yahoo! now, the rest

#110
df110compare <- merge(dictmeasure110, DaleChall110, by="document")
head(df110compare)
df110compare <- select (df110compare,-c(congsession))
df110compare <- merge(df110compare, DaleChallCHRG110, by="document")
df110compare <- select (df110compare,-c(congsession))

df110melted <- reshape2::melt(df110compare, id.var='document')
head(df110melted)

ggplot(df110melted, aes(x =document, y = value, colour = variable, group=variable)) +
  geom_line(aes(size=variable))+
  scale_colour_manual(values = c("#00441b", "#99d8c9"))+
  scale_size_manual(values=c(1, .5))+
  xlab("Document")+
  ylab("Complexity Score")+
  ggtitle("110th Congress Documents, Complexity Score")+
  theme(legend.title = element_blank())+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#111
df111compare <- merge(dictmeasure111, DaleChall111, by="document")
head(df111compare)
df111compare <- select (df111compare,-c(congsession.x, congsession.y))
df111compare <- merge(df111compare, DaleChallCHRG111, by="document")
df111compare <- select (df111compare,-c(congsession))

df111melted <- reshape2::melt(df111compare, id.var='document')
head(df111melted)

ggplot(df111melted, aes(x =document, y = value, colour = variable, group=variable)) +
  geom_line(aes(size=variable))+
  scale_colour_manual(values = c("#00441b", "#99d8c9"))+
  scale_size_manual(values=c(1, .5))+
  xlab("Document")+
  ylab("Complexity Score")+
  ggtitle("111th Congress Documents, Complexity Score")+
  theme(legend.title = element_blank())+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#112
df112compare <- merge(dictmeasure112, DaleChall112, by="document")
head(df112compare)
df112compare <- select (df112compare,-c(congsession.x, congsession.y))
df112compare <- merge(df112compare, DaleChallCHRG112, by="document")
df112compare <- select (df112compare,-c(congsession))

df112melted <- reshape2::melt(df112compare, id.var='document')
head(df112melted)

ggplot(df112melted, aes(x =document, y = value, colour = variable, group=variable)) +
  geom_line(aes(size=variable))+
  scale_colour_manual(values = c("#00441b", "#99d8c9"))+
  scale_size_manual(values=c(1, .5))+
  xlab("Document")+
  ylab("Complexity Score")+
  ggtitle("112th Congress Documents, Complexity Score")+
  theme(legend.title = element_blank())+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#113
df113compare <- merge(dictmeasure113, DaleChall113, by="document") 
head(df113compare)
df113compare <- select (df113compare,-c(congsession.x, congsession.y))
df113compare <- merge(df113compare, DaleChallCHRG113, by="document")
df113compare <- select (df113compare,-c(congsession))

df113melted <- reshape2::melt(df113compare, id.var='document')
head(df113melted)

ggplot(df113melted, aes(x =document, y = value, colour = variable, group=variable)) +
  geom_line(aes(size=variable))+
  scale_colour_manual(values = c("#00441b", "#99d8c9"))+
  scale_size_manual(values=c(1, .5))+
  xlab("Document")+
  ylab("Complexity Score")+
  ggtitle("113th Congress Documents, Complexity Score")+
  theme(legend.title = element_blank())+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#114
df114compare <- merge(dictmeasure114, DaleChall114, by="document")
head(df114compare)
df114compare <- merge(df114compare, DaleChallCHRG114, by="document")
df114compare <- select (df114compare,-c(congsession))

df114melted <- reshape2::melt(df114compare, id.var='document')
head(df114melted)

ggplot(df114melted, aes(x =document, y = value, colour = variable, group=variable)) +
  geom_line(aes(size=variable))+
  scale_colour_manual(values = c("#00441b", "#99d8c9"))+
  scale_size_manual(values=c(1, .5))+
  xlab("Document")+
  ylab("Complexity Score")+
  ggtitle("114th Congress Documents, Complexity Score")+
  theme(legend.title = element_blank())+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#114
df115compare <- merge(dictmeasure115, DaleChall115, by="document")
head(df115compare)
df115compare <- select (df115compare,-c(congsession.x, congsession.y))
df115compare <- merge(df115compare, DaleChallCHRG115, by="document")
df115compare <- select (df115compare,-c(congsession))

df115melted <- reshape2::melt(df115compare, id.var='document')
head(df115melted)

ggplot(df115melted, aes(x =document, y = value, colour = variable, group=variable)) +
  geom_line(aes(size=variable))+
  scale_colour_manual(values = c("#00441b", "#99d8c9"))+
  scale_size_manual(values=c(1, .5, .5))+
  xlab("Document")+
  ylab("Complexity Score")+
  ggtitle("115th Congress Documents, Complexity Score")+
  theme(legend.title = element_blank())+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#find the dif between dale.chall and dictionary method to then plot
head(df109compare)
df109compare <- transform(df109compare, diff_dict_dc = dictionary - Dale.Chall)
head(df109compare)
df109compare <- select(df109compare,-c(new.col))

#graph it
df109compare <- df109compare %>% 
  mutate(mycolor = ifelse(diff_dict_dc>0, "aquamarine4", "lightgreen"))
head(df109compare)

gg109compare <- ggplot(df109compare, aes(x=document, y=diff_dict_dc)) +
  geom_segment( aes(x=document, xend=document, y=0, yend=diff_dict_dc, color=mycolor), size=1.3, alpha=0.9) +
  theme_light() +
  scale_color_identity()+
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("Document") +
  ylab("Difference") +
  ggtitle("109th Congress")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

gg109compare

#110
df110compare <- transform(df110compare, diff_dict_dc = dictionary - Dale.Chall)
head(df110compare)
df110compare <- select(df110compare,-c(mycolor))

df110compare <- df110compare %>% 
  mutate(mycolor = ifelse(diff_dict_dc>0, "aquamarine4", "lightgreen"))

gg110compare<- ggplot(df110compare, aes(x=document, y=diff_dict_dc)) +
  geom_segment( aes(x=document, xend=document, y=0, yend=diff_dict_dc, color=mycolor), size=1.3, alpha=0.9) +
  theme_light() +
  scale_color_identity()+
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("Document") +
  ylab("Difference") +
  ggtitle("110th Congress")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#111
df111compare <- transform(df111compare, diff_dict_dc = dictionary - Dale.Chall)
head(df111compare)

df111compare <- df111compare %>% 
  mutate(mycolor = ifelse(diff_dict_dc>0, "aquamarine4", "lightgreen"))

gg111compare <- ggplot(df111compare, aes(x=document, y=diff_dict_dc)) +
  geom_segment( aes(x=document, xend=document, y=0, yend=diff_dict_dc, color=mycolor), size=1.3, alpha=0.9) +
  theme_light() +
  scale_color_identity()+
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("Document") +
  ylab("Difference") +
  ggtitle("111th Congress")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#112
df112compare <- transform(df112compare, diff_dict_dc = dictionary - Dale.Chall)
head(df112compare)

df112compare <- df112compare %>% 
  mutate(mycolor = ifelse(diff_dict_dc>0, "aquamarine4", "lightgreen"))

gg112compare <- ggplot(df112compare, aes(x=document, y=diff_dict_dc)) +
  geom_segment( aes(x=document, xend=document, y=0, yend=diff_dict_dc, color=mycolor), size=1.3, alpha=0.9) +
  theme_light() +
  scale_color_identity()+
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("Document") +
  ylab("Difference") +
  ggtitle("112th Congress")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#113
df113compare <- transform(df113compare, diff_dict_dc = dictionary - Dale.Chall)
head(df113compare)

df113compare <- df113compare %>% 
  mutate(mycolor = ifelse(diff_dict_dc>0, "aquamarine4", "lightgreen"))

gg113copmare <- ggplot(df113compare, aes(x=document, y=diff_dict_dc)) +
  geom_segment( aes(x=document, xend=document, y=0, yend=diff_dict_dc, color=mycolor), size=1.3, alpha=0.9) +
  theme_light() +
  scale_color_identity()+
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("Document") +
  ylab("Difference") +
  ggtitle("113th Congress")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#114
df114compare <- transform(df114compare, diff_dict_dc = dictionary - Dale.Chall)
head(df114compare)

df114compare <- df114compare %>% 
  mutate(mycolor = ifelse(diff_dict_dc>0, "aquamarine4", "lightgreen"))

gg114compare <- ggplot(df114compare, aes(x=document, y=diff_dict_dc)) +
  geom_segment( aes(x=document, xend=document, y=0, yend=diff_dict_dc, color=mycolor), size=1.3, alpha=0.9) +
  theme_light() +
  scale_color_identity()+
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("Document") +
  ylab("Difference") +
  ggtitle("114th Congress")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#115
df115compare <- transform(df115compare, diff_dict_dc = dictionary - Dale.Chall)
head(df115compare)

df115compare <- df115compare %>% 
  mutate(mycolor = ifelse(diff_dict_dc>0, "aquamarine4", "lightgreen"))

gg115compare<- ggplot(df115compare, aes(x=document, y=diff_dict_dc)) +
  geom_segment( aes(x=document, xend=document, y=0, yend=diff_dict_dc, color=mycolor), size=1.3, alpha=0.9) +
  theme_light() +
  scale_color_identity()+
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("Document") +
  ylab("Difference") +
  ggtitle("115th Congress")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

install.packages("ggpubr"
)
library(ggpubr)
dale.chall.dictcompare <- ggarrange(gg109compare, gg110compare, gg111compare,
                                    gg112compare, gg113copmare, gg114compare, gg115compare,
                    ncol = 2, nrow = 2)

par(mfrow=c(2,4)) 
gg109compare
gg110compare
gg111compare
gg112compare

64-(0.95*100*((100-20)/100)-(0.69*8))

64-(0.95*100*((100-80)/100)-(0.69*8))

