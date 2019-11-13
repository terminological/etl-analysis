

#' Get a searcher of the omop databse
#'
#' @param omop a R6 object of Omop class
#' @keywords omop
#' @import dplyr
#' @export
#' @examples
#' search = Searcher$new(omop)
Searcher = R6::R6Class("Searcher", public=list(
  #### Fields ----
  omop = NULL,
  #### Methods ----
  initialize = function(omop) {
    self$omop = omop;
  },

  findConcepts = function(term, vocabularyList=NA, domainList=NA, classList=NA, standardConcepts = TRUE) {
    lhs = self$omop$concept_synonym %>% filter(concept_synonym_name %like% term)
    rhs = self$omop$concept %>% filter(is.null(invalid_reason))
    if (!is.na(domainList) && length(domainList)>0) {
      rhs = rhs %>% filter(domain_id %in% local(domainList))
    }
    if (!is.na(vocabularyList) && length(vocabularyList)>0) {
      rhs = rhs %>% filter(vocabulary_id %in% local(vocabularyList))
    }
    if (!is.na(classList) && length(classList)>0) {
      rhs = rhs %>% filter(concept_class_id %in% local(classList))
    }
    if (standardConcepts) {
      rhs = rhs %>% filter(!is.null(standard_concept))
    }
    tmp = lhs %>% left_join(rhs, by="concept_id")
    return(tmp)
  },

  findDomains = function(domain) {
    return(
      self$omop$concept %>%
        select(domain_id) %>%
        distinct() %>%
        filter(domain_id %like% local(domain)) %>%
        collect())
  },

  findVocabularies = function(vocab) {
    return(
      self$omop$concept %>%
        select(vocabulary_id) %>%
        distinct() %>%
        filter(vocabulary_id %like% local(vocab)) %>%
        collect())
  },

  findConceptClasses = function(class) {
    return(
      self$omop$concept %>%
        select(concept_class_id) %>%
        distinct() %>%
        filter(concept_class_id %like% local(class)) %>%
        collect())
  }

))

#' directly lookup concepts in db
#'
#' @param searcher a searcher R6 object e.g. Searcher$new(omop) or omop$searcher
#' @param term an input search string
#' @param vocabularyList a vocabulary filter (see findVocabularies)
#' @param domainList a domain filter (see findDomains)
#' @param classList a class filter (see findConceptClasses)
#' @param standardConcepts only return standard concepts
#' @keywords omop
#' @import dplyr
#' @export
#' @examples
#' omop = Omop$new(...)
#' omop$searcher$findConcepts(term)
findConcepts <- function(searcher, term, vocabularyList=NA, domainList=NA, classList=NA, standardConcepts = TRUE) {
  return(searcher$findConcepts(term, vocabularyList, domainList, classList, standardConcepts))
}

#' list vocabularies
#'
#' @param searcher a searcher R6 object e.g. Searcher$new(omop)
#' @param vocabulary a vocabulary filter
#' @keywords omop
#' @import dplyr
#' @export
#' @examples
#' omop = Omop$new(...)
#' omop$searcher$findVocabularies(term)
findVocabularies <- function(searcher, vocab) {
  return(searcher$findVocabularies(vocab))
}

#' list domains
#'
#' @param searcher a searcher R6 object e.g. Searcher$new(omop)
#' @param vocabulary a vocabulary filter
#' @keywords omop
#' @import dplyr
#' @export
#' @examples
#' omop = Omop$new(...)
#' omop$searcher$findDomains("domain")
findDomains <- function(searcher, domain) {
  return(searcher$findDomains(domain))
}

#' list concept classes
#'
#' @param searcher a searcher R6 object e.g. Searcher$new(omop)
#' @param class a concept class filter
#' @keywords omop
#' @import dplyr
#' @export
#' @examples
#' omop = Omop$new(...)
#' omop$searcher$findConceptClasses("domain")
findConceptClasses <- function(searcher, class) {
  return(searcher$findConceptClasses(class))
}
