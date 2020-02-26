#' Get a searcher of the omop databse
#'
#' @keywords omop
#' @import dplyr
#' @export
#' @examples
#' search = Searcher$new(omop)
Searcher = R6::R6Class("Searcher", public=list(
  
  # TODO: refactor according to 
  # https://github.com/alan-turing-institute/distr6/blob/master/R/Distribution.R
  # and https://github.com/RaphaelS1/R62S3/
  
  #### Fields ----
  #' @field omop the omop connection
  omop = NULL,
  #' @field result the search result
  result = NULL,
  
  #### Methods ----
  #' @description create a vocabulary searcher
  #' @param omop an R6 Omop object
  initialize = function(omop) {
    self$omop = omop;
    self$result = omop$concept %>% mutate(count=1)
    self$applyStandardFilters()
  },
  
  #### mapping accross relationships for either input datafram or searcher$result ----
  
  #' @description get standard concept equivalents for a set of concepts
  #' @return a searcher of related standard concepts
  mapToStandardConcepts = function() {
    return(self$mapToRelatedConcepts("Maps to"))
  },
  
  #' @description get standard concept equivalents for a set of concepts
  #' @param relationships a list of relationship ids as a string (see getRelationshipIds())
  #' @return a searcher of related standard concepts
  mapToRelatedConcepts = function(relationships) {
    tmp = self$omop$concept_relationship %>%
      inner_join(self$result %>% select(concept_id,count), by=c("concept_id_1"="concept_id"), copy=TRUE) %>%
      filter(relationship_id %in% local(relationships))
    self$result = self$omop$concept %>% 
      inner_join(tmp %>% group_by(concept_id_2) %>% summarise(count=sum(count,na.rm=TRUE)), by=c("concept_id"="concept_id_2")) 
    self$applyStandardFilters()
    return(self)
  },

  #' @description get ancestors for a set of concepts from the current generation upwards
  #' @param min the minimum number of levels to find
  #' @param max the maximum number of levels to find
  #' @param df optional dataframe - if none will use the searcher$result
  #' @return a searcher of ancestor concepts
  expandAncestorConcepts = function(min=0, max=1) {
    tmp = self$omop$concept_ancestor %>% 
      inner_join(
        self$result %>% select(concept_id,count), 
        by=c("descendant_concept_id"="concept_id"), 
        copy=TRUE) %>%
      filter(min_levels_of_separation >= local(min) & min_levels_of_separation <= local(max))
    self$result = self$omop$concept %>% 
      inner_join(
        tmp %>% group_by(ancestor_concept_id) %>% summarise(count=sum(count,na.rm=TRUE)), 
        by=c("concept_id"="ancestor_concept_id"))
    self$applyStandardFilters()
    return(self)
  },

  #' @description get a distinct set of descendants including current generation for a set of concepts - N.B. only works if they are standard concepts
  #' @param min the minimum number of levels to find
  #' @param max the maximum number of levels to find
  #' @return a searcher of descendant concepts
  expandDescendantConcepts = function(min=0, max=1000) {
    tmp = self$omop$concept_ancestor %>% 
      inner_join(
        self$result %>% select(concept_id,count), 
        by=c("ancestor_concept_id"="concept_id"), copy=TRUE) %>%
      filter(min_levels_of_separation >= local(min) & min_levels_of_separation <= local(max))
    self$result = self$omop$concept %>% 
      inner_join(
        tmp %>% group_by(descendant_concept_id) %>% summarise(count=sum(count,na.rm=TRUE)), 
        by=c("concept_id"="descendant_concept_id"))
    self$applyStandardFilters()
    return(self)
  },
  
  #### apply filter functions act on either searcher$result or input data frame ----
  
  #' @description get rid of entries we are not interested in
  #' @param df optional dataframe - if none will modify the searcher$result
  #' @return a data.frame or searcher$result of filtered concepts
  applyStandardFilters = function() {
    self$result = self$result %>%  
      filter(
        vocabulary_id != "SPL" &
        is.null(invalid_reason)
      )
    return(self)
  },
  
  #' @description filter domains
  #' @param domainList a domain filter (if length one supports wildcard)
  #' @return a searcher of filtered concepts
  applyDomainFilter = function(domainList) {
    if (length(domainList)>1) {
      self$result = self$result %>% filter(domain_id %in% local(domainList))
    } else {
      self$result = self$result %>% filter(domain_id %like% local(domainList))
    }
  },
  
  #' @description find vocabulary identifiers
  #' @param vocabList a vocabulary filter (if length one supports wildcard)
  #' @return a searcher of filtered concepts
  applyVocabularyFilter = function(vocabList) {
    if (length(vocabList)>1) {
      self$result = self$result %>% filter(vocabulary_id %in% local(vocabList))
    } else {
      self$result = self$result %>% filter(vocabulary_id %like% local(vocabList))
    }
  },
  
  #' @description list concept classes
  #' @param classList a concept class filter (if length one supports wildcard)
  #' @return a searcher of filtered concepts
  applyConceptClassesFilter = function(classList) {
    if (length(classList)>1) {
        self$result = self$result %>% filter(concept_class_id %in% local(classList))
    } else {
        self$result = self$result %>% filter(concept_class_id %like% local(classList))
    }
    return(self)
  },
  
  #' @description perform a search of a data frame or the current selection of concepts
  #' @param term an input search string
  #' @param df optional dataframe - if none will modify the searcher$result
  #' @return a data.frame or searcher$result of filtered concepts
  applyTermFilter = function(term) {
    lhs = self$result
    rhs = self$omop$concept_synonym %>% filter(concept_synonym_name %like% term) %>% select(concept_id,concept_synonym_name)
    self$result = lhs %>% semi_join(rhs, by="concept_id")
    return(self)
  },
  
  #' @description selects concepts which are used in a specific vocabulary and returns the results
  #' @param df a dataframe with a concept id field
  #' @param field the name of the field containing concept_ids
  #' @return the searcher subset to include only concepts used in the df
  applyUsedInFilter = function(df, field = "concept_id") {
    field = ensym(field)
    self$result = self$result %>% semi_join(
            df %>% select(concept_id = !!field),
            by="concept_id", copy=TRUE)
    return(self)
  },
  
  # #' @description retain or exclude concepts based on a set of rules
  # #' @param ... a list of rules
  # #' @param df optional dataframe - if none will modify the searcher$result
  # #' @return a data.frame or searcher$result of filtered concepts
  # applyFilters = function(df = NA, filters) {
  #   if(identical(df,NA)) {
  #     self$result = self$applyFilters(self$result,filters)
  #     invisible(self)
  #   } else {
  #     filters = enexprs(filters)
  #     for (f in filters) {
  #       df = df %>% filter(!!f)
  #     }
  #     return(df)
  #   }
  # },
  
  #' @description retain or exclude concepts based on a set of rules
  #' @param filter a dplyr filter
  #' @param df optional dataframe - if none will modify the searcher$result
  #' @return a data.frame or searcher$result of filtered concepts
  applyFilter = function(filter) {
    filter = enexpr(filter)
    self$result = self$result %>% filter(!!filter)
    return(self)
  },
  
  #### modify in place dataframe set functions ----
    
  #' @description exclude concepts present in a dataframe
  #' @param searcher the concepts to exclude
  #' @return the modified searcher itself
  setExclude = function(searcher) {
    self$result = self$result %>% anti_join(searcher$result, by="concept_id",copy=TRUE)
    return(self)
  },
  
  # TODO: https://dbplyr.tidyverse.org/articles/sql-translation.html
  # If you’re working with large data, it maybe also be helpful to set auto_index = TRUE
  
  #' @description append concepts from a dataframe
  #' @param searcher the concepts to append
  #' @return the modified searcher itself
  setUnion = function(searcher) {
    tmp = self$result %>% select(concept_id,count) %>% union(searcher$result %>% select(concept_id,count), copy=TRUE) %>%
      group_by(concept_id) %>% summarise(count = sum(count,na.rm=TRUE)) %>% compute()
    self$result = self$omop$concept %>% inner_join(tmp)
    return(self)
  },
  
  #' @description append concepts from a dataframe
  #' @param searcher the concepts to append
  #' @return the modified searcher itself
  setIntersect = function(searcher) {
    self$result = self$result %>% 
      inner_join(searcher$result %>% select(concept_id, count_rhs = count), copy=TRUE)
      mutate(count = count+count_rhs) %>% select(-count_rhs)
    return(self)
  },
  
  #### find information associated with searcher$result ----
  
  #' @description list domains in result
  #' @param df a dataframe with a concept_id field
  #' @return a data.frame of domain_id and counts
  getDomains = function() {
    return(
      self$result %>% 
        group_by(domain_id) %>%
        summarise(codes=n(), counts=sum(count,na.rm=TRUE)) %>% collect()
    )
  },
  
  #' @description list vocabulary identifiers in result
  #' @param df a dataframe with a concept_id field
  #' @return a data.frame of vocabulary_id and counts
  getVocabularies = function() {
    return(
      self$result %>% 
        group_by(vocabulary_id) %>%
        summarise(codes=n(), counts=sum(count,na.rm=TRUE)) %>% collect()
    )
  },
  
  #' @description list concept classes in result
  #' @param df a dataframe with a concept_id field
  #' @return a data.frame of concept_class_id and counts
  getConceptClasses = function() {
    return(
      self$result %>% 
        group_by(concept_class_id) %>%
        summarise(codes=n(), counts=sum(count,na.rm=TRUE)) %>% collect()
    )
  },
  
  #' @description explore available concept_relationships for current set of concepts
  #' @param df a dataframe with a concept_id field
  #' @return a data.frame of relationship_id and count
  getRelationshipIds = function() {
    return(self$result %>% 
        inner_join(self$omop$concept_relationship, by=c("concept_id"="concept_id_1")) %>%
        group_by(relationship_id) %>%
        summarise(codes=n(), counts=sum(count,na.rm=TRUE)) %>% collect()
    )
  },
  
  #### control modify in place searcher functions ----
  
  #' @description execute a dplyr::compute
  #' @return the searcher with a cached result
  compute = function() {
    self$result = self$result %>% compute() #name=paste0("cohort_",name),overwrite=TRUE)
    return(self)
  },
  
  #' @description gets the result as a dataframe
  #' @return the subset as a data frame
  toDataframe = function() {
    # self$result = self$result %>% compute() #name=paste0("cohort_",name),overwrite=TRUE)
    return(self$result)
  },
  
  #' @description shows the result as a list of concept_names
  #' @param max the maximum number of rows to return
  #' @return the subset as a data frame
  summary = function(max=200) {
    if (!("concept_name" %in% colnames(self$result))) {
      self$result = self$omop$concept %>% semi_join(self$result, by="concept_id", copy=TRUE)
    }
    print("Searcher:")
    print("  Vocabularies:")
    print(self$getVocabularies())
    print("  Concept classes:")
    print(self$getConceptClasses())
    print("  Domains:")
    print(self$getDomains())
    print("  Concept names:")
    print(self$result %>% arrange(desc(count)) %>% head(max) %>% collect() %>% select(concept_id,concept_name))
  },
  
  #' @description saves the data locally
  #' @param name the name of the file - initial part of path
  #' @return the searcher itself (not modified)
  save = function(name) {
    filename = normalizePath(paste0(name,".vocab.rds"),mustWork = FALSE)
    saveRDS(self$result %>% collect(), filename)
    invisible(self)
  },
  
  #' @description print the searcher and number of results
  #' @return the searcher itself (not modified)
  print = function() {
    print("R6 vocabulary searcher class")
    print(self$result %>% select(concept_id,count,concept_name))
    invisible(self)
  },
  
  #' @description print the searcher and number of results
  #' @return the searcher itself (not modified)
  printSql = function() {
    self$result %>% show_query()
    invisible(self)
  }
))

#### Static constructors ----

#' @name Searcher_fromSearch
#' @title Create a searcher from a search term
#' @usage Searcher$fromSearch(omop,term)
#' @description perform a search of the concept table
#' @param omop a object of class Omop
#' @param term an input search string
#' @return a data.frame of domain_id and counts
NULL
Searcher$fromSearch = function(omop, term) {
  s = Searcher$new(omop)
  s$applyTermFilter(term)
  s$applyStandardFilters()
  return(s)
}
  
#' @name Searcher_fromDataframe
#' @title Create a searcher from a data frame
#' @usage Searcher$fromDataframe(omop,df,field)
#' @description selects concepts which are used in a specific vocabulary and returns the results
#' @param omop the omop connection as a Omop class object
#' @param df a dataframe with a ???_concept_id field
#' @param field the name of the field containing concept_ids
#' @return a data from of concepts
NULL
Searcher$fromDataframe = function(omop, df, field = "concept_id") {
  field = ensym(field)
  s = Searcher$new(omop)
  if(!"count" %in% colnames(df)) {
    df = df %>% mutate(count=1)
  }
  s$result = s$omop$concept %>% inner_join(
    df %>% group_by(concept_id = !!field) %>% summarise(count=sum(count,na.rm=TRUE)),
    by="concept_id", copy=TRUE)
  return(s)
}

#' @name Searcher_fromConceptCode
#' @title Create a searcher from a code based search
#' @usage Searcher$fromConceptCode(omop,vocabulary_id,concept_code)
#' @description get concept by vocabulary and concept code and return as a searcher
#' @param omop the omop connection as a Omop class object
#' @param vocabulary_id a vocab filter (can take sql wild card)
#' @param concept_code a concept_code (can take sql wild card)
#' @return a searcher of matching concepts
NULL
Searcher$fromConceptCode = function(omop, vocabulary_id, concept_code) {
  s = Searcher$new(omop)  
  s$result = s$omop$concept %>% mutate(count=1) %>% filter(
        vocabulary_id %like% local(vocabulary_id) &
        concept_code %like% local(concept_code)
  )
  s$applyStandardFilters()
  return(s)
}

#' @name Searcher_load
#' @title Create a searcher from a .RDS file
#' @usage Searcher$load(path)
#' @description loads the data from local path
#' @param name the name of the file - initial part of path - no .vocab.rds extension
#' @return the modified searcher itself
NULL
Searcher$load = function(omop, name) {
  filename = normalizePath(paste0(name,".vocab.rds"),mustWork = FALSE)
  s = Searcher$new(omop)
  s$result = readRDS(filename)
  table = stringr::str_match(filename,"/([^/\\.]+)\\..+$")[1,2]
  if (!"count" %in% colnames(s$result)) {
    s$result = s$result %>% mutate(count=1)
  }
  dplyr::copy_to(s$omop$con, s$result, name=paste0(table,"_vocab"),overwrite=TRUE)
  if (!"concept_name" %in% colnames(s$result)) {
    s$result = s$omop$concept %>% inner_join(
      s$result %>% group_by(concept_id) %>% summarise(count=sum(count,na.rm=TRUE)),
      by="concept_id")
  }
  return(s)
}

#' SQL like filtering in data tables
#' 
#' allow a sql like syntax in data tables. N.b. conflict with data.table's definition of like which is 
#' based on regex.
#' %	Represents zero or more characters	bl% finds bl, black, blue, and blob
#' _	Represents a single character	h_t finds hot, hat, and hit
#' []	Represents any single character within the brackets	h[oa]t finds hot and hat, but not hit
#' ^	Represents any character not in the brackets	h[^oa]t finds hit, but not hot and hat
#' -	Represents a range of characters	c[a-b]t finds cat and cbt
#' TODO: escaped sql
#'
#' @keywords omop
#' @import dplyr
#' @export
"%like%" = function(vector, like, ignore.case = TRUE, fixed = FALSE) {
  pattern = like %>% stringr::str_replace_all("%",".*") %>% stringr::str_replace_all("_",".")
  if (is.factor(vector)) {
    as.integer(vector) %in% grep(pattern, levels(vector), ignore.case = ignore.case, fixed = fixed)
  } else {
    # most usually character, but integer and numerics will be silently coerced by grepl
    grepl(pattern, vector, ignore.case = ignore.case, fixed = fixed)
  }
}
