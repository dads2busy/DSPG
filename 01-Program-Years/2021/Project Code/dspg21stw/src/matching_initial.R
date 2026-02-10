library(readxl)
library(lingmatch)
library(stringdist)
library(Rcpp)
library(RcppParallel)

# sources
onet = read.csv('ONET_SOC_credentials.csv')
stw = read.csv('STW VCCS Industry Credentials_062021.csv')

## combine sources, remove duplicate certificates, and make a certificate-term matrix
#ELK question: good to smoosh all certs together? Does this compare similarity of two ONET certs? What to do after that?
#It seems like somewhere in the match_certs function, this is dealt with - this is not the case
#target is matched, source is matched to
sources = data.frame(
  entity = c(onet$Certifying.Organization, stw$Awarding.Entity),
  certificate = c(onet$Certification.Name, stw$Credential) #take out stw stuff if stw is not the source
)
sources = sources[!duplicated(sources$certificate),]
ctm = lma_weight(lma_dtm(sources$certificate, numbers = TRUE)) #this makes the "sources" i.e. the credentials in VA and onet rows, and unique words columns

# target

#bgt = read.csv('bgt_credentials.csv')
colnames(bgt)[2] = 'Raw'

#
# map certification names
#

# separate certificates by certifying entity
#ELK question: this is where we could do by soc code when combine SOC and BGT, right? Follow this same code?
certifications = split(sources$certificate, sources$entity) #divides the certificates into |entity| groups, makes a list
#where each index is defined by an entity and following it are all the associated certificates

# make an entity-term matrix, including entity name
#can bring in a generic cert per entity (or per soc for bgt)
etm = lma_dtm(
  vapply(names(certifications), function(entity) paste(entity, certifications[[entity]], collapse = ' '), ''),
  numbers = TRUE
)

# make versions of term for partial matching
#ELK question: is this stemming?
suffixes = "(?:'?s|ed|or|er|ing|ive)$" #may be some issues here, check it out. or may be a bad one to keep
#ELK question: why use sub instead of gsub here? Compare help texts
cert_terms_stem = sub(suffixes, '', colnames(etm))
#ELK question: could throw stop words in right here?

# calculate terms weights based on distinctiveness
# -- if a term is associated with several certificates, it should get less weight
cert_terms_weight = (1 - (colSums(etm != 0) - 1) / length(certifications)) ^ 3
#could tweak above line of code

#
# simple matching algorithm
#

match_cert = function(certificates, verbose = FALSE){
  # process targets in the same way was source
  raw_ctm = lma_dtm(certificates, numbers = TRUE)

  do.call(rbind, lapply(seq_along(certificates), function(certificate){
    terms = raw_ctm[certificate, raw_ctm[certificate,] != 0, drop = FALSE]

    # the goal will be to associate entities and specific certificates with each entry
    res = data.frame(entity = '', certificate = '', confidence_entry = 0, confidence_cert = 0)

    # see if there are any recognized terms
    recognized = colnames(terms) %in% colnames(etm)

    # keep track of transformations to adjust confidence
    mismatch = terms[1,]
    mismatch[] = 0

    # try and match unrecognized terms
    #could add conditions down here or take some out depending on if we want to change distances, lowercase, not stem, etc.
    for(i in which(!recognized)){
      term = colnames(terms)[i]

      # see if a match can be made by removing suffixes
      if(sub(suffixes, '', term) %in% cert_terms_stem){
        colnames(terms)[i] = colnames(etm)[which(cert_terms_stem == sub(suffixes, '', term))[1]]
        recognized[i] = TRUE
        mismatch[i] = -.4
        if(verbose) message('suffix: ', term, ' -> ', sub(suffixes, '', term), ' -> ', colnames(terms)[i])

      # see if a unique match can be made by making a single string edit
      }else if(any(d <- stringdist(term, colnames(etm)) == 1) && sum(d) == 1){
        colnames(terms)[i] = colnames(etm)[which(d)]
        recognized[i] = TRUE
        mismatch[i] = -.6
        if(verbose) message('character: ', term, ' -> ', colnames(etm)[which(d)])
      }
    }

    if(any(recognized)){
      #actual matching happening here in this
      matched = cert_terms_weight[colnames(terms)[recognized]] + mismatch[recognized]

      # assign entries based on the entry with the most and highest weighted matches (weighting could be played with)
      score = as.numeric((etm[, names(matched), drop = FALSE] != 0) %*% matched)
      res$confidence_entry = max(score)
      cert = certifications[which.max(score)]
      res$entity = names(cert)

      # assign certificate based on binary match
      sctm = ctm[sources$entity == res$entity, colnames(ctm) %in% names(matched), drop = FALSE]
      score = as.numeric(sctm %*% matched[colnames(sctm)])
      res$confidence_cert = max(score)
      res$certificate = cert[[1]][which.max(score)]
      #ctm matrix - can weight based on how many words show up in the certification
    }

    res
  }))
}

# matching all raw certificates
matched_certs = match_cert(bgt$Raw, TRUE)

# adjusting confidence scores, and writing file
# ELK question: is certificate field the "chosen" ONET cert? If I wanted to say some weren't valid at all, would
# I just choose a cutoff? Would that be under the confidence column? What's a typical cutoff?
matched_certs$confidence_entry = matched_certs$confidence_entry / max(matched_certs$confidence_entry)
matched_certs$confidence_cert = matched_certs$confidence_cert / max(matched_certs$confidence_cert)
#ELK question: confidence is just part confidence of "entry" and of cert. What is "entry?" Can we reweight?
#Vicki has similar question. Micah says this is pretty task specific to choose weighting system. Should consider more sensitively
#weighing terms. 
matched_certs$confidence = matched_certs$confidence_entry * matched_certs$confidence_cert
matched_certs$confidence = matched_certs$confidence / max(matched_certs$confidence)
write.csv(cbind(bgt[, -1], matched_certs), 'matched_certs.csv', row.names = FALSE)
#Vicki suggestion: descriptives on confidences - is there a natural break? could illuminate a cutoff
#add a first pass - match exact names
