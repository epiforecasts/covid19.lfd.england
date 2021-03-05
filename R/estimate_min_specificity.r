##' Estimate minimum specificity from population testing
##'
##' Produces an estimate of minimum specificty under the assumption that of the tests identified only negatives as positives
##' @param positive vector of numbers; the number of positive tests in each sample
##' @param total vector of numbres; the total number of tests (positive and negative) in each sample
##' @param cutoff the cutoff for the estimate, i.e. the confidence level desired
##' @param samples number of values of specificity to sample
##' @param min_specificity minimum specificity to test
##' @param max_specificity maximum specificity to test
##' @param prevalence background prevalence; if >0 then it is assumed that
##' sensitivity is 0, i.e. all positives are still false positives and some of
##' the negatives are false negatives
##' @return estimate of the lower bound of specificity at
##' the desired confidence level
##' @author Sebastian Funk
##' @export
estimate_min_specificity <- function(positive, total, cutoff = 0.95,
                                     samples = 1000,
                                     min_specificity = 0.997,
                                     max_specificity = 1,
                                     prevalence = 0) {

  true_negative <- round(pmax(positive, total * (1 - prevalence)))

  ## create data frame with false test positive and true test negative
  dta <- tibble(positive = positive, negative = true_negative - positive)
  spec <- dta %>%
    expand_grid(specificity =
                  seq(min_specificity, max_specificity,
                      length.out = samples)) %>%
    ## probability of seeing positives given specificty is at most x
    ## (where at the maximum all positives are false positives)
    mutate(p = pbeta(1 - specificity, positive + 1,
                     negative + 1, lower.tail = FALSE)) %>%
    group_by(specificity) %>%
    ## overall probability that specificity is at most x
    summarise(probability = 1 - prod(p), .groups = "drop") %>%
    ungroup()

  min_spec <- spec %>%
    ## get probability that specificity is at least x
    filter(probability > cutoff) %>%
    summarise(min_spec = max(specificity)) %>%
    .$min_spec

  return(min_spec)
}
