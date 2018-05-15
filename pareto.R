#' Create a Voting Scenario by Pareto Distribution  
#' @author Leo Seo, \email{leo@eosnodeone.io}
#'
#' @examples
#' prt1 <- pareto(
#'		top21_r = 4,
#'		rest_r = 1 ) 
#'
#' plot(1:121, prt1, type = 'l')
#' @export
#' df_pareto <- as.data.frame(prt1)
#' write.csv(df_pareto, file = "pareto_dist.csv", fileEncoding = "macroman")
#'
#' \dontrun{
#' install.packages("VGAM") 
#' library(VGAM)

pareto <- function(
	top21_r = 4, ## the ratio of the number of votes of top21
	rest_r = 1 ## the ratio of the number of votes of the rest
){
  if ( top21_r == rest_r ) {
  print('top21_r and rest_r may not be same')
  }
  else {


 # initialize variables
 	 num_rank <- 0

 # initialize vector of quantiles
 	 seq_1 = 1
	 seq_121 = (121*(top21_r/rest_r) - 100)/21
	 pvec <- seq(1, seq_121 , length.out = 122)

 # set a pareto index from intput values
 	 alpha <- log( (top21_r + rest_r) / rest_r) / log(top21_r / rest_r)

 # get a cummulative pareto distribution
	 cum_pareto_dist = ppareto(pvec, scale = 1, alpha, lower.tail = TRUE, log.p = FALSE)

 # make a probability mass function of pareto distribution
 	 pareto_dist <- vector()
 	 for (i in 1:121) {
	  	pareto_dist_value <- 0
	  	num_rank <- num_rank + 1
		pareto_dist_value <- cum_pareto_dist[i+1] - cum_pareto_dist[i]
		pareto_dist <- c(pareto_dist, pareto_dist_value)
  	}
  return(pareto_dist)
  }	
}
