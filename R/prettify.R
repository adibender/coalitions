#' Make prettier labels for parties
#' 
#' @rdname prettify
#' @param x a Vector of parties to prettify. 
#' @param current A vector of current values \code{x} can take 
#' @param new A vector of labels that \code{x} should be transformed into.
#' @keywords internal 
#' @export
prettify_parties <- function(
	x,
	current = c("cdu", "spd", "gruene", "fdp", "linke", "piraten", "afd", 
		"sonstige"), 
	new = c("Union", "SPD", "Gr\u00fcne", "FDP", "Die Linke", "Piraten", "AfD", 
		"Sonstige")) {

	factor(x, levels=current, labels=new)

}

#' Make prettier labels for coalitoins
#' 
#' @rdname prettify
#' @inherit prettify_parties
#' @param x A vector of coalitions to prettify.
#' @keywords internal 
#' @export
prettify_coalitions <- function(
	x, 
	current = c("cdu", "cdu_fdp", "cdu_gruene", "cdu_fdp_gruene", "spd", 
		"linke_spd", "gruene_linke_spd"),
	new=c(
		"Union", "Union - FDP", 
		"Union - Gr\u00fcne", 
		"Union - FDP - Gr\u00fcne", 
		"SPD", 
		"SPD - Die Linke", 
		"SPD - Die Linke - Gr\u00fcne"))	 {

	factor(x, levels=current, labels=new)

}