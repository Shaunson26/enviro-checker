#' Custom selectizeInput
#' 
#' Create a select list that can be used to choose a single or multiple items from a list of values.
#' The 'custom' part has a number of options set.
#' 
#' @param id The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
selectizeInputCustom <- function(id, label, placeholder) {
  
  score_function <-
    "function scoreFilter(search) {

    this.getScoreFunction = function(query) { return this.sifter.getScoreFunction(query, $.extend(this.getSearchOptions(), {respect_word_boundaries: true}));	}

    var score = this.getScoreFunction(search);

    return function onScore(item) {
            var result = score(item);
            return result;
        }
    }}"
  
  score_function <-
    "function(search)
    {
      var score = this.getScoreFunction(search);
      return function(item)
      {
        return item.label
        .toLowerCase()
        .startsWith(search.toLowerCase()) ? 1 : 0;
      };
    }"
  
  if (missing(placeholder)){
    placeholder = sprintf('input a %s', tolower(label))
  }
  
  selectizeInput(
    inputId = id,
    label = p(label),
    choices = NULL,
    options = list(placeholder = placeholder,
                   selectOnTab = TRUE,
                   #maxOptions = 10,
                   score = base::I(score_function))
  )
}