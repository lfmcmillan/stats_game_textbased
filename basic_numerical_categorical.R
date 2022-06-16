generate_variable_type_question_set <- function(level=2) {
    var_types <- read.csv("variable_types.csv")
    var_types <- var_types[var_types$min_level <= level,]

    nvar <- 4
    var_idxs <- sample(1:nrow(var_types),nvar,replace=FALSE)

    qna <- list()
    for (i in 1:nvar) {
        var <- var_types$variable[var_idxs[i]]
        type <- var_types$type[var_idxs[i]]

        question <- paste0("Is '", var, "' a numerical or a categorical variable?")

        if (type == "numerical") {
            answers <- "numerical"
            distractors <- "categorical"
        } else {
            answers <- "categorical"
            distractors <- "numerical"
        }
        qna[[i]] <- list(question=question, answers=answers,
                         distractors=distractors)
    }

    list(display="",qna=qna, shuffle=FALSE)
}