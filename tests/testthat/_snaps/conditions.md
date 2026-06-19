# catches invalid conditions

    Code
      validate_condition(1)
    Output
      [1] "Underlying data must be a <list>"
    Code
      validate_condition(structure(list(), class = "condition"))
    Output
      [1] "`message` must be a single string"
    Code
      validate_condition(structure(list(message = 1), class = "condition"))
    Output
      [1] "`message` must be a single string"

