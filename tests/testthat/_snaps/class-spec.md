# as_class gives informative errors

    Code
      as_class(letters)
    Error <simpleError>
      Can't convert `letters` to a valid class. Class specification must be an R7 class object, the result of `new_S3_class()`, an S4 class object, or a base class, not a <character>.
    Code
      as_class(1:5)
    Error <simpleError>
      Can't convert `1:5` to a valid class. Class specification must be an R7 class object, the result of `new_S3_class()`, an S4 class object, or a base class, not a <integer>.

