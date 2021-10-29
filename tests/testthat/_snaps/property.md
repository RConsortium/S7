# property: does not use partial matching

    Can't find property <range>@st

# @: does not use partial matching

    Can't find property <range>@st

# @: falls back to `base::@` for non-R7 objects

    Code
      "foo"@blah
    Error <simpleError>
      trying to get slot "blah" from an object of a basic class ("character") with no slots
    Code
      NULL@blah
    Error <simpleError>
      trying to get slot "blah" from an object of a basic class ("NULL") with no slots

