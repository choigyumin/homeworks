exception TODO
type ae =
    CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list
val diff : ae -> string -> ae
