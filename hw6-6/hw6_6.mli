exception TODO
type metro =
    STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string
val stn_list : metro -> string list
val area_list : metro -> string list
val check_in : string list -> string list -> bool
val checkMetro : metro -> bool
