exception TODO
type team =
    Korea
  | France
  | Usa
  | Brazil
  | Japan
  | Nigeria
  | Cameroon
  | Poland
  | Portugal
  | Italy
  | Germany
  | Norway
  | Sweden
  | England
  | Argentina
type tourna = LEAF of team | NODE of tourna * tourna
val team_to_string : team -> string
val parenize : tourna -> string
val drop_h : tourna * team -> tourna
val drop : tourna -> team -> string
