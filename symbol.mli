type symbol
val symbol : string -> symbol
val name : symbol -> string

type 'a table
val empty : 'a table
val get : 'a table -> symbol -> 'a option
val put : 'a table -> symbol -> 'a -> 'a table
