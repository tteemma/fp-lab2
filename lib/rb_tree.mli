type color = R | B

type ('k, 'v) node =
  | E
  | T of {
      color : color;
      left : ('k, 'v) node;
      key : 'k;
      value : 'v;
      right : ('k, 'v) node;
    }

val empty : ('k, 'v) node
val mem : ('k -> 'k -> int) -> 'k -> ('k, 'v) node -> bool
val lookup : ('k -> 'k -> int) -> 'k -> ('k, 'v) node -> 'v option
val insert : ('k -> 'k -> int) -> 'k -> 'v -> ('k, 'v) node -> ('k, 'v) node
val remove_key : ('k -> 'k -> int) -> 'k -> ('k, 'v) node -> ('k, 'v) node
val of_sorted : ('k * 'v) list -> ('k, 'v) node
val to_inorder : ('k, 'v) node -> ('k * 'v) list
