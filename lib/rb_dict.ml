open Rb_tree

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val find_opt : key -> 'a t -> 'a option
  val size : 'a t -> int
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val fold_left : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val fold_right : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val empty_monoid : 'a t
  val union : 'a t -> 'a t -> 'a t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module Make (Ord : OrderedType) = struct
  type key = Ord.t
  type 'a t = (key, 'a) Rb_tree.node

  let empty = Rb_tree.empty
  let empty_monoid = empty

  let is_empty = function
    | E -> true
    | _ -> false

  let add k v t = insert Ord.compare k v t
  let remove k t = remove_key Ord.compare k t
  let mem k t = mem Ord.compare k t
  let find_opt k t = lookup Ord.compare k t
  let size t = List.length (to_inorder t)

  (* ---------- исправленные fold'ы ---------- *)

  let fold_left f acc t =
    let rec go acc = function
      | E -> acc
      | T { left; key; value; right; _ } ->
          go (f (go acc left) key value) right
    in
    go acc t

  let fold_right f t acc =
    let rec go acc = function
      | E -> acc
      | T { left; key; value; right; _ } ->
          f key value (go (go acc right) left)
    in
    go acc t

  (* ---------- остальные функции ---------- *)

  let map g =
    let rec go = function
      | E -> E
      | T { color; left; key; value; right } ->
          T { color; left = go left; key; value = g value; right = go right }
    in
    go

  let filter p t =
    fold_right (fun k v acc -> if p k v then add k v acc else acc) t empty

  let union a b =
    fold_left (fun acc k v -> add k v acc) a b

  let equal eq a b =
    let rec loop la lb =
      match la, lb with
      | [], [] -> true
      | (ka, va) :: ta, (kb, vb) :: tb ->
          Ord.compare ka kb = 0 && eq va vb && loop ta tb
      | _ -> false
    in
    loop (to_inorder a) (to_inorder b)
end
