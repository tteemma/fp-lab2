type color = R | B

type ('k,'v) node =
  | E
  | T of color * ('k,'v) node * 'k * 'v * ('k,'v) node

let empty = E

let rec mem cmp x = function
  | E -> false
  | T(_, a, y, _, b) ->
      let c = cmp x y in
      if c < 0 then mem cmp x a
      else if c > 0 then mem cmp x b
      else true

let rec lookup cmp x = function
  | E -> None
  | T(_, a, y, v, b) ->
      let c = cmp x y in
      if c < 0 then lookup cmp x a
      else if c > 0 then lookup cmp x b
      else Some v

let balance = function
  | B, T(R, T(R,a,xk,xv,b), yk, yv, c), zk, zv, d
  | B, T(R, a, xk, xv, T(R,b,yk,yv,c)), zk, zv, d
  | B, a, xk, xv, T(R, T(R,b,yk,yv,c), zk, zv, d)
  | B, a, xk, xv, T(R, b, yk, yv, T(R,c,zk,zv,d)) ->
      T(R, T(B,a,xk,xv,b), yk, yv, T(B,c,zk,zv,d))
  | col, a, k, v, b -> T(col, a, k, v, b)

let insert cmp k v s =
  let rec ins = function
    | E -> T(R,E,k,v,E)
    | T(col, a, xk, xv, b) ->
        let c = cmp k xk in
        if c < 0 then balance (col, ins a, xk, xv, b)
        else if c > 0 then balance (col, a, xk, xv, ins b)
        else T(col, a, xk, v, b)
  in
  match ins s with
  | T(_, a, xk, xv, b) -> T(B, a, xk, xv, b)
  | E -> E

let rec to_inorder = function
  | E -> []
  | T(_, l, k, v, r) -> (to_inorder l) @ ((k,v) :: to_inorder r)

let of_sorted kvs =
  let rec build n lst =
    if n = 0 then (E, lst)
    else
      let left_n = (n - 1) / 2 in
      let right_n = n - 1 - left_n in
      let (l, lst1) = build left_n lst in
      match lst1 with
      | [] -> (E, [])
      | (k,v)::rest ->
          let (r, rest') = build right_n rest in
          (T(B, l, k, v, r), rest')
  in
  fst (build (List.length kvs) kvs)

let remove_key cmp key t =
  let rec drop = function
    | [] -> []
    | (k,_)::xs when cmp k key = 0 -> xs
    | x::xs -> x :: drop xs
  in
  of_sorted (drop (to_inorder t))
