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

let empty = E

let rec mem cmp x = function
  | E -> false
  | T { color = _; left; key; value = _; right } ->
      let c = cmp x key in
      if c < 0 then mem cmp x left
      else if c > 0 then mem cmp x right
      else true

let rec lookup cmp x = function
  | E -> None
  | T { color = _; left; key; value; right } ->
      let c = cmp x key in
      if c < 0 then lookup cmp x left
      else if c > 0 then lookup cmp x right
      else Some value

let balance = function
  | B, T { color = R; left = T { color = R; left = a; key = xk; value = xv; right = b };
            key = yk; value = yv; right = c }, zk, zv, d
  | B, T { color = R; left = a; key = xk; value = xv;
            right = T { color = R; left = b; key = yk; value = yv; right = c } }, zk, zv, d
  | B, a, xk, xv, T { color = R; left = T { color = R; left = b; key = yk; value = yv; right = c };
                      key = zk; value = zv; right = d }
  | B, a, xk, xv, T { color = R; left = b; key = yk; value = yv;
                      right = T { color = R; left = c; key = zk; value = zv; right = d } } ->
      T { color = R;
          left = T { color = B; left = a; key = xk; value = xv; right = b };
          key = yk; value = yv;
          right = T { color = B; left = c; key = zk; value = zv; right = d } }
  | col, a, k, v, b ->
      T { color = col; left = a; key = k; value = v; right = b }

let insert cmp k v s =
  let rec ins = function
    | E -> T { color = R; left = E; key = k; value = v; right = E }
    | T { color; left; key; value; right } ->
        let c = cmp k key in
        if c < 0 then balance (color, ins left, key, value, right)
        else if c > 0 then balance (color, left, key, value, ins right)
        else T { color; left; key; value = v; right }
  in
  match ins s with
  | T t -> T { t with color = B }
  | E -> E

let rec to_inorder = function
  | E -> []
  | T { left; key; value; right; _ } ->
      (to_inorder left) @ ((key, value) :: to_inorder right)

let of_sorted kvs =
  let rec build n lst =
    if n = 0 then (E, lst)
    else
      let left_n = (n - 1) / 2 in
      let right_n = n - 1 - left_n in
      let (l, lst1) = build left_n lst in
      match lst1 with
      | [] -> (E, [])
      | (k, v) :: rest ->
          let (r, rest') = build right_n rest in
          (T { color = B; left = l; key = k; value = v; right = r }, rest')
  in
  fst (build (List.length kvs) kvs)

let remove_key cmp key t =
  let rec drop = function
    | [] -> []
    | (k, _) :: xs when cmp k key = 0 -> xs
    | x :: xs -> x :: drop xs
  in
  of_sorted (drop (to_inorder t))
