module IntOrd = struct type t = int let compare = Stdlib.compare end
module D = Labwork2.RBDict.Make(IntOrd)

let () =
  let d = D.empty |> D.add 2 "two" |> D.add 1 "one" |> D.add 3 "three" in
  match D.find_opt 2 d with
  | Some s -> Printf.printf "2 -> %s\n" s
  | None -> Printf.printf "2 not found\n"
