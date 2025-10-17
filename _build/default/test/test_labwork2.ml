open OUnit2
open QCheck

module IntOrd = struct
  type t = int
  let compare = Stdlib.compare
end

module D = Labwork2.RBDict.Make(IntOrd)

(* ---------- UNIT TESTS ---------- *)

let suite_unit : test list =
  [
    "add/find" >:: (fun _ ->
      let d = D.empty |> D.add 3 "x" |> D.add 1 "a" |> D.add 2 "b" in
      assert_equal (Some "a") (D.find_opt 1 d);
      assert_bool "mem 3" (D.mem 3 d);
      assert_equal 3 (D.size d));

    "remove" >:: (fun _ ->
      let d = D.empty |> D.add 1 "a" |> D.add 2 "b" |> D.remove 1 in
      assert_equal None (D.find_opt 1 d);
      assert_equal (Some "b") (D.find_opt 2 d));

    "map/filter" >:: (fun _ ->
      let d = D.empty |> D.add 1 10 |> D.add 2 20 |> D.map (fun x -> x + 1) in
      let d' = D.filter (fun k _ -> k = 2) d in
      assert_equal (Some 11) (D.find_opt 1 d);
      assert_equal None (D.find_opt 1 d');
      assert_equal (Some 21) (D.find_opt 2 d'));

    "monoid identity" >:: (fun _ ->
      let a = D.empty |> D.add 1 "a" in
      let b = D.empty |> D.add 2 "b" in
      let u = D.union a b in
      assert_bool "contains 1" (D.mem 1 u);
      assert_bool "contains 2" (D.mem 2 u);
      assert_bool "left identity" (D.equal String.equal a (D.union D.empty a));
      assert_bool "right identity" (D.equal String.equal a (D.union a D.empty)));
  ]

(* ---------- PROPERTY TESTS ---------- *)

let gen_pair = QCheck.(pair small_nat string)
let arb_assoc_list = QCheck.(list gen_pair)

let as_dict lst = List.fold_left (fun d (k,v) -> D.add k v d) D.empty lst

let prop_union_assoc =
  Test.make ~name:"union associative"
    QCheck.(triple arb_assoc_list arb_assoc_list arb_assoc_list)
    (fun (a,b,c) ->
      let da,db,dc = as_dict a, as_dict b, as_dict c in
      D.equal String.equal
        (D.union (D.union da db) dc)
        (D.union da (D.union db dc)))

let prop_empty_identity =
  Test.make ~name:"empty identity"
    arb_assoc_list
    (fun a ->
      let d = as_dict a in
      D.equal String.equal d (D.union d D.empty)
      && D.equal String.equal d (D.union D.empty d))

let prop_lookup_after_add =
  Test.make ~name:"find after add"
    gen_pair
    (fun (k,v) ->
      match D.find_opt k (D.add k v D.empty) with
      | Some v' -> String.equal v v'
      | None -> false)

(* ---------- MAIN ---------- *)

let () =
  let q : test list =
    [ prop_union_assoc; prop_empty_identity; prop_lookup_after_add ]
    |> List.map QCheck_ounit.to_ounit2_test
  in
  run_test_tt_main ("all" >::: (suite_unit @ q))
