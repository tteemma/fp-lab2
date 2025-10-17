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
    "insert & find" >:: (fun _ ->
      let d = D.empty |> D.add 1 "one" |> D.add 2 "two" in
      assert_equal (Some "one") (D.find_opt 1 d);
      assert_equal (Some "two") (D.find_opt 2 d));

    "remove" >:: (fun _ ->
      let d = D.empty |> D.add 1 "a" |> D.add 2 "b" |> D.remove 1 in
      assert_bool "key 1 removed" (not (D.mem 1 d));
      assert_bool "key 2 present" (D.mem 2 d));

    "map & filter" >:: (fun _ ->
      let d = D.empty |> D.add 1 10 |> D.add 2 20 in
      let d' = D.map (fun x -> x + 1) d in
      let d'' = D.filter (fun k _ -> k = 1) d' in
      assert_equal (Some 11) (D.find_opt 1 d'');
      assert_equal None (D.find_opt 2 d''));

    "folds" >:: (fun _ ->
      let d = D.empty |> D.add 1 1 |> D.add 2 2 |> D.add 3 3 in
      let sum = D.fold_left (fun acc _ v -> acc + v) 0 d in
      assert_equal 6 sum);
  ]

(* ---------- PROPERTY TESTS ---------- *)

let gen_pair = QCheck.(pair small_nat string)
let arb_assoc_list = QCheck.(list gen_pair)

let as_dict lst = List.fold_left (fun d (k,v) -> D.add k v d) D.empty lst

let prop_left_id =
  Test.make ~name:"append left identity"
    arb_assoc_list
    (fun a ->
      let da = as_dict a in
      D.equal String.equal (D.union D.empty da) da)

let prop_right_id =
  Test.make ~name:"append right identity"
    arb_assoc_list
    (fun a ->
      let da = as_dict a in
      D.equal String.equal (D.union da D.empty) da)

let prop_assoc =
  Test.make ~name:"append associativity"
    QCheck.(triple arb_assoc_list arb_assoc_list arb_assoc_list)
    (fun (a,b,c) ->
      let da, db, dc = as_dict a, as_dict b, as_dict c in
      D.equal String.equal
        (D.union (D.union da db) dc)
        (D.union da (D.union db dc)))

let prop_map_comp =
  Test.make ~name:"map composition"
    QCheck.(list (pair small_nat small_nat))
    (fun lst ->
      let d = as_dict lst in
      let f x = x + 1 and g x = x * 2 in
      let left = D.map (fun x -> f (g x)) d
      and right = D.map f (D.map g d) in
      D.equal (=) left right)

let suite_props : test list =
  [
    QCheck_ounit.to_ounit2_test prop_left_id;
    QCheck_ounit.to_ounit2_test prop_right_id;
    QCheck_ounit.to_ounit2_test prop_assoc;
    QCheck_ounit.to_ounit2_test prop_map_comp;
  ]

(* ---------- MAIN RUNNER ---------- *)

let () =
  run_test_tt_main
    ("Labwork2 Dict" >::: (suite_unit @ suite_props))
