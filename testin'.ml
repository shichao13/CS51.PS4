(* belongs near line 500! *)

let test_is_empty () =
      let emptyset = empty in
      let x = C.generate () in
      let x2 = C.generate_lt x () in
      let x3 = C.generate_lt x2 () in
      let x4 = C.generate_lt x3 () in
      assert (is_empty emptyset = true)
      assert (is_empty x = false)
      assert (is_empty x2 = false)
      assert (is_empty x3 = false)
      assert (is_empty x4 = false)
      assert (is_empty (add x emptyset) = false)
      assert (is_empty (add x3 (add x2 emptyset)) = false)

  let test_add () =
      let emptyset = empty in
      let x = C.generate () in
      let x2 = C.generate_gt x () in
      let x3 = C.generate_gt x2 () in
      let x4 = C.generate_gt x3 () in
      assert (add (take add (x emptyset)) = add (x emptyset))
      assert (is_empty (add x3 (add x2 (add x emptyset))) = false)

  let test_take () =
      let emptyset = empty in
      let x = C.generate () in
      let x2 = C.generate_lt x () in
      let x3 = C.generate_lt x2 () in
      let x4 = C.generate_lt x3 () in
      let all_added = add x2 (add x3 (add x4 (add x emptyset))) in
      let (y, almost) = take (all_added) in
      let (y2, almost2) = take (almost) in
      let (y3, almost3) = take (almost2) in
      let (y4, nempty) = take (almost3) in
      assert (x = y)
      assert (x2 = y2)
      assert (x3 = y3)
      assert (x4 = y4)
      assert (is_empty nempty = true)
      assert (is_empty almost2 = false)

  let run_tests () =
    test_is_empty ();
    test_add ();
    test_take ();
    ()

(* Belongs near line 640! *)

let test_is_empty () =
      let emptyset = empty in
      let x = C.generate () in
      let x2 = C.generate_lt x () in
      let x3 = C.generate_lt x2 () in
      let x4 = C.generate_lt x3 () in
      assert (is_empty emptyset = true)
      assert (is_empty x = false)
      assert (is_empty x2 = false)
      assert (is_empty x3 = false)
      assert (is_empty x4 = false)
      assert (is_empty (add x emptyset) = false)
      assert (is_empty (add x3 (add x2 emptyset)) = false)

  let test_add () =
      let emptyset = empty in
      let x = C.generate () in
      let x2 = C.generate_gt x () in
      let x3 = C.generate_gt x2 () in
      let x4 = C.generate_gt x3 () in
      assert (add (take add (x emptyset)) = add (x emptyset))
      assert (add x emptyset = Tree (Leaf x))
      assert (add x2 (add x emptyset) = Tree (OneBranch (x, x2)))
      assert (add x3 (add x2 (add x emptyset)) = Tree (TwoBranch (Leaf x2, x, Leaf x3)))
      assert (is_empty (add x3 (add x2 (add x emptyset))) = false)

  let test_take () =
      let emptyset = empty in
      let x = C.generate () in
      let x2 = C.generate_lt x () in
      let x3 = C.generate_lt x2 () in
      let x4 = C.generate_lt x3 () in
      let all_added = add x2 (add x3 (add x4 (add x emptyset))) in
      let (y, almost) = take (all_added) in
      let (y2, almost2) = take (almost) in
      let (y3, almost3) = take (almost2) in
      let (y4, nempty) = take (almost3) in
      assert (x4 = y)
      assert (x3 = y2)
      assert (x2 = y3)
      assert (x = y4)
      assert (is_empty nempty = true)
      assert (is_empty almost2 = false)
      assert (almost3 = Tree (Leaf x))
      assert (all_added = Tree (OneBranch (x2, x4), x, Leaf x3))

  let run_tests () =
    test_is_empty ();
    test_add ();
    test_take ();
    ()