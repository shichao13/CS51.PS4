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

(* Problem 4 Testing *)
  type balance = Even | Odd

  type tree =   TwoBranch of balance * elt * tree * tree
              | OneBranch of elt * elt
              | Leaf of elt

  type queue = Empty | Tree of tree

  let empty = Empty

  let add (e : elt) (q : queue) : queue =
    let rec add_to_tree (e : elt) (t : tree) : tree =
      match t with
      | Leaf e1 ->
        if e >= e1 then 
          OneBranch (e1, e)
        else
          OneBranch (e, e1)
      | OneBranch(e1, e2) ->
        if e >= e1 then
          TwoBranch (Even, e1, Leaf e2, Leaf e)
        else
          TwoBranch (Even, e, Leaf e2, Leaf e1)
      | TwoBranch(Even, e1, t1, t2) ->
        if e >= e1 then
         TwoBranch(Odd, e1, add_to_tree e t1, t2)
        else
         TwoBranch(Odd, e, add_to_tree e1 t1, t2)
      | TwoBranch(Odd, e1, t1, t2) ->
        if e >= e1 then
          TwoBranch(Even, e1, t1, add_to_tree e t2)
        else
          TwoBranch(Even, e, t1, add_to_tree e1 t2)
    in
    match q with
    | Empty -> Tree (Leaf e)
    | Tree t -> Tree (add_to_tree e t)

  let get_top (t : tree) : elt =
    match t with
    | Leaf e -> e
    | OneBranch (e, _) -> e
    | TwoBranch (_, e, _, _) -> e

  let child_compare (e1 : elt) (e2 : elt) (e3 : elt) : int =
    if e1 > e2 then
      if e2 > e3 then 3 else 2
    else
      if e1 > e3 then 3 else 1

  let top_switch (node : elt) (t : tree) : tree =
    match t with
    | Leaf e -> Leaf node
    | OneBranch (e1, e2) -> OneBranch (node, e2)
    | TwoBranch (bal, e, l, r) -> TwoBranch (bal, node, l, r)

  let rec fix (t : tree) : tree =
    match t with
    | Leaf e -> t
    | OneBranch (p, c) -> if p > c then OneBranch(c,p) else t
    | TwoBranch (bal, node, l, r) ->
      (match child_compare node (get_top l) (get_top r) with
      | 1 -> t
      | 2 -> TwoBranch(bal, (get_top l), fix (top_switch node l), r)
      | _ -> TwoBranch(bal, (get_top r), l, fix (top_switch node r)))

  let get_odd_side (l : tree) (r : tree) : bool =
    match l with
    (* If leaf, the other side is one branch (we'll never run into a starting
    tree. So, we just set it to the right side of the tree. *)
    | Leaf _ -> true
    | OneBranch (_, _) -> false
    | TwoBranch (bal, _, _, _) ->
      (match bal with
      | Odd -> false
      | Even -> true)

  let rec get_last (t : tree) : elt * queue =
    match t with
    | Leaf e -> (e, Empty)
    | OneBranch (p, c) -> (c, Tree(Leaf p))
    | TwoBranch (bal, node, l, r) ->
      (match bal with
      | Even ->
        let (bot, myTreeQL) = get_last r in
        (match myTreeQL, l with
        | Empty, Leaf lea -> (bot, Tree (OneBranch (node, lea)))
        | Empty, OneBranch (e1, e2) -> (bot, Tree (TwoBranch(Even, node, Leaf e1, Leaf e2)))
        | Empty, _ -> failwith "Unbalanced Tree!"
        | Tree tre, _ -> (bot, Tree (TwoBranch(bal, node, l, tre))))
      | Odd  -> 
        if get_odd_side l r then
          let (bot, myTreeQL) = get_last r in
          (match myTreeQL, l with
          | Empty, Leaf lea -> (bot, Tree (OneBranch (node, lea)))
          | Empty, OneBranch (e1, e2) -> (bot, Tree (TwoBranch(Even, node, Leaf e1, Leaf e2)))
          | Empty, _ -> failwith "Unbalanced Tree!"
          | Tree tre, _ -> (bot, Tree (TwoBranch(bal, node, l, tre))))
        else
          let (bot, myTreeQL) = get_last l in
          (match myTreeQL, r with
          | Empty, Leaf lea -> (bot, Tree (OneBranch (node, lea)))
          | Empty, OneBranch (e1, e2) -> (bot, Tree (TwoBranch(Even, node, Leaf e1, Leaf e2)))
          | Empty, _ -> failwith "Unbalanced Tree!"
          | Tree tre, _ -> (bot, Tree (TwoBranch(bal, node, tre, r)))))

  let extract_tree (q : queue) : tree =
    match q with
    | Empty -> failwith "No Tree D:"
    | Tree t -> t