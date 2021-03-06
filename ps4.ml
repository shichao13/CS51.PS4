(* PS4
 * CS51 Spring 2014
 * Author(s): Michael Ma, Allen Chen
 *)

(* NOTE: Please read (and understand) all of the comments in this file! *)

open Core.Std

(* Things related to the TreeQueue module in this file are commented out
 * because this file would not compile otherwise. Please uncomment them as you
 * get to them. *)

exception ImplementMe

type order = Equal | Less | Greater

(*****************************************************************************)
(*                              Part 1.5                                     *)
(*****************************************************************************)

(* Please read motivation.ml for some background on why we use functors here *)

(*****************************************************************************)
(*                               Part 2                                      *)
(*****************************************************************************)

(* A better signature for a binary tree, avoiding the comparison function
 * found in motivation.ml. *)
module type BINTREE =
sig
  exception EmptyTree
  exception NodeNotFound

  (* The type of an element in the tree *)
  type elt

  (* What this type actually looks like is left up to the
   * particular BINTREE implementation (i.e. the struct) *)
  type tree

  (* Returns an empty tree *)
  val empty : tree

  (* Search a binary tree for the given value. *)
  val search : elt -> tree -> bool

  (* Insert elt into tree *)
  val insert : elt -> tree -> tree

  (* Delete the given value from a binary tree.
   * May raise NodeNotFound exception. *)
  val delete : elt -> tree -> tree

  (* Return the minimum value of a binary tree.
   * May raise EmptyTree exception *)
  val getmin : tree -> elt

  (* Return the maximum value of a binary tree.
   * May raise EmptyTree exception *)
  val getmax : tree -> elt

  (* Run invariant checks on the implementation of this binary tree.
   * May raise Assert_failure exception *)
  val run_tests : unit -> unit
end

(* A signature for a module which defines a type and
 * how to compare values of that type, as well as ways of generating
 * values of that type. *)
module type COMPARABLE =
sig
  type t
  val compare : t -> t -> order
  val to_string : t -> string

  (* See the testing.ml for an explanation of
   * what these "generate*" functions do, and why we included them in
   * this signature. *)

  (* Generate a value of type t *)
  val generate: unit -> t

  (* Generate a value of type t that is greater than the argument. *)
  val generate_gt: t -> unit -> t

  (* Generate a value of type t that is less than the argument. *)
  val generate_lt: t -> unit -> t

  (* Generate a value of type t that is between argument 1 and argument 2.
   * Returns None if there is no value between argument 1 and argument 2. *)
  val generate_between: t -> t -> unit -> t option
end

(* An example implementation of the COMPARABLE signature. In this
 * example, the value of the integer also gives its priority. *)
module IntCompare : COMPARABLE with type t=int =
struct
  type t = int

  let compare x y = if x < y then Less else if x > y then Greater else Equal

  let to_string = string_of_int

  let generate () = 0

  let generate_gt x () = x + 1

  let generate_lt x () = x - 1

  let generate_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end

(* Another example implementation that only uses the first part
 * of the tuple in comparisons. *)
module IntStringCompare : COMPARABLE with type t=(int * string) =
struct
  type t = int * string
  let compare (p1,_) (p2,_) =
    if p1 < p2 then Less else if p1 > p2 then Greater else Equal

  let to_string (p, s) = "(" ^ string_of_int p ^ "," ^ s ^ ")"


  let () = Random.self_init ()

  let generate () = (0, string_of_int (Random.int Int.max_value))

  let generate_gt (p,s) () = (p+1, s)

  let generate_lt (p,s) () = (p-1, s)

  let generate_between (p1,_) (p2,s2) () =
    let (lower, higher) = (min p1 p2, max p1 p2) in
    (* Reuse the string from the second argument in the output value *)
    if higher - lower < 2 then None else Some (higher - 1, s2)
end

(* BinSTree is a *functor*, which takes an argument C which is a module
 * that implements the COMPARABLE signature. BinSTree ultimately
 * must return a module which matches the BINTREE signature.
 * We can do further abstraction by specifying a signature for
 * the functor, but won't do that here.
 *
 * Now that we are passing in a COMPARABLE module, which separately
 * defines a type and comparison for that type, we can just implement something
 * matching BINTREE's signature in terms of that type and comparison function,
 * and can wait until later to actually say what that type and comparison
 * function are.
 *
 * Here, you'll fill in the implementation of a binary search tree. Unlike a
 * usual binary search tree, this implementation keeps a list with each node in
 * the tree that contains each instance of the value inserted into the tree. For
 * example, if the integer 3 is inserted into an Int BinSTree 5 times, then
 * there will be a node with [3;3;3;3;3] in the tree, and the node will only be
 * removed after 5 deletions on 3 (assuming no further intermediate insertions
 * of 3).
 *)

module BinSTree(C : COMPARABLE) : BINTREE with type elt = C.t =
struct
  (* Inside of here, you can use C.t to refer to the type defined in
   * the C module (which matches the COMPARABLE signature), and
   * C.compare to access the function which compares elements of type
   * C.t
   *)
  exception EmptyTree
  exception NodeNotFound

  (* Grab the type of the tree from the module C that's passed in
   * this is the only place you explicitly need to use C.t; you
   * should use elt everywhere else *)
  type elt = C.t

  (* One possible type for a tree *)
  type tree = Leaf | Branch of tree * elt list * tree

  (* Representation of the empty tree *)
  let empty = Leaf


(*>* Problem 2.0 *>*)

  (* Define a method to insert element x into the tree t.
   * The left subtree of a given node should only have "smaller"
   * elements than that node, while the right subtree should only have
   * "greater". Remember that "equal" elements should all be stored in
   * a list. *The most recently inserted elements should be at the front
   * of the list* (this is important for later).
   *
   * Hint: use C.compare. See delete for inspiration
   *)

  let rec insert (x : elt) (t : tree) : tree =
    match t with
    | Leaf -> Branch (Leaf, [x], Leaf)
    | Branch (left, elts, right) ->
      match elts with
      | [] -> Branch (left, [x], right)
      | hd::_ ->
        match C.compare x hd with
          | Less -> Branch ((insert x left), elts, right)
          | Equal -> Branch (left, x :: elts, right)
          | Greater -> Branch (left, elts, (insert x right))


(*>* Problem 2.1 *>*)

  (* Returns true if the element x is in tree t, else false *)
  (* Hint: multiple values might compare Equal to x, but
   * that doesn't necessarily mean that x itself is in the
   * tree.
   *)

  let rec searchlist (x : elt) (xs : elt list) : bool =
    match xs with
    | [] -> false
    | hd :: tl ->
      if hd = x 
      then true
      else (searchlist x tl)

  let rec search (x : elt) (t : tree) : bool =
    match t with
    | Leaf -> raise EmptyTree
    | Branch (left, elts, right) ->
      match elts with
      | [] -> false
      | hd::tl ->
        match C.compare x hd with
        | Less -> (search x left)
        | Greater -> (search x right)
        | Equal -> (searchlist x elts) 


  (* A useful function for removing the node with the minimum value from
   * a binary tree, returning that node and the new tree.
   *
   * Notice that the pull_min function is not defined in the signature BINTREE.
   * When you're working on a structure that implements a signature like
   * BINTREE, you are free to write "helper" functions for your implementation
   * (such as pull_min) that are not defined in the signature.  Note, however,
   * that if a function foo IS defined in a signature BAR, and you attempt to
   * make a structure satisfying the signature BAR, then you MUST define the
   * function foo in your structure.  Otherwise the compiler will complain that
   * your structure does not, in fact, satisfy the signature BAR (but you claim
   * that it does).
   * So, if it's in the signature, it needs to be in the structure.  But if
   * it's in the structure, it doesn't necessarily need to show up in the
   * signature.
   *)
  let rec pull_min (t : tree) : elt list * tree =
    match t with
    | Leaf -> raise EmptyTree
    | Branch (Leaf, v, r) -> (v, r)
    | Branch (l, v, r) -> let min, t' = pull_min l in (min, Branch (t', v, r))


  (* Removes an element from the tree. If multiple elements are in the list,
   * removes the one that was inserted first.  *)
  let rec delete (x : elt) (t : tree) : tree =
    match t with
    | Leaf -> raise NodeNotFound
    | Branch (l, lst, r) ->
      (* Reverse the list so that we pop off the last element in the list *)
      match List.rev lst with
      | [] -> failwith "Invalid tree: empty list as node"
      | hd::tl ->
        match C.compare x hd with
        | Less -> Branch (delete x l, lst, r)
        | Greater -> Branch (l, lst, delete x r)
        | Equal ->
          match tl with
          | _::_ -> Branch (l, List.rev tl, r)
          (* The list in the node is empty, so we have to
           * remove the node from the tree.  *)
          | [] ->
            match l, r with
            | Leaf, _ -> r
            | _, Leaf -> l
            | _ -> let v, r' = pull_min r in Branch (l,v,r')

(*>* Problem 2.2 *>*)

  (* Simply returns the minimum value of the tree t. If there are multiple
   * minimum values, it should return the one that was inserted first (note
   * that, even though the list might look like [3;3;3;3;3], you should
   * return the *last* 3 in the list. This is because we might pass in
   * a module to this functor that defines a type and comparison function
   * where each element in the list *is* distinct, but are Equal
   * from the perspective of the comparison function (like IntStringCompare).
   *
   * The exception "EmptyTree", defined within this module, might come in
   * handy. *)

  let rec getmin (t : tree) : elt =
    match t with
    | Leaf -> raise EmptyTree
    | Branch (Leaf, x, _) ->
      (match x with
      | [x'] -> x'
      | _ :: tl -> getmin (Branch (Leaf, tl, Leaf)))
    | Branch (left, _, _) -> getmin left


(*>* Problem 2.3 *>*)

  (* Simply returns the maximum value of the tree t. Similarly should
   * return the last element in the matching list. *)
  let rec getmax (t : tree) : elt =
    match t with
    | Leaf -> raise EmptyTree
    | Branch (_, x, Leaf) ->
      (match x with
      | [x] -> x
      | _ :: tl -> getmax (Branch (Leaf, tl, Leaf)))
    | Branch (_, _, right) -> getmax right

  let test_insert () =
    let x = C.generate () in
    let t = insert x empty in
    assert (t = Branch(Leaf, [x], Leaf));
    let t = insert x t in
    assert (t = Branch(Leaf, [x;x], Leaf));
    let y = C.generate_gt x () in
    let t = insert y t in
    assert (t = Branch(Leaf, [x;x], Branch(Leaf, [y], Leaf)));
    let z = C.generate_lt x () in
    let t = insert z t in
    assert (t = Branch(
                        Branch(Leaf, [z], Leaf),
                        [x;x],
                        Branch(Leaf, [y], Leaf)
                      ));
    (* Can add further cases here *)
    ()

  (* Insert a bunch of elements, and test to make sure that we
   * can search for all of them. *)
  let test_search () =
    let x = C.generate () in
    let t = insert x empty in
    assert (search x t);
    let order = [ true; false; true; true; true; false; false] in
    let full_tree, values_inserted =
      List.fold_right
        ~f:(fun current_order (tree_so_far, values_so_far) ->
          let prev_value =
            match values_so_far with
            | [] -> x
            | hd :: _ -> hd
          in
          let value =
            if current_order
            then C.generate_gt prev_value ()
            else C.generate_lt prev_value ()
          in
          insert value tree_so_far, value :: values_so_far
        ) ~init:(t, []) order
    in
    List.iter ~f:(fun value -> assert (search value full_tree)) values_inserted

  (* None of these tests are particularly exhaustive.
   * For instance, we could try varying the order in which we insert
   * values, and making sure that the result is still correct.
   * So, the strategy here is more to try to build up a reasonable degree
   * of coverage across the various code-paths, rather than it is to
   * test exhaustively that our code does the right thing on every single
   * possible input. *)
  let test_getmax () =
      let x = C.generate () in
      let x2 = C.generate_lt x () in
      let x3 = C.generate_lt x2 () in
      let x4 = C.generate_lt x3 () in
      assert (getmax (insert x4 (insert x3 (insert x2 (insert x empty)))) = x)

  let test_getmin () =
      let x = C.generate () in
      let x2 = C.generate_gt x () in
      let x3 = C.generate_gt x2 () in
      let x4 = C.generate_gt x3 () in
      assert (getmin (insert x2 (insert x4 (insert x (insert x3 empty)))) = x)

  let test_delete () =
      let x = C.generate () in
      let x2 = C.generate_lt x () in
      let x3 = C.generate_lt x2 () in
      let x4 = C.generate_lt x3 () in
      let after_ins = insert x4 (insert x3 (insert x2 (insert x empty))) in
      assert (delete x (delete x4 (delete x3 (delete x2 after_ins))) = empty)

  let run_tests () =
    test_insert ();
    test_search ();
    test_getmax ();
    test_getmin ();
    test_delete ();
    ()

end

(* Here is how you would define an int binary tree using the BinSTree
 * functor, which expects a module to be passed in as an argument.
 * You should write tests using the IntTree module (or you can
 * give the module a different type), and you should use
 * this call to a functor as an example for how to test modules further
 * down in the pset.
 *)

module IntTree = BinSTree(IntCompare)

(* Please read the entirety of "testing.ml" for an explanation of how
 * testing works.
 *)
let _ = IntTree.run_tests ()

(*****************************************************************************)
(*                               Part 3                                      *)
(*****************************************************************************)

(* A signature for a priority queue. See the pset specification on the
 * course website and section notes for week 4
 * for more information if you are unfamiliar with priority queues.
 *
 * IMPORTANT: In your implementations of priority queues, the MINIMUM
 * valued element corresponds to the HIGHEST priority. For example,
 * in just an int prioqueue, the integer 4 has lower priority than
 * the integer 2.
 *)
module type PRIOQUEUE =
sig
  exception QueueEmpty

  (* What's being stored in the priority queue *)
  type elt

  (* The queue itself (stores things of type elt) *)
  type queue

  (* Returns an empty queue *)
  val empty : queue

  (* Takes a queue, and returns whether or not it is empty *)
  val is_empty : queue -> bool

  (* Takes an element and a queue, and returns a new queue with the
   * element added *)
  val add : elt -> queue -> queue

  (* Pulls the highest priority element out of the passed-in queue,
   * also returning the queue with that element
   * removed. Can raise the QueueEmpty exception. *)
  val take : queue -> elt * queue

  (* Run invariant checks on the implementation of this binary tree.
   * May raise Assert_failure exception *)
  val run_tests : unit -> unit
end

(*>* Problem 3.0 *>*)

(* Implement a priority queue using lists
 * You can use OCaml's built-in lists (i.e. [] and ::)
 * You are also free to use anything from the List module,
 *)
module ListQueue(C : COMPARABLE) : PRIOQUEUE with type elt = C.t =
struct
  (* Remember to use the "C" (COMPARABLE) module! You may want to
   * look above at BinSTree for inspiration *)
  exception QueueEmpty

  type elt = C.t

  type queue = elt list

(*>* Problem 3.1 *>*)
  let empty = []

(*>* Problem 3.2 *>*)
  let is_empty (t : queue) : bool =
    match t with
    | [] -> true
    | _ -> false

(*>* Problem 3.3 *>*)

  (* Like with getmin and getmax in your binary tree, you should implement
   * add and take such that, if all elements have the same priority, this
   * module simply becomes a regular queue (i.e., elements inserted earlier
   * should be removed before elements of the same priority inserted later)
   *)
  let rec add (e : elt) (q : queue) : queue =
    match q with
    | [] -> [e]
    | x :: xs ->
      match (C.compare e x) with
      | Less -> e :: q
      | _ -> x :: (add e xs)


(*>* Problem 3.4 *>*)
  let take (q : queue) : elt * queue =
    match q with
    | hd :: tl -> (hd, tl)
    | [] -> raise QueueEmpty

  let test_is_empty () =
      let x = C.generate () in
      let x2 = C.generate_lt x () in
      let x3 = C.generate_lt x2 () in
      let x4 = C.generate_lt x3 () in
      assert (is_empty empty = true);
      assert (is_empty (add x empty) = false);
      assert (is_empty (add x3 (add x2 empty)) = false)

  let test_add () =
      let x = C.generate () in
      let x2 = C.generate_gt x () in
      let x3 = C.generate_gt x2 () in
      let x4 = C.generate_gt x3 () in
      assert (take (add x empty) = (x, empty));
      assert (is_empty (add x3 (add x2 (add x empty))) = false);
      assert (add x empty = [x]);
      assert (add x2 (add x empty) = [x;x2])

  let test_take () =
      let x = C.generate () in
      let x2 = C.generate_gt x () in
      let x3 = C.generate_gt x2 () in
      let x4 = C.generate_gt x3 () in
      let all_added = add x2 (add x3 (add x4 (add x empty))) in
      let (y, almost) = take all_added in
      let (y2, almost2) = take almost in
      let (y3, almost3) = take almost2 in
      let (y4, nempty) = take almost3 in
      assert (x = y);
      assert (x2 = y2);
      assert (x3 = y3);
      assert (x4 = y4);
      assert (is_empty nempty = true);
      assert (is_empty almost2 = false)

  let run_tests () =
    test_is_empty ();
    test_add ();
    test_take ();
    ()
end

(* IMPORTANT: Don't forget to actually *call* run_tests, as with
 * IntTree above! *)

module IntLQueue = ListQueue(IntCompare)

let _ = IntLQueue.run_tests ()


(*>* Problem 3.5 *>*)

(* Now implement a priority queue using a Binary Search Tree.
 * Luckily, you should be able to use *a lot* of your code from above! *)

(* Uncomment when you finish! *)

module TreeQueue(C : COMPARABLE) : PRIOQUEUE with type elt = C.t=
struct
  exception QueueEmpty

  (* You can use the module T to access the functions defined in BinSTree,
   * e.g. T.insert *)
  module T = (BinSTree(C) : BINTREE with type elt = C.t)

  (* Implement the remainder of the module! *)
  type elt = C.t

  type tree = T.tree
  (* Leaf | Branch of tree * elt list * tree *)
  
  type queue = Empty | Tree of tree

  let empty = Empty

  let is_empty (q : queue) : bool =
    match q with
    | Empty -> true
    | _ -> false

  let add (e: elt) (q: queue) : queue =
    match q with
    | Empty -> Tree (T.insert e T.empty)
    (* Tree (Branch (Leaf, [e], Leaf)) *)
    | Tree t -> Tree (T.insert e t)
    (*
    match q with
    | Leaf -> (Leaf, e, Leaf)
    | Branch (left, elts, right) ->
      let hd :: tl = elts in
      let diff = C.compare e hd in
      match diff with
      | Less -> (add e left, elts, right)
      | Equal -> (left, e :: elts, right)
      | Greater -> (left, elts, add e right)
    *)

  let take (q: queue) : elt * queue =
    match q with
    | Empty -> raise QueueEmpty
    | Tree t ->
      let e = T.getmin t in
      if t = T.insert e T.empty then
       (e, empty)
      else
        (e,  Tree (T.delete e t))
      (*
      match elts with
      | [] -> raise QueueEmpty
      | hd::_ -> T.delete hd t 
      *)
    
  let test_is_empty () =
      let x = C.generate () in
      let x2 = C.generate_lt x () in
      let x3 = C.generate_lt x2 () in
      let x4 = C.generate_lt x3 () in
      assert (is_empty empty = true);
      assert (is_empty (add x empty) = false)
      (* assert (is_empty (add x3 (add x2 emptyset)) = false) *)

  let test_add () =
      let x = C.generate () in
      let x2 = C.generate_gt x () in
      let x3 = C.generate_gt x2 () in
      let x4 = C.generate_gt x3 () in
      assert (take (add x empty) = (x, empty));
      (*assert (add x emptyset = Tree (Leaf x));
      assert (add x2 (add x emptyset) = Tree (Branch(Leaf, x2, Leaf),x, Leaf));
      assert (add x3 (add x2 (add x emptyset)) = Tree (Branch(Leaf, x2, Leaf), x, Branch(Leaf, x3, Leaf)));
      *)assert (is_empty (add x3 (add x2 (add x empty))) = false)

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
      assert (x4 = y);
      assert (x3 = y2);
      assert (x2 = y3);
      assert (x = y4);
      assert (is_empty nempty = true);
      assert (is_empty almost2 = false)
      (*assert (almost3 = Tree (Branch(Leaf, x, Leaf)));
      assert (all_added = Tree (Branch(Branch(Leaf, x4, Leaf), x2, Leaf), x, Branch(Leaf, x3, Leaf)))
*)
  let run_tests () =
    test_is_empty ();
    test_add (); 
    test_take ();
    ()
end


module IntTQueue = TreeQueue(IntCompare)

let _ = IntTQueue.run_tests ()

(*****************************************************************************)
(*                               Part 4                                      *)
(*****************************************************************************)

(*>* Problem 4.0 *>*)
module BinaryHeap(C : COMPARABLE) : PRIOQUEUE with type elt = C.t =
struct

  exception QueueEmpty

  type elt = C.t

  (* Access module function *)
  module T = (BinSTree(C) : BINTREE with type elt = C.t)

  (* A node in the tree is either even or odd *)
  type balance = Even | Odd

  (* Type definition for tree *)
  type tree =   TwoBranch of balance * elt * tree * tree
              | OneBranch of elt * elt
              | Leaf of elt

  (* A queue is either empty, or a tree *)
  type queue = Empty | Tree of tree

  let empty = Empty

  let is_empty (q : queue) = q = Empty

  (* Adds element e to the queue q *)
  let add (e : elt) (q : queue) : queue =
    let rec add_to_tree (e : elt) (t : tree) : tree =
      match t with
      (* If the tree is just a Leaf, then we end up with a OneBranch *)
      | Leaf e1 ->
        (match C.compare e e1 with
         | Equal | Greater -> OneBranch (e1, e)
         | Less -> OneBranch (e, e1))

      (* If the tree was a OneBranch, it will now be a TwoBranch *)
      | OneBranch(e1, e2) ->
        (match C.compare e e1 with
         | Equal | Greater -> TwoBranch (Even, e1, Leaf e2, Leaf e)
         | Less -> TwoBranch (Even, e, Leaf e2, Leaf e1))

      (* If the tree was even, then it will become an odd tree (and the element
       * is inserted to the left *)
      | TwoBranch(Even, e1, t1, t2) ->
        (match C.compare e e1 with
         | Equal | Greater -> TwoBranch(Odd, e1, add_to_tree e t1, t2)
         | Less -> TwoBranch(Odd, e, add_to_tree e1 t1, t2))

      (* If the tree was odd, then it will become an even tree (and the element
       * is inserted to the right *)
      | TwoBranch(Odd, e1, t1, t2) ->
        match C.compare e e1 with
        | Equal | Greater -> TwoBranch(Even, e1, t1, add_to_tree e t2)
        | Less -> TwoBranch(Even, e, t1, add_to_tree e1 t2)
    in
    (* If the queue is empty, then e is the only Leaf in the tree.
     * Else, insert it into the proper location in the pre-existing tree *)
    match q with
    | Empty -> Tree (Leaf e)
    | Tree t -> Tree (add_to_tree e t)

  (* Returns the top element of the tree t *)
  let get_top (t : tree) : elt =
    match t with
    | Leaf e -> e
    | OneBranch (e, _) -> e
    | TwoBranch (_, e, _, _) -> e

  (* Helper for fix. Compares three elements, returns lowest *)
  let child_compare (e1 : elt) (e2 : elt) (e3 : elt) : int =
    if (C.compare e1 e2) = Greater then
      if (C.compare e2 e3) = Greater then 3 else 2
    else
      if (C.compare e1 e3) = Greater then 3 else 1

  (* Helper for fix. Switches top element with node *)
  let top_switch (node : elt) (t : tree) : tree =
    match t with
    | Leaf e -> Leaf node
    | OneBranch (e1, e2) -> OneBranch (node, e2)
    | TwoBranch (bal, e, l, r) -> TwoBranch (bal, node, l, r)

  (* If the top node is greater than its children, fixes tree. *)
  let rec fix (t : tree) : tree =
    match t with
    | Leaf e -> t
    | OneBranch (p, c) -> 
      (match C.compare p c with
      | Greater -> OneBranch(c,p)
      | _ -> t)
    | TwoBranch (bal, node, l, r) ->
      (match child_compare node (get_top l) (get_top r) with
      | 1 -> t
      | 2 -> TwoBranch(bal, (get_top l), fix (top_switch node l), r)
      | _ -> TwoBranch(bal, (get_top r), l, fix (top_switch node r)))

  let extract_tree (q : queue) : tree =
    match q with
    | Empty -> raise QueueEmpty
    | Tree t -> t

  (* Gets the last node of the tree *)
  let rec get_last (t : tree) : elt * queue =
    match t with
    | Leaf e -> (e, Empty)
    | OneBranch (p, c) -> (c, Tree(Leaf p))
    | TwoBranch (Odd, node, l, r) ->
       (* matches the left side - we always will have the new node on left*) 
       (match get_last l with
       | (e, Empty) -> (e, Tree (OneBranch (node, get_top r)))
       | (e, Tree l') -> (e, Tree (TwoBranch (Even, node, l', r))))

    | TwoBranch (Even, node, l, r) ->
      (* Even means that we are going to the right *)
       match get_last r with
       | (e, Empty) -> (e, Tree (OneBranch (node, get_top l)))
       | (e, Tree r') -> (e, Tree (TwoBranch (Odd, node, l, r')))

  (* Implements the algorithm described in the writeup.*)
  let take (q : queue) : elt * queue =
    match extract_tree q with
    (* If the tree is just a Leaf, then return the value of that leaf, and the
     * new queue is now empty *)
    | Leaf e -> e, Empty

    (* If the tree is a OneBranch, then the new queue is just a Leaf *)
    | OneBranch (e1, e2) -> e1, Tree (Leaf e2)

    (* Removing an item from an even tree results in an odd tree. This
     * implementation replaces the root node with the most recently inserted
     * item, and then fixes the tree that results if it is violating the
     * strong invariant *)
    | TwoBranch (Even, e, t1, t2) ->
      let (last, q2') = get_last t2 in
      (match q2' with
       (* If one branch of the tree was just a leaf, we now have just
        * a OneBranch *)
       | Empty -> (e, Tree (fix (OneBranch (last, get_top t1))))
       | Tree t2' -> (e, Tree (fix (TwoBranch (Odd, last, t1, t2')))))
    (* Implement the odd case! *)
    | TwoBranch (Odd, e, t1, t2) ->
      let (last, q') = get_last t1 in
      (match q' with
        | Empty -> failwith "Tree Wasn't Odd"
        | Tree (Leaf e') -> 
            (e, Tree( fix (TwoBranch (Even, last, Leaf (e'), t2))))
        | Tree t' -> 
            (e, Tree (fix (TwoBranch (Even, last, t', t2))))
      )

(* Assert tests for this function! *)
let test_get_top () = 
  let x  = C.generate       () in
  let x2 = C.generate_gt x  () in
  let x3 = C.generate_gt x2 () in
  let x4 = C.generate_gt x3 () in
  let Tree t1 = add x empty in
  assert (get_top t1 = x);
  let Tree t2 = (add x4 (add x2 empty)) in 
  assert (get_top t2 = x2);
  let Tree t3 = (add x4 (add x3 (add x2 (add x empty)))) in
  assert (get_top t3 = x);
  let Tree t4 = (add x2 (add x3 empty)) in
  assert (get_top t4 = x2)

let test_get_last () =
  let x  = C.generate       () in
  let x2 = C.generate_gt x  () in
  let x3 = C.generate_gt x2 () in
  let x4 = C.generate_gt x3 () in
  let Tree t1 = add x empty in
  assert (get_last t1 = (x, empty));
  let Tree t2 = (add x3 (add x3 empty)) in
  assert (get_last t2 = (x3, add x3 empty));
  let Tree t3 = (add x4 (add x3 (add x2 (add x empty)))) in
  assert (get_last t3 = (x4, add x3 (add x2 (add x empty))));
  let Tree t4 = (add x2 (add x3 empty)) in
  assert (get_last t4 = (x3, add x2 empty));
  let Tree t5 = (add x (add x2 empty)) in
  assert (get_last t5 = (x2, add x empty))

(* Since there is no way to test fix, we are testing it within take*)
let test_take () =
  let x  = C.generate       () in
  let x2 = C.generate_gt x  () in
  let x3 = C.generate_gt x2 () in
  let x4 = C.generate_gt x3 () in
  let q  = add x empty in
  let q2 = add x2 q in
  let q3 = add x3 (add x (add x4 empty)) in
  let q4 = add x4 (add x4 (add x4 empty)) in
  assert (take q2 = (x, add x2 empty));
  assert (take q3 = (x, add x3 (add x4 empty)));
  assert (take q = (x, empty));
  assert (take q4 = (x4, add x4 (add x4 empty)))
  
  let run_tests () =
    test_get_top ();
    test_get_last ();
    test_take (); 
    ()
end

module IntHeap = BinaryHeap(IntCompare)

let () = IntHeap.run_tests ()

module IntListQueue = (ListQueue(IntCompare) :
                        PRIOQUEUE with type elt = IntCompare.t)
module IntHeapQueue = (BinaryHeap(IntCompare) :
                        PRIOQUEUE with type elt = IntCompare.t)

module IntTreeQueue = (TreeQueue(IntCompare) :
                        PRIOQUEUE with type elt = IntCompare.t)

(* store the whole modules in these variables *)
let list_module = (module IntListQueue : PRIOQUEUE with type elt = IntCompare.t)
let heap_module = (module IntHeapQueue : PRIOQUEUE with type elt = IntCompare.t)

let tree_module = (module IntTreeQueue : PRIOQUEUE with type elt = IntCompare.t)

(* Implements sort using generic priority queues. *)
let sort (m : (module PRIOQUEUE with type elt=IntCompare.t)) (lst : int list) =
  let module P = (val (m) : PRIOQUEUE with type elt = IntCompare.t) in

  let rec extractor pq lst =
    if P.is_empty pq then lst else
    let (x, pq') = P.take pq in
    extractor pq' (x::lst) in
  let pq = List.fold_right ~f:P.add ~init:P.empty lst in
  List.rev (extractor pq [])

(* Sorting with a priority queue with an underlying heap
 * implementation is equivalent to heap sort! *)
let heapsort = sort heap_module

(* Sorting with a priority queue with your underlying tree*)
let treesort = sort tree_module

(* Sorting with a priority queue with an underlying unordered list
 * implementation is equivalent to heap sort! *)
let selectionsort = sort list_module

(* Test! *)
(* Random int list and its sorted version. *)
let lst = [100; 7; 29; 4; 18]
let sorted = [4; 7; 18; 29; 100]

(* Test all 3 sorts. *)
assert(heapsort lst = sorted);;
assert(treesort lst = sorted);;
assert(selectionsort lst = sorted);;

(*****************************************************************************)
(*                               Part N                                      *)
(*****************************************************************************)

(*>* Problem N.0 *>*)
(* Module's signature - only need to sort*)
module type SORT =
sig
  type c
  val sort : c list -> c list
end

(* New module based on thsi signiature *)
module Hsort(C : COMPARABLE) : SORT with type c = C.t =
struct

  type c = C.t

  (* Use earlier defined BinaryHeap module. *)
  module Bheap = (BinaryHeap(C) : PRIOQUEUE with type elt = C.t)

  (* Sort list a Bheap priority queue. *)
  let sort (lst : c list) : c list =
    let rec extractor pq lst =
      if Bheap.is_empty pq then lst else
      let (x, pq') = Bheap.take pq in
      extractor pq' (x::lst) in
    let pq = List.fold_right ~f:Bheap.add ~init:Bheap.empty lst in
    List.rev (extractor pq [])
end

module IntSort = Hsort(IntCompare)
module IntStringSort = Hsort(IntStringCompare)

(* Test Cases *)
assert(IntSort.sort [100; 7; 29; 4; 18] = [4; 7; 18; 29; 100]);;
assert(IntStringSort.sort [(100, "a"); (7, "b"); (29, "c"); (4, "d"); (18, "e")] = [(4, "d"); (7, "b"); (18, "e"); (29, "c"); (100, "a")]);;


(*>* Problem N.1 *>*)
(* Test Results:
 * Heap sort:
 * 1000 elements: 0.0051069 seconds
 * 10000 elements: 0.09926 seconds (18x)
 * 100000 elements: 1.0405 seconds (10x)
 * 1000000 elements: 14.393 seconds (14x)
 *
 * Tree sort:
 * 1000 elements: 0.0031710 seconds
 * 10000 elements: 0.062144 seconds (20x)
 * 100000 elements: 0.56301 seconds (9x)
 * 1000000 elements: 7.1989 seconds (13x)
 *
 * Selection sort:
 * 100 elements: 0.00067902 seconds
 * 1000 elements: 0.053416 seconds (78x)
 * 10000 elements: 2.1371 seconds  (40x)
 *
 * Since an increase in the number of elements by one order of magnitude
 * causes the time to increase by two orders, selection sort seems to be O(n^2)  
 * complexity. This makes sense because when we insert and remove an element,
 * we have to traverse the list twice. This implies that we traverse a list of
 * length n, 2n times, resulting in an overall complexity of O(n^2).
 *
 *
 * With increasing number of elements, heap sort and tree sort both increase 
 * similarly in runtime although tree sort is slightly better than heap sort.
 * They both perform much better than selection sort, which is consistent 
 * with them having O(nlog(n)) complexity. For both, this makes sense. For
 * heapsort, we pull the last node from the heap n times, each time of which
 * we search through the depth of the heap, taking log n time. Tree sort is
 * very similar - we search through the tree n times, each taking log n time
 *)

(*>* Problem N.2 *>*)

(* Generate a n-long list of random ints. *)
let randlist (n : int) : int list =
  let rec randint (n : int) (lst : int list) : int list =
    if n = 0
    then lst
    else (randint (n-1) ((Random.int Int.max_value)::lst)) in
  (randint n [])   
;;

(* Time how long function takes to sort n-long list. *)
let timesort (f : int list -> int list) (n : int) =
  let xs = (randlist n) in
  let start = Unix.gettimeofday() in
  let run = f xs in
  let stop = Unix.gettimeofday() in
  stop -. start
;;

(* Run trials. *)
timesort heapsort 1000000;;
timesort treesort 1000000;;
timesort selectionsort 10000;;

let minutes_spent : int = 900;;
