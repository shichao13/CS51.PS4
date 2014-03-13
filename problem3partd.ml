  let rec insert_downward (d: dict) (k: key) (v: value) : kicked =
    match d with
      | Leaf -> Done (Two (Leaf, (k, v), Leaf))
      | Two(left,n,right) -> insert_downward_two (k, v) n left right
      | Three(left,n1,middle,n2,right) -> insert_downward_three (k, v) n1 n2 left middle right

  (* Downward phase on a Two node. (k,v) is the (key,value) we are inserting,
   * (k1,v1) is the (key,value) of the current Two node, and left and right
   * are the two subtrees of the current Two node. *)
  and insert_downward_two ((k,v): pair) ((k1,v1): pair)
      (left: dict) (right: dict) : kicked =
    match D.compare k k1 with
    | Equal -> failwith "Keys are Equal"
    | Less -> 
      (match insert_downward left k v with
      | Up _ -> failwith "Insert did not work"
      | Done tree -> Done (Two (tree, (k1, v1), right)))
    | Greater -> 
      match insert_downward right k v with
      | Up _ -> failwith "Insert did not work"
      | Done tree -> Done (Two (left, (k1, v1), tree))

  (* Downward phase on a Three node. (k,v) is the (key,value) we are inserting,
   * (k1,v1) and (k2,v2) are the two (key,value) pairs in our Three node, and
   * left, middle, and right are the three subtrees of our current Three node *)
  and insert_downward_three ((k,v): pair) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : kicked =
    match D.compare k k1 with
    | Equal -> failwith "Keys are Equal!"
    | Less -> 
      (match insert_downward left k v with
      | Up _ -> failwith "Insert did not work"
      | Done tree -> Done ( Three (tree, (k1, v1), middle, (k2, v2), right)))
    | Greater ->
      match D.compare k k2 with
      | Equal -> failwith "Keys are Equal!"
      | Less -> 
        (match insert_downward middle k v with
        | Up _ -> failwith "Insert did not work"
        | Done tree -> Done ( Three (left, (k1, v1), tree, (k2,  v2), right)))
      | Greater -> 
        (match insert_downward right k v with
        | Up _ -> failwith "Insert did not work"
        | Done tree -> Done ( Three (left, (k1, v1), middle, (k2, v2), tree)))