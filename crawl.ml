open Core.Std
open Util
open CrawlerServices
open Order
open Pagerank


(* RandomWalkRanker and QuantumRanker are for karma questions only *)
module MoogleRanker
  = InDegreeRanker (PageGraph) (PageScore)
  (*
     = RandomWalkRanker (PageGraph) (PageScore) (struct
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
  *)
  (*
   = QuantumRanker (PageGraph) (PageScore) (struct
       let alpha = 0.01
       let num_steps = 1
       let debug = true
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s =
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* TODO: Build an index as follows:
 *
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec loopover (p: link list) (frontier: LinkSet.set) : LinkSet.set =
  match p with
  | [] -> frontier
  | x :: xs -> loopover xs (LinkSet.insert x frontier)

let rec addwords (l: link) (p: string list) (d: WordDict.dict) : WordDict.dict =
  match p with
  | [] -> d
  | x :: xs -> 
    let value = WordDict.lookup d x in
    match value with
    | None -> addwords l xs (WordDict.insert d x (LinkSet.singleton l))
    | Some setlinks -> 
      let d = WordDict.remove d x in
      addwords l xs (WordDict.insert d x (LinkSet.insert l setlinks))


let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict =
  match LinkSet.choose frontier with
  | None -> d
  | Some (link, linkset) ->
  if LinkSet.member visited link then
    crawl (n-1) frontier visited d
  else if n = 0 then d else
    match CrawlerServices.get_page link with
    | None -> d
    | Some page ->
    (* May not be passing frontier by reference *)
    let d = addwords link page.words d in
    let frontier = loopover page.links frontier in
    crawl (n-1) frontier visited d
;;

let crawler () =
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
