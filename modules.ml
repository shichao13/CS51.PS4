(*****************************************************************************)
(*                           Part 1 - Warmup                                 *)
(*****************************************************************************)

open Core.Std

(*>* Problem 1.0 *>*)
module type MATH =
sig
    val pi : float
    val cos : float -> float
    val sin : float -> float
    val sum : float -> float -> float
    val max : float list -> float option
end

(* Write a module called Math that implements the MATH signature above *)

module Math : MATH =
    struct
        let pi = 4. *. atan 1.
        let cos = cos
        let sin = sin
        let sum = (+.)
        let max (lst : float list) : float option =
            match lst with
            | [] -> None
            | x :: xs' -> 
                Some (List.fold_right ~f:(fun acc x -> if x > acc then x else acc) ~init:(Float.of_int Int.min_value) lst)


(*>* Problem 1.1 *>*)

(*
 * Write a signature called LIST that only exposes the functions length,
 * fold_right, and rev. The Core.Std.List documentation might be helpful:
 * https://ocaml.janestreet.com/ocaml-core/109.60.00/doc/core/#Std.List
 *)

module type LIST =
sig
    type 'a t = 'a list
    val length : 'a t -> int
    val fold_right : 'a t -> f:('a -> 'b -> 'b) -> init:'b -> 'b
    val rev : 'a t -> 'a t
end


(* The following should work after you have created your signature (you
 * can uncomment them *)



module MyList = (List : LIST);;

let _ =
    assert(MyList.length [1;2;3] = 3);
    assert(MyList.fold_right ~f:(+) ~init:0 [1;2;3] = 6);
    assert(MyList.rev [1;2;3] = [3;2;1]);;


(* Even with your signature, the following line should never compile:

MyList.fold_left ~f:(+) ~init:0 [1;2;3]

*)


(*>* Problem 1.2 *>*)
module Rob =
struct
    type house =
        Adams | Lowell | Quincy |
        Kirkland | Winthrop | Eliot |
        Leverett | Mather | Dunster |
        Pforzheimer | Cabot | Currier

    type info = {
        hometown : string;
        year : int;
        concentration : string;
        house : house
    }

    let hometown = "Franklin Borough, NJ"
    let year = 2013
    let concentration = "Computer Science"
    let house = Kirkland
    let fold = List.fold_left ~f:(+)

    let info = {
        hometown;
        year;
        concentration;
        house
    }

    let grade_assignment assignment =
      "Everyone gets a perfect score for pset " ^ String.strip assignment ^ "!"

    let favorite_function _ = failwith "I don't have a favorite function"
    let least_favorite_function = ( ** )

    let print_info () =
        let _ = print_string (
            info.hometown ^ "\n" ^
            string_of_int year ^ "\n" ^
            info.concentration ^ "\n") in
        match info.house with
        | Kirkland -> print_string "Kirkland!\n"
        | _ -> failwith "Do any other houses matter?"

end

module Ben =
struct
    type info = {
        hometown : string;
        house : string;
        year : int;
        concentration : string
    }

    let least_favorite_function = (land)

    let rec info = {
        hometown = hometown;
        house = house;
        year = 2015;
        concentration = "Computer Science"
    }
    and hometown = "Holland, Michigan"
    and house = "Dunster"

    let grade_assignment assignment =
      "Everyone gets a zero for pset " ^ string_of_int assignment ^ ">:-("

    let favorite_function x y =
        log x +. log y +. Int64.to_float 0x46524F535459L

    let print_info = fun _ ->
        print_string (info.house ^ "\n" ^ info.hometown ^ "\n" ^
            string_of_int info.year ^ "\n" ^ info.concentration)

    let f = (-)
    let fold = List.fold_right ~f:

end

(* Write a signature TF that exposes as much of *both* Ben
 * and Rob as possible. *)

(* The follow should work after you define the TF signature *)
(*
module TFBen = (Ben : TF)
module TFRob = (Rob : TF)
*)

module type TF =
sig
    type info
    val info : info
    val hometown : string
    val favorite_function : float -> float -> float
    val print_info : unit -> unit
    val fold : int list -> init:int -> int
end


(*>* Problem 1.3 *>*)

(* Challenge (worth 1 point)!

 * Write another module that matches sig TF. Try to fill
 * in the info of your own TF! If you don't have a TF, or
 * your TF is already listed here, then feel free to
 * create a module for Henry, Jesse, or yourself.
 *
 * You should not trivially reuse any of the same values as
 * were used above in order to implement the module.
 *)

module Ryan =
struct

    type info = {
        hometown : string;
        year : int;
        concentration : string;
        house : string
    }

    let hometown = "Unknown Hometown"
    let year = 2014
    let concentration = "Computer Science"
    let house = "Pforzheimer"

    let info = {
        hometown;
        year;
        concentration;
        house
    }

    let least_favorite_function = ( Float.of_int )
    let favorite_function _ = failwith "All folds are beautiful, so no favorite."
    let fold = List.fold_right ~f:( * )
    let grade_assignment assignment =
      "Everyone gets a fair score for pset " ^ string_of_int assignment ^ "!!!"

    let print_info = fun _ -> 
        print_string (info.house ^ "\n" ^
            string_of_int year ^ "\n" ^
            info.concentration ^ "\n" ^
            info.hometown ^ "\n" ^
            (grade_assignment 4)) 

end

module TFRyan = (Ryan : TF)
