open! Core

(* Geometric primitives shared by the solver and everything that reads its
   output. *)

module Rect = struct
  type t =
    { x : int
    ; y : int
    ; width : int
    ; height : int
    }
  [@@deriving sexp, equal]

  let contains t ~x ~y =
    x >= t.x && x < t.x + t.width && y >= t.y && y < t.y + t.height
  ;;
end

module Axis = struct
  type t =
    [ `Row (** children laid out left-to-right *)
    | `Col (** children laid out top-to-bottom *)
    ]
  [@@deriving sexp, equal]

  (* A rect's position and extent along the axis. *)
  let start (t : t) (r : Rect.t) =
    match t with
    | `Row -> r.x
    | `Col -> r.y
  ;;

  let size (t : t) (r : Rect.t) =
    match t with
    | `Row -> r.width
    | `Col -> r.height
  ;;
end
