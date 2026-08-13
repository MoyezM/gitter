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

  (* Every leaf draws a 1-cell frame; the box inside it is where content
     renders and clicks land. The inset law lives HERE, once — sizing,
     drawing and hit-testing must agree on it, or clicks land on rows
     that aren't where they're drawn. *)
  let frame = 1

  let inner t =
    { x = t.x + frame
    ; y = t.y + frame
    ; width = Int.max 0 (t.width - (2 * frame))
    ; height = Int.max 0 (t.height - (2 * frame))
    }
  ;;

  (* [(x, y)] translated into [t]'s inner coordinates; None on the frame
     or outside — border clicks are nobody's. *)
  let to_inner t ~x ~y =
    let i = inner t in
    if x >= i.x && x < i.x + i.width && y >= i.y && y < i.y + i.height
    then Some (x - i.x, y - i.y)
    else None
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
