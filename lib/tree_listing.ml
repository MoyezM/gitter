open! Core
open! Bonsai_term

(* The shared navigation core of the tree panes (files, stack): ONE
   action vocabulary, one transition, and one event shell over
   [Search.Tree_search.Model], parameterized by the pane's existing
   search record plus two fold capabilities. What stays per-pane: what a
   row IS, what folding MEANS ([caps]), and the pane's extra actions
   (the files ops, the stack's Enter). *)

module Action = struct
  (* Cursor-moving actions carry the pane height so the transition can
     reveal the selection (state machines may be hosted where there are
     no dimensions — the pane handler stamps it). *)
  type t =
    | Move of
        { dir : [ `Up | `Down ]
        ; height : int
        }
    | Activate of
        { row : int (* absolute displayed-row index; click: toggles folds *)
        ; height : int
        }
    | Fold of { height : int } (* h/left: fold, or jump to the parent *)
    | Unfold of { height : int } (* l/right *)
    | Wheel of
        { dir : int (* +1 down, -1 up *)
        ; height : int
        }
    | Rows_changed (* the derived rows changed: repair the selection *)
  [@@deriving sexp_of]
end

(* The pane's search record plus what folding means for it: which of
   open/closed/leaf a row is, and how the fold state changes to
   fold/unfold a key. *)
type ('row, 'fold) caps =
  { pane : ('row, 'fold) Search.Tree_search.pane
  ; fold_state : 'row -> [ `Open | `Closed | `Leaf ]
  ; set_folded : 'fold -> key:string -> folded:bool -> 'fold
  }

let apply
      { pane; fold_state; set_folded }
      (model : 'fold Search.Tree_search.Model.t)
      (action : Action.t)
  =
  let current =
    Search.Tree_search.displayed_rows pane ~fold:model.fold ~search:model.search
  in
  if List.is_empty current
  then
    if Search.Prompt.is_active model.search
    then model (* an active search narrowed to nothing: hold still (T5) *)
    else { model with Search.Tree_search.Model.listing = Listing.empty }
  else (
    let index_of rows selection = Listing.index_of ~key:pane.key rows selection in
    let index = index_of current model.listing.selection in
    (* Select by index in [rows] under a (possibly changed) fold state. *)
    let select_in ?height rows ~fold index =
      { model with
        Search.Tree_search.Model.fold
      ; listing = Listing.select ~key:pane.key rows ?height ~index model.listing
      }
    in
    (* Fold/unfold [r], re-locating it BY KEY: the toggle reshuffles
       indices. *)
    let refold ~height r ~folded =
      let fold = set_folded model.fold ~key:(pane.key r) ~folded in
      let rows = pane.rows fold in
      select_in ~height rows ~fold (index_of rows (Some (pane.key r)))
    in
    match action with
    | Action.Wheel { dir; height } ->
      { model with
        Search.Tree_search.Model.listing =
          Listing.wheel model.listing ~total:(List.length current) ~height ~dir
      }
    (* Motions walk the DISPLAYED rows — the narrowed survivors while a
       search is active (T4), the folded tree otherwise. *)
    | Move { dir = `Up; height } -> select_in ~height current ~fold:model.fold (index - 1)
    | Move { dir = `Down; height } -> select_in ~height current ~fold:model.fold (index + 1)
    | Activate { row; height } ->
      let row = Int.clamp_exn row ~min:0 ~max:(List.length current - 1) in
      let r = List.nth_exn current row in
      (match fold_state r with
       | `Open when not (Search.Prompt.is_active model.search) ->
         refold ~height r ~folded:true
       | `Closed when not (Search.Prompt.is_active model.search) ->
         refold ~height r ~folded:false
       | `Open | `Closed | `Leaf ->
         (* While narrowing, a click selects — the fold state is not the
            filter's to change; the commit that follows un-narrows. *)
         select_in ~height current ~fold:model.fold row)
    | Fold { height } ->
      (match fold_state (List.nth_exn current index) with
       | `Open -> refold ~height (List.nth_exn current index) ~folded:true
       | `Closed | `Leaf ->
         (match Listing.parent_index ~depth:pane.depth current index with
          | Some i -> select_in ~height current ~fold:model.fold i
          | None -> model))
    | Unfold { height } ->
      (match fold_state (List.nth_exn current index) with
       | `Closed -> refold ~height (List.nth_exn current index) ~folded:false
       | `Open | `Leaf -> model)
    | Rows_changed ->
      { model with
        Search.Tree_search.Model.listing =
          Listing.rows_changed ~key:pane.key current model.listing
      })
;;

(* ---- the event shell --------------------------------------------------- *)

(* The nav reading of printables — merge into the pane's own [idle_char]
   (its ops chars first, this as the fallback) before handing to
   [Search.Action.keymap]. [lift] embeds the shared action into the
   pane's own action type. *)
let idle_char ~height ~lift c =
  let nav a = Some (Search.Action.pane (lift a)) in
  match c with
  | 'j' -> nav (Action.Move { dir = `Down; height })
  | 'k' -> nav (Action.Move { dir = `Up; height })
  | 'h' -> nav (Action.Fold { height })
  | 'l' -> nav (Action.Unfold { height })
  | _ -> None
;;

(* Every non-keymap event: arrows mirror j/k (both modes — among the
   survivors while narrowed) and h/l (idle only), the wheel scrolls, a
   click resolves against the rendered rows FIRST (blank space below a
   short tree is not a row), THEN commits any active prompt (keys
   table). *)
let handle ~height ~total ~scroll ~inject ~lift (event : Event.t) =
  let pane a = Search.Action.pane (lift a) in
  match event with
  | Event.Key_press { key = Arrow `Down; mods = [] } ->
    inject (pane (Action.Move { dir = `Down; height }))
  | Key_press { key = Arrow `Up; mods = [] } ->
    inject (pane (Action.Move { dir = `Up; height }))
  | Key_press { key = Arrow `Left; mods = [] } ->
    inject (Search.Action.when_idle (pane (Action.Fold { height })))
  | Key_press { key = Arrow `Right; mods = [] } ->
    inject (Search.Action.when_idle (pane (Action.Unfold { height })))
  | Mouse { kind = Scroll `Down; _ } -> inject (pane (Action.Wheel { dir = 1; height }))
  | Mouse { kind = Scroll `Up; _ } -> inject (pane (Action.Wheel { dir = -1; height }))
  | Mouse { kind = Left; position; _ } ->
    let activate =
      match Listing.row_at ~total ~height ~y:position.y scroll with
      | None -> []
      | Some row -> [ inject (pane (Action.Activate { row; height })) ]
    in
    Effect.Many (activate @ [ inject Search.Action.implicit_commit ])
  | _ -> Effect.Ignore
;;
