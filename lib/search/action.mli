open! Core
open! Bonsai_term

(** THE integration point between a searchable pane and the search: the
    pane's state machine runs on ['pane t] — its own action type wrapped
    with the search's — and search owns dispatch entirely. The pane's
    action type contains no search constructors; its handler is
    mode-BLIND ([keymap] + the constructors below); its transition
    delegates wrapped actions back untouched.

    Mode-dependent keys carry BOTH readings ([By_mode]) and [resolve]
    picks one against the CURRENT model at apply time — a handler's view
    of the prompt is a per-frame snapshot, and a same-frame "/d" burst
    must not fire the idle binding (the codebase's burst-collapse
    class). Hosts that schedule effects off pane actions resolve first,
    so their scheduling sees the same choice as the transition. *)

type 'pane t =
  | Pane of 'pane (** the pane's own action, untouched by search *)
  | Prompt of
      { event : Prompt.event
      ; height : int
      }
  | Jump of
      { dir : int (** +1 next, -1 previous; wraps *)
      ; height : int
      }
  | By_mode of
      { if_active : 'pane t option (** [None] = the key means nothing there *)
      ; if_idle : 'pane t option
      }
[@@deriving sexp_of]

val pane : 'pane -> 'pane t
val when_active : 'pane t -> 'pane t
val when_idle : 'pane t -> 'pane t
val both : active:'pane t -> idle:'pane t -> 'pane t
val jump : height:int -> int -> 'pane t

(** Implicitly commit an active prompt; a no-op otherwise — safe to fire
    unconditionally (focus steals, clicks, the layout's L3 paths). *)
val implicit_commit : 'pane t

(** A [By_mode] action's reading under [search]'s CURRENT mode ([None] =
    the key means nothing in this mode); everything else passes
    through. *)
val resolve : search:Prompt.t -> 'pane t -> 'pane t option

(** The ([Widget.pane.search_active], [commit_search]) pair — the leaf
    surface every searchable pane exposes identically. *)
val surface
  :  inject:('pane t -> unit Effect.t) Bonsai.t
  -> search:Prompt.t Bonsai.t
  -> bool Bonsai.t * unit Effect.t Bonsai.t

(** The prompt keymap, whole: printables (ASCII and Uchar) append while
    active or fall to [idle_char]'s binding; [/] opens; [n]/[N] jump a
    committed register; Enter commits (or [enter_idle]); Esc cancels or
    clears the faded register; Backspace edits; Ctrl-p/Ctrl-n recall
    history. [None]: the key is the pane's own (arrows, wheel, clicks,
    pane chords). *)
val keymap
  :  height:int
  -> idle_char:(char -> 'pane t option)
  -> ?enter_idle:'pane t
  -> Event.t
  -> 'pane t option
