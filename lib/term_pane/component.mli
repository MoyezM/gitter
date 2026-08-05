open! Core
open! Bonsai_term

(** The embedded editor: a fullscreen overlay hosting [$EDITOR <file>] in a
    [Terminal.Session] (real pty + libvterm), toggleable to the background
    (Ctrl-T both ways) while the process keeps running. Spawned in the
    working directory gitter was launched from. While an editor is live,
    opening another file only foregrounds the existing session — keys are
    never injected into a running editor. When the editor exits, any key
    returns to gitter (the 2s status poller picks up the file changes). *)

type t

val create : dimensions:Dimensions.t Bonsai.t -> local_ Bonsai.graph -> t

module Controls : sig
  type t =
    { open_file : string -> unit Effect.t
    ; toggle : unit Effect.t (** no-op until something has been opened *)
    }
end

val controls : t -> Controls.t Bonsai.t

(** Wrap the app: replaces the view and consumes all events while visible. *)
val wrap : t -> Widget.t -> Widget.t
