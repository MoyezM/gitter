open! Core
module S = Gitter.Scrollbar

let check name cond = if not cond then failwithf "FAILED: %s" name ()

let () =
  check "fits -> no bar" (Option.is_none (S.thumb ~total:10 ~visible:10 ~offset:0));
  check "shorter -> no bar" (Option.is_none (S.thumb ~total:3 ~visible:10 ~offset:0));
  (* 100 rows in a 10-row window: 1-cell thumb sweeping the track. *)
  check "top" ([%equal: (int * int) option] (S.thumb ~total:100 ~visible:10 ~offset:0) (Some (0, 1)));
  check
    "bottom pins to the end"
    ([%equal: (int * int) option] (S.thumb ~total:100 ~visible:10 ~offset:90) (Some (9, 1)));
  (* Half visible: half-height thumb. *)
  check
    "thumb is proportional"
    ([%equal: (int * int) option] (S.thumb ~total:20 ~visible:10 ~offset:0) (Some (0, 5)));
  check
    "middle offset sits mid-track"
    (match S.thumb ~total:100 ~visible:10 ~offset:45 with
     | Some (p, 1) -> p = 4
     | _ -> false);
  check
    "huge totals keep a 1-cell thumb"
    (match S.thumb ~total:1_000_000 ~visible:10 ~offset:0 with
     | Some (_, len) -> len = 1
     | None -> false);
  check "offset overflow clamps" (Option.is_some (S.thumb ~total:100 ~visible:10 ~offset:9999))
;;

let () = print_endline "All scrollbar tests passed."
