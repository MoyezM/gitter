(* Locates libvterm via pkg-config (provided by the nix dev shell) and
   writes the C flags the gitter library's stubs need. Falls back to bare
   -lvterm so the error outside the dev shell is a clear link failure, not
   a confusing configurator crash. *)
module C = Configurator.V1

let () =
  C.main ~name:"vterm" (fun c ->
    let default : C.Pkg_config.package_conf = { libs = [ "-lvterm" ]; cflags = [] } in
    let conf =
      match C.Pkg_config.get c with
      | None -> default
      | Some pc ->
        (match C.Pkg_config.query pc ~package:"vterm" with
         | None -> default
         | Some conf -> conf)
    in
    C.Flags.write_sexp "vterm_c_flags.sexp" conf.cflags;
    C.Flags.write_sexp "vterm_c_library_flags.sexp" conf.libs)
