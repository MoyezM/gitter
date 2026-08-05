/* PTY spawn support for the embedded terminal (lib/term_pane/).
   forkpty lives in C so the multi-threaded OCaml/Async runtime never forks
   on the OCaml side: the child execs immediately. */
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

#if defined(__APPLE__)
#include <util.h>
#else
#include <pty.h>
#endif

/* spawn(rows, cols, cwd, term, command) -> (pid, master_fd)
   The command runs under /bin/sh -c with the pty as its controlling
   terminal. */
CAMLprim value gitter_pty_spawn(value v_rows, value v_cols, value v_cwd,
                                value v_term, value v_command) {
  CAMLparam5(v_rows, v_cols, v_cwd, v_term, v_command);
  CAMLlocal1(result);

  struct winsize ws;
  memset(&ws, 0, sizeof(ws));
  ws.ws_row = Int_val(v_rows);
  ws.ws_col = Int_val(v_cols);

  /* Copy OCaml strings before fork: the child must not touch the OCaml
     heap. */
  char *cwd = strdup(String_val(v_cwd));
  char *term = strdup(String_val(v_term));
  char *command = strdup(String_val(v_command));
  if (cwd == NULL || term == NULL || command == NULL)
    caml_failwith("gitter_pty_spawn: out of memory");

  int master = -1;
  caml_release_runtime_system();
  pid_t pid = forkpty(&master, NULL, NULL, &ws);
  if (pid == 0) {
    /* child: async-signal-safe territory — straight to exec */
    if (cwd[0] != '\0' && chdir(cwd) != 0) _exit(127);
    setenv("TERM", term, 1);
    unsetenv("TMUX");
    signal(SIGINT, SIG_DFL);
    signal(SIGQUIT, SIG_DFL);
    signal(SIGTSTP, SIG_DFL);
    execl("/bin/sh", "sh", "-c", command, (char *)NULL);
    _exit(127);
  }
  caml_acquire_runtime_system();
  free(cwd);
  free(term);
  free(command);

  if (pid < 0) uerror("forkpty", Nothing);

  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(pid));
  Store_field(result, 1, Val_int(master));
  CAMLreturn(result);
}

CAMLprim value gitter_pty_set_winsize(value v_fd, value v_rows, value v_cols) {
  CAMLparam3(v_fd, v_rows, v_cols);
  struct winsize ws;
  memset(&ws, 0, sizeof(ws));
  ws.ws_row = Int_val(v_rows);
  ws.ws_col = Int_val(v_cols);
  if (ioctl(Int_val(v_fd), TIOCSWINSZ, &ws) != 0) uerror("TIOCSWINSZ", Nothing);
  CAMLreturn(Val_unit);
}
