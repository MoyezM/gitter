/* libvterm bindings for the embedded terminal (lib/terminal/). One handle
   wraps a VTerm + its screen; output bytes (query responses AND encoded
   input — libvterm does the key/mouse encoding, honoring the app's modes)
   accumulate in a growable buffer the OCaml side drains after every call. */
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <stdlib.h>
#include <string.h>
#include <vterm.h>

typedef struct {
  VTerm *vt;
  VTermScreen *screen;
  char *out;
  size_t out_len, out_cap;
  int damaged;
  int cursor_visible;
} gt_term;

static void out_append(gt_term *t, const char *s, size_t len) {
  if (t->out_len + len > t->out_cap) {
    size_t cap = t->out_cap ? t->out_cap * 2 : 4096;
    while (cap < t->out_len + len) cap *= 2;
    t->out = realloc(t->out, cap);
    t->out_cap = cap;
  }
  memcpy(t->out + t->out_len, s, len);
  t->out_len += len;
}

static void output_cb(const char *s, size_t len, void *user) {
  out_append((gt_term *)user, s, len);
}

static int cb_damage(VTermRect rect, void *user) {
  (void)rect;
  ((gt_term *)user)->damaged = 1;
  return 1;
}
static int cb_moverect(VTermRect dest, VTermRect src, void *user) {
  (void)dest; (void)src;
  ((gt_term *)user)->damaged = 1;
  return 1;
}
static int cb_movecursor(VTermPos pos, VTermPos oldpos, int visible, void *user) {
  (void)pos; (void)oldpos; (void)visible;
  ((gt_term *)user)->damaged = 1;
  return 1;
}
static int cb_settermprop(VTermProp prop, VTermValue *val, void *user) {
  gt_term *t = user;
  if (prop == VTERM_PROP_CURSORVISIBLE) {
    t->cursor_visible = val->boolean;
    t->damaged = 1;
  }
  if (prop == VTERM_PROP_ALTSCREEN) t->damaged = 1;
  return 1;
}

static VTermScreenCallbacks screen_cbs = {
    .damage = cb_damage,
    .moverect = cb_moverect,
    .movecursor = cb_movecursor,
    .settermprop = cb_settermprop,
};

#define Term_val(v) (*(gt_term **)Data_custom_val(v))

static void gt_finalize(value v) {
  gt_term *t = Term_val(v);
  if (t) {
    vterm_free(t->vt);
    free(t->out);
    free(t);
  }
}

static struct custom_operations gt_ops = {
    .identifier = "gitter.vterm",
    .finalize = gt_finalize,
    .compare = custom_compare_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .compare_ext = custom_compare_ext_default,
    .fixed_length = custom_fixed_length_default,
};

CAMLprim value gt_vterm_new(value v_rows, value v_cols) {
  CAMLparam2(v_rows, v_cols);
  CAMLlocal1(res);
  gt_term *t = calloc(1, sizeof(gt_term));
  t->vt = vterm_new(Int_val(v_rows), Int_val(v_cols));
  if (t->vt == NULL) {
    free(t);
    caml_failwith("vterm_new failed");
  }
  vterm_set_utf8(t->vt, 1);
  vterm_output_set_callback(t->vt, output_cb, t);
  t->screen = vterm_obtain_screen(t->vt);
  t->cursor_visible = 1;
  vterm_screen_set_callbacks(t->screen, &screen_cbs, t);
  vterm_screen_enable_altscreen(t->screen, 1);
  vterm_screen_reset(t->screen, 1);
  res = caml_alloc_custom(&gt_ops, sizeof(gt_term *), 0, 1);
  Term_val(res) = t;
  CAMLreturn(res);
}

CAMLprim value gt_vterm_feed(value v_t, value v_bytes, value v_len) {
  CAMLparam3(v_t, v_bytes, v_len);
  vterm_input_write(Term_val(v_t)->vt, (const char *)Bytes_val(v_bytes),
                    Int_val(v_len));
  CAMLreturn(Val_unit);
}

CAMLprim value gt_vterm_take_output(value v_t) {
  CAMLparam1(v_t);
  CAMLlocal1(s);
  gt_term *t = Term_val(v_t);
  s = caml_alloc_string(t->out_len);
  memcpy(Bytes_val(s), t->out, t->out_len);
  t->out_len = 0;
  CAMLreturn(s);
}

CAMLprim value gt_vterm_take_damage(value v_t) {
  CAMLparam1(v_t);
  gt_term *t = Term_val(v_t);
  int d = t->damaged;
  t->damaged = 0;
  CAMLreturn(Val_bool(d));
}

CAMLprim value gt_vterm_resize(value v_t, value v_rows, value v_cols) {
  CAMLparam3(v_t, v_rows, v_cols);
  vterm_set_size(Term_val(v_t)->vt, Int_val(v_rows), Int_val(v_cols));
  CAMLreturn(Val_unit);
}

CAMLprim value gt_vterm_key_unichar(value v_t, value v_cp, value v_mods) {
  CAMLparam3(v_t, v_cp, v_mods);
  vterm_keyboard_unichar(Term_val(v_t)->vt, (uint32_t)Int_val(v_cp),
                         (VTermModifier)Int_val(v_mods));
  CAMLreturn(Val_unit);
}

/* Key codes: keep this switch in sync with Terminal.Vterm.Key. */
CAMLprim value gt_vterm_key_special(value v_t, value v_key, value v_mods) {
  CAMLparam3(v_t, v_key, v_mods);
  VTermKey key;
  int k = Int_val(v_key);
  switch (k) {
  case 0: key = VTERM_KEY_ENTER; break;
  case 1: key = VTERM_KEY_TAB; break;
  case 2: key = VTERM_KEY_BACKSPACE; break;
  case 3: key = VTERM_KEY_ESCAPE; break;
  case 4: key = VTERM_KEY_UP; break;
  case 5: key = VTERM_KEY_DOWN; break;
  case 6: key = VTERM_KEY_LEFT; break;
  case 7: key = VTERM_KEY_RIGHT; break;
  case 8: key = VTERM_KEY_INS; break;
  case 9: key = VTERM_KEY_DEL; break;
  case 10: key = VTERM_KEY_HOME; break;
  case 11: key = VTERM_KEY_END; break;
  case 12: key = VTERM_KEY_PAGEUP; break;
  case 13: key = VTERM_KEY_PAGEDOWN; break;
  default: key = (VTermKey)(VTERM_KEY_FUNCTION_0 + (k - 100)); break;
  }
  vterm_keyboard_key(Term_val(v_t)->vt, key, (VTermModifier)Int_val(v_mods));
  CAMLreturn(Val_unit);
}

CAMLprim value gt_vterm_mouse_move(value v_t, value v_row, value v_col,
                                   value v_mods) {
  CAMLparam4(v_t, v_row, v_col, v_mods);
  vterm_mouse_move(Term_val(v_t)->vt, Int_val(v_row), Int_val(v_col),
                   (VTermModifier)Int_val(v_mods));
  CAMLreturn(Val_unit);
}

CAMLprim value gt_vterm_mouse_button(value v_t, value v_button, value v_pressed,
                                     value v_mods) {
  CAMLparam4(v_t, v_button, v_pressed, v_mods);
  vterm_mouse_button(Term_val(v_t)->vt, Int_val(v_button), Bool_val(v_pressed),
                     (VTermModifier)Int_val(v_mods));
  CAMLreturn(Val_unit);
}

CAMLprim value gt_vterm_cursor(value v_t) {
  CAMLparam1(v_t);
  CAMLlocal1(res);
  gt_term *t = Term_val(v_t);
  VTermPos pos;
  vterm_state_get_cursorpos(vterm_obtain_state(t->vt), &pos);
  res = caml_alloc_tuple(3);
  Store_field(res, 0, Val_int(pos.row));
  Store_field(res, 1, Val_int(pos.col));
  Store_field(res, 2, Val_bool(t->cursor_visible));
  CAMLreturn(res);
}

/* Pack one row of cells into [buf]: 20 bytes per cell.
   [0]      utf8 length (0 = width-continuation cell, skip)
   [1..8]   utf8 bytes (base char + combining)
   [9]      attr bits: 1 bold, 2 italic, 4 underline, 8 reverse
   [10]     fg tag: 0 default, 1 rgb, 2 indexed
            [11..13] rgb, or [11] palette index
   [14]     bg tag (same scheme)       [15..17] rgb / [15] index
   [18]     cell width                  [19] pad

   Indexed colors are passed through UNRESOLVED (no convert_color_to_rgb):
   the renderer re-emits them as ANSI/xterm-256 indices so the HOST
   terminal resolves them against the user's own palette — the tmux trick
   that makes the embedded shell's colors match the outer terminal. */
static int put_utf8(unsigned char *dst, uint32_t cp) {
  if (cp < 0x80) {
    dst[0] = cp;
    return 1;
  } else if (cp < 0x800) {
    dst[0] = 0xC0 | (cp >> 6);
    dst[1] = 0x80 | (cp & 0x3F);
    return 2;
  } else if (cp < 0x10000) {
    dst[0] = 0xE0 | (cp >> 12);
    dst[1] = 0x80 | ((cp >> 6) & 0x3F);
    dst[2] = 0x80 | (cp & 0x3F);
    return 3;
  } else {
    dst[0] = 0xF0 | (cp >> 18);
    dst[1] = 0x80 | ((cp >> 12) & 0x3F);
    dst[2] = 0x80 | ((cp >> 6) & 0x3F);
    dst[3] = 0x80 | (cp & 0x3F);
    return 4;
  }
}

CAMLprim value gt_vterm_read_row(value v_t, value v_row, value v_cols,
                                 value v_buf) {
  CAMLparam4(v_t, v_row, v_cols, v_buf);
  gt_term *t = Term_val(v_t);
  int cols = Int_val(v_cols);
  unsigned char *buf = Bytes_val(v_buf);
  for (int c = 0; c < cols; c++) {
    unsigned char *cell_buf = buf + c * 20;
    memset(cell_buf, 0, 20);
    VTermScreenCell cell;
    VTermPos pos = {.row = Int_val(v_row), .col = c};
    if (!vterm_screen_get_cell(t->screen, pos, &cell)) continue;
    /* right half of a double-width char: leave len 0, renderer skips */
    if (cell.chars[0] == (uint32_t)-1) continue;
    int len = 0;
    for (int i = 0; i < VTERM_MAX_CHARS_PER_CELL && cell.chars[i]; i++) {
      if (len > 4) break; /* keep base + one combining mark max */
      len += put_utf8(cell_buf + 1 + len, cell.chars[i]);
    }
    if (len == 0 && cell.width > 0) {
      cell_buf[1] = ' ';
      len = 1;
    }
    cell_buf[0] = len;
    cell_buf[9] = (cell.attrs.bold ? 1 : 0) | (cell.attrs.italic ? 2 : 0) |
                  (cell.attrs.underline ? 4 : 0) | (cell.attrs.reverse ? 8 : 0);
    VTermColor fg = cell.fg, bg = cell.bg;
    if (!VTERM_COLOR_IS_DEFAULT_FG(&fg)) {
      if (VTERM_COLOR_IS_INDEXED(&fg)) {
        cell_buf[10] = 2;
        cell_buf[11] = fg.indexed.idx;
      } else {
        cell_buf[10] = 1;
        cell_buf[11] = fg.rgb.red;
        cell_buf[12] = fg.rgb.green;
        cell_buf[13] = fg.rgb.blue;
      }
    }
    if (!VTERM_COLOR_IS_DEFAULT_BG(&bg)) {
      if (VTERM_COLOR_IS_INDEXED(&bg)) {
        cell_buf[14] = 2;
        cell_buf[15] = bg.indexed.idx;
      } else {
        cell_buf[14] = 1;
        cell_buf[15] = bg.rgb.red;
        cell_buf[16] = bg.rgb.green;
        cell_buf[17] = bg.rgb.blue;
      }
    }
    cell_buf[18] = cell.width;
    if (cell.width == 0) cell_buf[0] = 0;
  }
  CAMLreturn(Val_unit);
}
