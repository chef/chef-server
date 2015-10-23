/* Copyright 2015 Chef Software, Inc.
 *
 * This file is provided to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License.  You may obtain
 * a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 * cs_escape: Does text-replacement on select ASCII punctionation
 * to avoid problems with CloudSearch's word-splitting rules.
 *
 */
#include <string.h>
#include <stdbool.h>
#include "erl_nif.h"

#pragma GCC diagnostic ignored "-Wmultichar"
// The first 32 ASCII/UTF8 Characters are control characters.  We
// don't expect to see them so we don't define replacements for them,
// shortening the array.

static int replacements[] = {
  '⨡', // ! 10785
  '„', // " 8222
  '⧺', //  # 10746
  '⨕', //  $ 10733
  '‰', //  % 2030
  '∫', //  & 8747
  0, // ' No replacement
  '⟨',  // ( 10216
  '⟩',  // ) 10217
  '⦁',  // * 10625
  '⧾',  // + 10750
  '‣',  // , 8227
  '–',  // - 8211
  0,  // . No replacement
  '⧸',  // / 10744
  // [0-9 - no replacements]
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  '⦂', // :  10626
  '⨟', //  ; 10783
  '⧼', //  < 10748
  '⩵', //  = 10869
  '⧽', //  > 10749
  '≟', //  ? 8799
  '⊕', //  @ 2295
  // [A-Z - no replacements]
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0,
  '⟦', // [ 10214
  '⧹', // \ 10745  Back Slash
  '⟧', // ] 10215
  '≏', // ^ 8783
  0, // _ no replacement
  '⨝', // ` 1078
  // [a-z - no replacements]
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0,
  '〈', // { 12296
  '|',  // ⫾  11006
  '〉', // } 12297
  '∼' // ~  8764
};

#define replacement_for(n) replacements[n - 33]

// 0x21- 0x2f is ! through /
// 0x3a - 0x40 is : through @
// 0x5b - 0x60 is [ through `
// 0x7b - 0x7e is { through ~
static inline bool has_replacement(int n) {
  return replacement_for(n) > 0;
}

static inline bool is_cs_safe(int n) {
  return (n == 0x23 || n == 0x24 || n == 0x25 || \
          n == 0x2c || n == 0x2f || n == 0x3b || \
          n == 0x3c || n == 0x3d || n == 0x3e ||
          n == 0x40 || n == 0x60);
}

static inline bool is_cs_term_safe(int n) {
  return (n == 0x2b || n == 0x2d || is_cs_safe(n));
}

static inline bool is_cs_phrase_safe(int n) {
  return (n == 0x2b || n == 0x2d || \
          n == 0x2a || n == 0x3f || \
          is_cs_safe(n));
}

typedef bool (*cs_escape_cond_func)(int character);

int cs_escape_if(int character, cs_escape_cond_func condPtr) {
  if (condPtr(character)) {
    return replacement_for(character);
  }
  return character;
}



ERL_NIF_TERM escape_nif_gen(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[], cs_escape_cond_func  funcPtr) {
  ERL_NIF_TERM retval;
  ERL_NIF_TERM head, tail;
  ERL_NIF_TERM * replacement_list = 0;
  int character, len;
  int pos = 0;

  if (argc != 1) {
      enif_make_badarg(env);
      return enif_make_string(env, "Error: no argument provided.", ERL_NIF_LATIN1);
  }
  if (!enif_get_list_length(env, argv[0], &len)) {
      enif_make_badarg(env);
      return enif_make_string(env, "Error: argument is not a list.", ERL_NIF_LATIN1);
  }

  replacement_list = enif_alloc(len * sizeof(ERL_NIF_TERM));
  head = argv[0];
  while (enif_get_list_cell(env, head, &head, &tail)) {
    if (!enif_get_int(env, head, &character)) {
      enif_free(replacement_list);
      enif_make_badarg(env);
      return enif_make_string(env, "Error: invalid character in list.", ERL_NIF_LATIN1);
    }
    replacement_list[pos] = enif_make_int(env, cs_escape_if(character, funcPtr));
  }
  retval = enif_make_list_from_array(env, replacement_list, len);
  enif_free(replacement_list);
  return retval;
}

static ERL_NIF_TERM escape_nif(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {
  return escape_nif_gen(env, argc, argv, &has_replacement);
}

static ERL_NIF_TERM escape_safe_nif(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {
  return escape_nif_gen(env, argc, argv, &is_cs_safe);
}

static ERL_NIF_TERM escape_term_safe_nif(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {
  return escape_nif_gen(env, argc, argv, &is_cs_term_safe);
}

static ERL_NIF_TERM escape_phrase_safe_nif(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {
  return escape_nif_gen(env, argc, argv, &is_cs_phrase_safe);
}


/* Nif Boiler Plate */

static ErlNifFunc nif_funcs[] = {
  {"escape", 1, escape_nif},
  {"escape_safe", 1, escape_safe_nif},
  {"escape_term_safe", 1, escape_term_safe_nif},
  {"escape_phrase_safe", 1, escape_phrase_safe_nif}
};

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
}

ERL_NIF_INIT(cs_escape, nif_funcs, load, reload, upgrade, unload)
