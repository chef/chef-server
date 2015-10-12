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

// The first 32 ASCII/UTF8 Characters are control characters.  We
// don't expect to see them so we don't define replacements for them,
// shortening the array.
#define replacement_for(n) replacements[n - 33]

static const char *replacements[] = {
  "__EX__", // !
  "__DQ__", // "
  "__HS__", // #
  "__DL__", // $
  "__PC__", // %
  "__AM__", // &
  "'", // ' No replacement
  "__OP__", // (
  "__CP__", // )
  "__ST__", // *
  "__PL__", // +
  "__CM__", // ,
  "__DS__", // -
  ".", // . No replacement
  "__FS__", // Foward Slash
  "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", // No replacement
  ":", // :
  "__SC__", // ;
  "__LT__", // <
  "__EQ__", // =
  "__GT__", // >
  "__QS__", // ?
  "__AT__", // @
  "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
  "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V",
  "W", "X", "Y", "Z", // No replacements
  "__OB__", // [
  "__BS__", // Back Slash
  "__CB__", // ]
  "__CA__", // ^
  "_", // _
  "__BT__", // `
  "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
  "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v",
  "w", "x", "y", "z", // No replacements
  "__OC__", // {
  "__PI__", // |
  "__CC__", // }
  "__TL__" // ~
};

// 0x21- 0x2f is ! through /
// 0x3a - 0x40 is : through @
// 0x5b - 0x60 is [ through `
// 0x7b - 0x7e is { through ~
static inline bool has_replacement(unsigned char n) {
  return ((n >= 0x21 && n <= 0x2f) || \
          (n >= 0x3a && n <= 0x40) || \
          (n >= 0x5b && n <= 0x60) || \
          (n >= 0x7b && n <= 0x7e));
}

static inline bool is_cs_safe(unsigned char n) {
  return (n == 0x23 || n == 0x24 || n == 0x25 || \
          n == 0x2c || n == 0x2f || n == 0x3b || \
          n == 0x3c || n == 0x3d || n == 0x3e ||
          n == 0x40 || n == 0x60);
}

static inline bool is_cs_term_safe(unsigned char n) {
  return (n == 0x2b || n == 0x2d || is_cs_safe(n));
}

static inline bool is_cs_phrase_safe(unsigned char n) {
  return (n == 0x2b || n == 0x2d || \
          n == 0x2a || n == 0x3f || \
          is_cs_safe(n));
}

static size_t do_replace(unsigned char *buf, int n) {
  size_t len = strlen(replacement_for(n));
  memcpy(buf, replacement_for(n), len);
  return len;
}

unsigned char * cs_escape_if(unsigned char * string, size_t len, size_t * res_size, bool (*condPtr)(unsigned char)) {
  long i;
  size_t s_len = len;
  unsigned char * rv;
  // Below we assgin tmp_rv = rv and tmp_string = string to avoid
  // incrementing the original pointers in the copy loop
  unsigned char * tmp_rv;
  unsigned char * tmp_string;

  for (i = len-1; i >=0; --i) {
    int n = string[i];
    if (condPtr(n)) {
      s_len += (strlen(replacements[n-33]) - 1);
    }
  }

  *res_size = s_len;
  // The caller is expected to call enif_free
  rv = enif_alloc(s_len);
  tmp_rv = rv;

  for (tmp_string = string, i = len - 1; i >= 0; --i, tmp_string++) {
    if (condPtr(*tmp_string)) {
      tmp_rv += do_replace(tmp_rv, *tmp_string);
    } else {
      *tmp_rv = *tmp_string;
      tmp_rv++;
    }
  }

  return rv;
}

unsigned char * cs_escape(unsigned char * string, size_t len, size_t * res_size) {
  return cs_escape_if(string, len, res_size, &has_replacement);
}

unsigned char * cs_escape_safe(unsigned char * string, size_t len, size_t * res_size) {
  return cs_escape_if(string, len, res_size, &is_cs_safe);
}

unsigned char * cs_escape_term_safe(unsigned char * string, size_t len, size_t * res_size) {
  return cs_escape_if(string, len, res_size, &is_cs_term_safe);
}

unsigned char * cs_escape_phrase_safe(unsigned char * string, size_t len, size_t * res_size) {
  return cs_escape_if(string, len, res_size, &is_cs_phrase_safe);
}

typedef unsigned char * (*cs_escape_func)(unsigned char * string, size_t len, size_t * res_size);

unsigned char * escape_nif_gen(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[], cs_escape_func funcPtr) {
  unsigned char * result_binary, * dest_binary;
  ErlNifBinary input_binary;
  ERL_NIF_TERM retval;
  size_t res_size = 0;

  if (!enif_inspect_binary(env, argv[0], &input_binary))
    return enif_make_badarg(env);

  result_binary = funcPtr(input_binary.data, input_binary.size, &res_size);
  dest_binary = enif_make_new_binary(env, res_size, &retval);
  memcpy(dest_binary, result_binary, res_size);
  enif_free(result_binary);
  return retval;
}


unsigned char * escape_nif(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {
  return escape_nif_gen(env, argc, argv, &cs_escape);
}

unsigned char * escape_safe_nif(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {
  return escape_nif_gen(env, argc, argv, &cs_escape_safe);
}

unsigned char * escape_term_safe_nif(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {
  return escape_nif_gen(env, argc, argv, &cs_escape_term_safe);
}

unsigned char * escape_phrase_safe_nif(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {
  return escape_nif_gen(env, argc, argv, &cs_escape_phrase_safe);
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
