#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "fast_xs_type.h"
#include "gcc.h"
#include "erl_nif.h"

/* pass-through certain characters for CP-1252 */
#define p(x) (x-128)

extern int escape(char * text);


static const int cp_1252[] = {
	8364,		/* 128 => 8364, euro sign */
	p(129),		/* 129 => 129,  pass-through */
	8218,		/* 130 => 8218, single low-9 quotation mark */
	402,		/* 131 =>  402, latin small letter f with hook */
	8222,		/* 132 => 8222, double low-9 quotation mark */
	8230,		/* 133 => 8230, horizontal ellipsis */
	8224,		/* 134 => 8224, dagger */
	8225,		/* 135 => 8225, double dagger */
	710,		/* 136 =>  710, modifier letter circumflex accent */
	8240,		/* 137 => 8240, per mille sign */
	352,		/* 138 =>  352, latin capital letter s with caron */
	8249,		/* 139 => 8249, single left-pointing angle quotation mark */
	338,		/* 140 =>  338, latin capital ligature oe */
	p(141),		/* 141 =>  141, pass-through */
	381,		/* 142 =>  381, latin capital letter z with caron */
	p(143),		/* 143 =>  143, pass-through */
	p(144),		/* 144 =>  144, pass-through */
	8216,		/* 145 => 8216, left single quotation mark */
	8217,		/* 146 => 8217, right single quotation mark */
	8220,		/* 147 => 8220, left double quotation mark */
	8221,		/* 148 => 8221, right double quotation mark */
	8226,		/* 149 => 8226, bullet */
	8211,		/* 150 => 8211, en dash */
	8212,		/* 151 => 8212, em dash */
	732,		/* 152 =>  732, small tilde */
	8482,		/* 153 => 8482, trade mark sign */
	353,		/* 154 =>  353, latin small letter s with caron */
	8250,		/* 155 => 8250, single right-pointing angle quotation mark */
	339,		/* 156 =>  339, latin small ligature oe */
	p(157),		/* 157 =>  157, pass-through */
	382,		/* 158 =>  382, latin small letter z with caron */
	376		/* 159 =>  376} latin capital letter y with diaeresis */
};

#define VALID_VALUE(n) \
	(n >= 0x20 && n <= 0xD7FF) || \
	    (n >= 0xE000 && n <= 0xFFFD) || \
	    (n >= 0x10000 && n <= 0x10FFFF)

#define CP_1252_ESCAPE(n) do { \
	if (n >= 128 && n <= 159) \
		n = cp_1252[n - 128]; \
	} while(0)

static inline size_t bytes_for(int n)
{
	if (n < 1000)
		return sizeof("&#999;") - 1;
	if (n < 10000)
		return sizeof("&#9999;") - 1;
	if (n < 100000)
		return sizeof("&#99999;") - 1;
	if (n < 1000000)
		return sizeof("&#999999;") - 1;
	/* if (n < 10000000), we won't have cases above 0x10FFFF */
	return sizeof("&#9999999;") - 1;
}

static size_t do_escape(unsigned char *buf, int n)
{

#define return_const_len(x) do { \
	memcpy(buf, x, sizeof(x) - 1); \
	return (sizeof(x) - 1); \
} while (0)
	/* handle ASCII first */
	if (likely(n < 128)) {
		if (likely(n >= 0x20 || n == '\t' || n == '\n' || n == '\r')) {
			if (unlikely(n == '"'))
				return_const_len("&quot;");
			if (unlikely(n == '&'))
				return_const_len("&amp;");
			if (unlikely(n == '<'))
				return_const_len("&lt;");
			if (unlikely(n == '>'))
				return_const_len("&gt;");
			buf[0] = (char)n;
			return 1;
		}

		buf[0] = '*';
		return 1;
	}

#undef return_const_len

	CP_1252_ESCAPE(n);

	if (VALID_VALUE(n)) {
		/* return snprintf(buf, sizeof("&#1114111;"), "&#%i;", n); */
		static const char digitmap[] = "0123456789";
		size_t rv = sizeof("&#;") - 1;
		buf += bytes_for(n);
		*--buf = ';';
		do {
			*--buf = digitmap[(int)(n % 10)];
			++rv;
		} while (n /= 10);
		*--buf = '#';
		*--buf = '&';
		return rv;
	}
	buf[0] = '*';
	return 1;
}

/*
 * escapes strings for XML
 * The double-quote (") character is translated to "&quot;"
 * string is the binary string from erlang, len is the initial length
 * result size is an out param that will get assigned the resulting
 * length of the string after it is escaped
 */
unsigned char * fast_xs(unsigned char * string, size_t len, size_t * result_size)
{
	long i;
	size_t s_len = len;
	unsigned char *tmp;
	unsigned char * rv;
        unsigned char * c;

	for (i = len-1; i>=0; --i) {
		int n = string[i];
		if (likely(n < 128)) {
			if (unlikely(n == '"'))
				s_len += (sizeof("&quot;") - 2);
			if (unlikely(n == '&'))
				s_len += (sizeof("&amp;") - 2);
			if (unlikely(n == '>' || n == '<'))
				s_len += (sizeof("&gt;") - 2);
			continue;
		}

		CP_1252_ESCAPE(n);

		if (VALID_VALUE(n))
			s_len += bytes_for(n) - 1;
	}
	rv = enif_alloc(s_len);
        *result_size = s_len;
        c = rv;
	for (tmp = string, i = len-1;i >= 0; --i,tmp++){
	    c += do_escape(c, *tmp);
        }
	return rv;
}

static ERL_NIF_TERM escape_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned char * result_binary_string, * dest_binary_data;
    ErlNifBinary input_binary_ro;
    ERL_NIF_TERM retval;
    size_t result_size = 0;

    if(!enif_inspect_binary(env, argv[0], &input_binary_ro)){
        return enif_make_badarg(env);
    }
    result_binary_string = fast_xs(input_binary_ro.data, input_binary_ro.size, &result_size);
    //enif_make_new_binary uses retval as an out parameter and returns
    //the data ptr from the resulting term.  We then copy into that
    //buffer the result of the expansion which is of size result_size
    dest_binary_data = enif_make_new_binary(env,result_size, &retval);
    memcpy(dest_binary_data, result_binary_string, result_size);

    enif_free(result_binary_string);

    return retval;
}


static ErlNifFunc nif_funcs[] = {
    {"escape", 1, escape_nif}
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

ERL_NIF_INIT(fast_xs, nif_funcs, load, reload, upgrade, unload)
