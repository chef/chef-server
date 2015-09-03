#ifndef FAST_XS_TYPE_H
#define  FAST_XS_TYPE_H

/* I don't trust ctype.h when it comes to locale-independence: */

static __inline__ int is_hex(const int x)
{
	return (((x) >= '0' && (x) <= '9') ||
	       ((x) >= 'a' && (x) <= 'f') ||
	       ((x) >= 'A' && (x) <= 'F'));
}

static __inline__ int xtoupper(const int x)
{
	return (x >= 'a' && x <= 'f') ? (x & ~0x20) : x;
}

static __inline__ int hexchar_to_int(const int x)
{
	return (x < 'A') ? (x - '0') : (xtoupper(x) - 'A' + 10);
}

static __inline__ int hexpair_to_int(const int x1, const int x2)
{
	return ((hexchar_to_int(x1) << 4) | hexchar_to_int(x2));
}

#endif /* FAST_XS_TYPE_H */
