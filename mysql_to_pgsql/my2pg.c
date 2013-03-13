/* my2pg - Munge hex blobs from mysqldump for use in pg load
 *
 * Reads from stdin and writes to stdout. Transforms hex data in a way
 * that is equivalent to the following sed command:
 *
 *     sed "s/,0x\([0-9A-F]*\)/,decode('\1','hex')/g"
 *
 * The MAX_NODE_BYTES determines the size of the buffer used to store
 * the ASCII hex string representing serialized nodes. It is
 * hard-coded to 40GB which should be way more than enough. my2pg will
 * exit with status code 1 if it encounters node data that is too
 * large.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

/* 40MB */
int MAX_NODE_BYTES = 41943040;

/* Consume characters until next sep. Characters are stored in buf
 * assumed to have size MAX_NODE_BYTES. Exit if we attempt to consume
 * more than MAX_NODE_BYTES. */
int collect_until_sep(char *buf, int sep)
{
    int c, i = 0;

    while (EOF != (c = fgetc(stdin)) && i < MAX_NODE_BYTES) {
        if (c == sep) {
            break;
        }
        buf[i] = c;
        i++;
        if (i == MAX_NODE_BYTES) {
            fprintf(stderr, "FATAL: node data exceeded MAX_NODE_BYTES (%d)\n",
                    MAX_NODE_BYTES);
            exit(1);
        }
    }
    buf[i] = '\0';
    return i;
}

/* Treat buf like a fixed size three element stack and push another
 * char from stdin onto buf. Items are pushed onto the right. If the
 * stack is full, the left-most item is overwritten */
int fill_read_ahead_buf(char * buf)
{
    int c, fz = 2;
    if (buf[1]) {
        buf[0] = buf[1];
        if (buf[2]) {
            buf[1] = buf[2];
        } else {
            fz = 1;
        }
    }  else {
        fz = 0;
    }
    c = fgetc(stdin);
    if (EOF != c) {
        buf[fz] = c;
    } else {
        buf[fz] = '\0';
        return 0;
    }
    return 1;
}

int init_read_ahead_buf(char *buf)
{
    int c, i;
    bzero(buf, 4);
    for (i = 0; i < 3; i++) {
        c = fgetc(stdin);
        if (EOF == c) {
            buf[i] = '\0';
            return 0;
        }
        buf[i] = c;
    }
    buf[i] = '\0';
    return 1;
}

void emit_hex(char *buf, int len)
{
    printf(",decode('%s','hex'),", buf);
}

unsigned char decode(char x)
{
    if (x >= '0' && x <= '9')          /* 0-9 is offset by 48 */
      return (x - 0x30);
    else if (x >= 'A' && x <= 'F')    /* A-F offset by 55 */
      return(x - 0x37);
    else if (x >= 'a' && x <= 'f')   /* a-f offset by 87 */
        return(x - 0x57);
    else {                            /* Otherwise, an illegal hex digit */
        fprintf(stderr,"\nInput is not in legal hex format\n");
        exit(1);
    }
}

void emit_decoded_hex(char *buf, int len)
{
    unsigned char a, b, res;
    char *pos = buf;
    printf(",E'");
    while (*pos) {
        a = *pos;
        b = *(++pos);
        res = ((decode(a) * 16) & 0xF0) + (decode(b) & 0xF);
        if (res == 0x27)     /* we need to escape the ' with \' */
            printf("\\'");
        else
            putchar(res);
        pos++;
    }
    printf("',");
}

void my2pg(int decode)
{
    /* buf is used to accumulate serialized node hex data */
    char * buf = NULL;
    buf = (char *) malloc(MAX_NODE_BYTES);
    if (buf == NULL) {
        perror("unable to allocate memory for node buffer");
        exit(1);
    }
    /* Read Ahead Buffer of three chars + '\0' */
    char rab[4];
    int sep = ',', len;

    /* Start by reading up to 3 chars of input. Proceed by checking
     * wither we've encountered ",0x" in which case we collect up to
     * next ",", transform, and continue. Otherwise, write head of
     * rab stack and continue. */
    init_read_ahead_buf(rab);
    do {
        if (rab[0] == ',' && rab[1] == '0' && rab[2] == 'x') {
            /* We've found hex data, collect until next sep. XXX:
             * assumes hex data is not last column in input */
            len = collect_until_sep(buf, sep);
            if (decode) {
                emit_decoded_hex(buf, len);
            } else {
                emit_hex(buf, len);
            }
            init_read_ahead_buf(rab);
            fputc(rab[0], stdout);
        } else {
            /* emit head of read ahead "stack" rab */
            fputc(rab[0], stdout); 
        }
    } while (fill_read_ahead_buf(rab));
    printf("%s", rab);
}

int main(int argc, char *argv[])
{
    int decode = 0;
    if (argc == 2) {
        if (strncmp(argv[1], "users", 5) == 0) {
            decode = 1;
        }
    }
    my2pg(decode);
    fflush(NULL);
    return 0;
}
