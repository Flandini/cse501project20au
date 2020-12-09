#include <string.h>
#include <stdint.h>
#include <ctype.h>
#include <stdlib.h>

char** StrSplit(char* in, char* delim)
{
    char** tokens = (char**) calloc(strlen(in) + 1, sizeof(char*));
    char* cur;
    size_t sep_tok_idx = 0;
    while ( (cur = strsep(&in, delim)) )
        tokens[sep_tok_idx++] = cur;
    return tokens;
}