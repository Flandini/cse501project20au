#include <string.h>
#include <stdint.h>
#include <ctype.h>
#include <stdlib.h>

char** str_split(char* in, char* delim)
{
    char** tokens = (char**) calloc(strlen(in) + 2, sizeof(char*));
    char* cur;
    size_t sep_tok_idx = 0;
    while ( (cur = strsep(&in, delim)) )
        tokens[sep_tok_idx++] = cur;
    return tokens;
}

size_t int_iter_length(int* iter)
{
    if (!(*iter)) return 0;
    size_t length;
    for (length = 0; iter[length]; length++);
    return length;
}

size_t char_iter_length(char** iter)
{
    if (!(*iter)) return 0;
    size_t length;
    for (length = 0; iter[length]; length++);
    return length;
}

int32_t* scan_line(char* clause_line) {
    char* lit;
    char* delim0 = " ";
    char* pass_through_0 = clause_line;
    char* pass_through_0_dup_0 = strdup(pass_through_0);
    char** iterator0 = str_split(pass_through_0_dup_0, delim0);
    int32_t* literals  = calloc(char_iter_length(iterator0), sizeof(int32_t));

    for (uint32_t i_0 = 0; (lit = iterator0[i_0]); ++i_0) {
        literals[i_0] = atoi(lit);
    }

    free (pass_through_0_dup_0);
    free (iterator0);
    return literals;
}