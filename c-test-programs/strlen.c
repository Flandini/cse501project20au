
#include "stdio.h"

size_t
strlen(char* s)
{
	size_t len = 0;
	for(unsigned long i = 0; s[i]; i++, len++) {}
	return len;
}

void
randomstr(char* s, size_t len)
{
	for (size_t i = 0; i < len - 1; ++i)
		s[i] = 'A';

	s[len - 1] = '\0';
}

int
main(int argc, char** argv)
{
	// improper null termination -> OOB read
	char* str1 = calloc(14, sizeof(char));
	char* str2 = str1 + 6;
	randomstr(str1, 14);
	printf("Size str1: %u\n", strlen(str1));
	printf("Size str2: %u\n", strlen(str2));
}
