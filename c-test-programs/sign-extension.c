#include <stdio.h>
#include <stdlib.h>

void
my_snprintf(char* blah, size_t n, char* fmt, char* arg)
{
	printf("%zu\n", n);
	return;
}

int
main(int argc, char** argv)
{
	char len = -1;
	int extended_len = len;

	my_snprintf("blah", extended_len, "something", "something");

	unsigned int l;
	char c = -1;
	l = c;

	printf("%u\n", l);
	return 0;
}
