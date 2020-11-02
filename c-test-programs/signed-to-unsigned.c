#include <stdio.h>
#include <stdlib.h>

void
my_snprintf(char* buf, size_t n, char* fmt, char*arg);

int
main(int argc, char** argv)
{
	signed int a = 40;
	unsigned int b = a;
	unsigned long long c = a;
	printf ("%d %u %llu\n", a, b, c);

	signed int d = 1 << 31;
	printf("%d\n", d);

	signed short e = d;
	printf("%hd\n", e);
	return 0;
}

void
my_snprintf(char* buf, size_t n, char* fmt, char*arg)
{
	printf("Size is: %zu\n", n);
}
