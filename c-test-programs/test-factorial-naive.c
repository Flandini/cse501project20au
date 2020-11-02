#include "stdio.h"
#include "stdlib.h"

unsigned long long
factorial(unsigned long long n)
{
	unsigned long long z = 1;
	for (unsigned long long a = 1; a <= n; z *= a++);
	return z;
}

int
main(int argc, char** argv)
{
	for (unsigned long long i = 0; i < 40; ++i)
		printf("%llu : %llu\n", i, factorial(i));

	return 0;
}
