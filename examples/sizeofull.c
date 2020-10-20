#include "stdio.h"

int
main(void)
{
	printf("%lu\n", 2 ^ (8 * sizeof(unsigned long long)) - 1);
}
