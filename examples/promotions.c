#include <stdio.h>
#include <limits.h>

#define FIVE 5

int
main(int argc, char** argv)
{
	short n1 = 90;
	short n2 = 120;
	short n3 = -70;
	short n5 = 1000;
	unsigned long long n6 = INT_MAX;
	short n4 = n1 + n2 + n3 + n5;
	return n4;
}
