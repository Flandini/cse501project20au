#include <stdio.h>
#include <stdlib.h>


int
main(int argc, char** argv)
{
	unsigned int a = 0x12345678;
	unsigned short s = a;
	printf("0x%hx\n", s);
	return 0;
}
