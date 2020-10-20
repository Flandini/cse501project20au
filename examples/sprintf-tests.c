#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int
main(int argc, char** argv)
{
	char* str = calloc(25, sizeof(char));
	char* longstr = calloc(26, sizeof(char));
	memset(longstr, 'A', 25);

	sprintf(str, "%s%s%s%s", "hello", longstr, ", ", "world");
	printf("%s\n",str);
	return 0;
}
