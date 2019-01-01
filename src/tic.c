
// Tic Function

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern void sandbox(int *ins, int *outs);

int main(int argc, char **argv)
{	
	// argv[1]...argv[argc - 3] are inputs
	// argv[argc - 2] is number of outputs expected 
	// argv[argc - 1] is how times to loop
	int inc = argc - 3;
	int outc = atoi(argv[argc - 2]);
	int loop = atoi(argv[argc - 1]);
	int *ins = malloc(inc * sizeof(int));
	int *outs = malloc(outc * sizeof(int));

	int i;
	for (i = 1; i <= argc - 3; i++)
		ins[i] = atoi(argv[i]);
	

	for (i = 0; i < loop; i++){
		sandbox(ins, outs);
		for(int j = 0; j < outc; j++)
			printf("%d ", *(outs+j));
		printf("\n");
	}
	return 0;
}