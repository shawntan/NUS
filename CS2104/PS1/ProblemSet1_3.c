// Benjamin Tan Wei Hao
// U077129N

// Problem Set 3

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>

int eax, esi;
unsigned char M[10000];
void exec();

int main() {
	/*** Linked list setup ***/
	/*
		Note: There is a possiblity of address overlap because of the randomly 
		 	  generated address. Therefore, multiple runs of the program _might_ 
			  be needed. 
	*/
	int list_data, val_addr;
	int curr_addr, next_addr;
	int LENGTH = 10;
	int index = 0;

	srand(time(NULL)); 
	
	esi = (rand() % (9900 + 1));
	
	while ( index < LENGTH ) {
	
		list_data = (rand() % (10));
		
		if ( index == 0 ) {
			curr_addr = esi; 	// Setup first address
		} else {
			curr_addr = next_addr;
		}
		 
		next_addr = (rand() % (9900 + 1));

		if ( index + 1 == LENGTH || (index == 0 && LENGTH == 1) ) {
			*(int*)&M[curr_addr] = list_data;
			*(int*)&M[curr_addr+4] = 0;
		} else {
			*(int*)&M[curr_addr] = list_data;
			*(int*)&M[curr_addr+4] = next_addr;
		}
		
		printf( "%4d [%d|%4d]\n", curr_addr, list_data, next_addr );
		index++;
	}
	
	exec();
	printf("eax: %d\n", eax);
}

/** 
	eax stores the return value
	esi is the current address of the linked element
**/

void exec() {
	eax = 0;
loop: 
	eax += *(int *)&M[esi];  // Value of linked list
	esi = *(int *)&M[esi+4]; // Value of next address
	if ( esi == 0 ) goto exit_loop;
	goto loop;
exit_loop: {}	
}