// Benjamin Tan Wei Hao
// U077129N

/*
	Problem Set 1 [1 mark]	

	Write a VAL program that converts a string made up of digit characters into 
	the unsigned numeric value corresponding to that string. 
	Your program should assume that the sting resides in memory at the address 
	pointed to by register esi, and should return the result in register eax.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h> 

unsigned char M[10000];
unsigned char a[10] = {'2','1','4','7','4','8', '3', '6', '4', '7'};
int eax, ebx, ecx, edx, esi, edi;

// eax : return value
// ebx : length of a
// ecx : multiplier
// esi : address of string
// edx : temp variable

void exec();

int main() {
	// Setup
	esi = 1234; // VAL address of string
	eax = 0;		// Return value
	ebx = 10;		// Length of a
	ecx = 1;
	memcpy(&M[esi], a, ebx);
	exec();
	printf("%d\n", eax);
}

void exec() {	
	ebx -= 1; 	// start index from the end	
loop:
	if (ebx < 0) goto exit_loop;
	edx = esi;
	edx += ebx; // calculate base address
	edi = M[edx];
	edi -= 48;  // calculate the numeric representation
	edi *= ecx;
	ecx *= 10;
	ebx -= 1;
	eax += edi; 
	goto loop;
exit_loop: {};
}