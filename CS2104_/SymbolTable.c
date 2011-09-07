#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#define SYM_TABLE_SIZE 100
enum { ADD_OP, SUBTRACT_OP, MULT_OP, DIV_OP, ASSIGN_OP };

/* Symbol Table */
typedef struct {
	char identifier[100];
	int value;
} Symbol;

Symbol SymbolTable[SYM_TABLE_SIZE]; // Symbol table that can store 100 variables.
int numOfSymbols = 0;

int findSymbolInTable(char *identifier) {
	int i;
	for ( i = 0; i < numOfSymbols; i++ ) {
		if (strcmp( SymbolTable[i].identifier, identifier ) == 0 ) {
			return i;
		} 
	}
	return -1;
}

// Handle cases like variable[+|-|*|/]=integer
void insertSymbolWithValue(char *identifier, int operatorType, int value ) {	
	// Check if varible exists
	int i = findSymbolInTable( identifier );

	if ( i == -1 ) {
		strcpy( SymbolTable[numOfSymbols].identifier, identifier );
		SymbolTable[numOfSymbols].value = value;
		numOfSymbols++;
	} else {
		switch(operatorType) {
			case ASSIGN_OP:
			 	SymbolTable[i].value = value;			
				break;
			
			case ADD_OP:
				SymbolTable[i].value += value;
				break;
				
			case SUBTRACT_OP:
				SymbolTable[i].value -= value;
				break;
				
			case MULT_OP:
				SymbolTable[i].value *= value;
				break;
			
			case DIV_OP:
				SymbolTable[i].value /= value;
				break;
			
			default:
				printf("Unexpected operator. Exiting.\n");
		}
	}
}

// Handle cases like variable1[+|-|*|/]=variable2
void insertSymbolWithVariable(char *identifier1, int operatorType, char *identifier2 ) {
	
	// Check both identifiers exist, otherwise, intialize each of them to 0
	if ( findSymbolInTable( identifier1 ) == -1 ) { insertSymbolWithValue(identifier1, ASSIGN_OP, 0 ); }
	if ( findSymbolInTable( identifier2 ) == -1 ) { insertSymbolWithValue(identifier2, ASSIGN_OP, 0 ); }
	
	// At this point, both symbols exist.
	int i = findSymbolInTable( identifier1 );
	int j = findSymbolInTable( identifier2 ); 
	
	switch(operatorType) {
		case ASSIGN_OP:
		 	SymbolTable[i].value = SymbolTable[j].value;			
			break;
		
		case ADD_OP:
			SymbolTable[i].value += SymbolTable[j].value;
			break;
			
		case SUBTRACT_OP:
			SymbolTable[i].value -= SymbolTable[j].value;
			break;
			
		case MULT_OP:
			SymbolTable[i].value *= SymbolTable[j].value;
			break;
		
		case DIV_OP:
			SymbolTable[i].value /= SymbolTable[j].value;
			break;
		
		default:
			printf("Unexpected operator. Exiting.\n");
	}
}
void printSymbolTable() {
	int i;
	for ( i = 0; i < numOfSymbols; i++ ) {
		printf("%s = %d  ", SymbolTable[i].identifier, SymbolTable[i].value );
	}
	printf("\n");
}

int main() {

	insertSymbolWithValue("Hello", ASSIGN_OP, 3);
	insertSymbolWithValue("Darling", ASSIGN_OP, 2);
	insertSymbolWithValue("Huat", ADD_OP, 3);
	insertSymbolWithValue("Hello", MULT_OP, 4);
	insertSymbolWithVariable("Hello", SUBTRACT_OP, "HuatMe");
	
	
	printSymbolTable();
}




