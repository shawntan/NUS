// Benjamin Tan Wei Hao
// U077129N

// Implement an interpreter for simple assignment statements.
// variables, integers are operators are treated as tokens.
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#define SYM_TABLE_SIZE 100

/* Symbol Table */
typedef struct {
	char identifier[100];
	int value;
} Symbol;

Symbol SymbolTable[SYM_TABLE_SIZE]; // Symbol table that can store 100 variables.
int numOfSymbols = 0;

enum { ADD_OP, SUBTRACT_OP, MULT_OP, DIV_OP, ASSIGN_OP };
enum { START_STATE, VARIABLE_STATE, INTEGER_STATE, OPERATOR_STATE, END_OPERATOR_STATE, ERROR_STATE, FINAL_STATE };

char M[10000];
char token[100];
char leftIdentifier[100];
char rightIdentifier[100];
int rightValue;
int operatorType;

int charIndex = 0;
int tokenIndex = 0;

char currChar, prevChar;
int prevState;
int currentState = START_STATE;

char nextChar();
void readInput();
bool isLetter(char);
bool isDigit(char);
bool isOperator(char);
bool isEqual(char);
bool isSemicolon(char);
bool isValidChar(char);


// Symbol table.

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


void readInput() {
	printf("Input the expression:\n");
	scanf("%s", M);
}

void concat(char c) {
	token[tokenIndex] = c;
	tokenIndex++;
}


int getState(char c) {
	prevState = currentState;
	
	if ( isLetter(c) ) { 
		if ( currentState == START_STATE || currentState == VARIABLE_STATE || currentState == END_OPERATOR_STATE ) {
			currentState = VARIABLE_STATE;
		} else {
			currentState = ERROR_STATE;
		}
	} else if ( isDigit(c) ) { 
		if ( currentState == VARIABLE_STATE ) {
			currentState = VARIABLE_STATE;
		} else if ( currentState == INTEGER_STATE || currentState == END_OPERATOR_STATE ) {
			currentState = INTEGER_STATE;
		} else {
			currentState = ERROR_STATE;
		}
	} else if ( isOperator(c) ) {
		if ( currentState == VARIABLE_STATE ) {

			// Save the left hand side variable
			strcpy(leftIdentifier, token);
			
			currentState = OPERATOR_STATE;
			switch(c) {
				case '+':
					operatorType = ADD_OP;
					break;
				case '-':
					operatorType = SUBTRACT_OP;
					break;
				case '*':
					operatorType = MULT_OP;
					break;
				case '/':
					operatorType = DIV_OP;
					break;
			}
		} else {
			currentState = ERROR_STATE;
		}
	} else if ( isEqual(c) ) {
		if ( currentState == OPERATOR_STATE ) {
			currentState = END_OPERATOR_STATE;
		} else {
			currentState = ERROR_STATE;
		}
		 
	} else if ( isSemicolon(c) ) { 
		if ( currentState == VARIABLE_STATE ) {
			currentState = START_STATE;
			strcpy(rightIdentifier, token);
			insertSymbolWithVariable(leftIdentifier, operatorType, rightIdentifier);
			
		} else if ( currentState == INTEGER_STATE ) {
			currentState = START_STATE;
			rightValue = atoi(token);
			insertSymbolWithValue(leftIdentifier, operatorType, rightValue);
		}
		// Reset all values;
		memset(leftIdentifier, 0, 100);
		memset(rightIdentifier, 0, 100);
		
	} else if ( c == '\0' ) {
		if ( currentState == START_STATE ) {
			currentState = FINAL_STATE;
		} else {
			currentState = ERROR_STATE;
		}
	} else {
		currentState = ERROR_STATE;
	}
	return currentState;
}

bool tokenize() {
	char c;
	while(1) {
		c = nextChar();
		printf("%c\n", c);
		switch ( getState(c) ) {
			case START_STATE:
				tokenIndex = 0;
				memset(token, 0, 100);
				break;
			case VARIABLE_STATE:
				if ( prevState != VARIABLE_STATE ) {
					tokenIndex = 0;
					memset(token, 0, 100);
				}
				concat(c);
				break;
			case INTEGER_STATE:
				if ( prevState != VARIABLE_STATE ) {
					tokenIndex = 0;
					memset(token, 0, 100);
				}
				concat(c);
				break;
			case OPERATOR_STATE:
				break;
			case END_OPERATOR_STATE:
				break;
			case FINAL_STATE:
				printf("Parse complete. Exiting.\n");
				return true;
			case ERROR_STATE:
				printf("Syntax error. Exiting.\n");
				return false;
			default:
				printf("Unexpected error!\n");
				return false;
		}
	}
}

char nextChar() {
	if (charIndex == 0) {
		currChar = M[charIndex];
		prevChar = '\0';
	} else {
		prevChar = M[charIndex-1];
		currChar = M[charIndex]; 
	}
	charIndex++;
	return currChar;
}


bool isLetter(char c) { return isalpha(c); }
bool isDigit(char c) { return isdigit(c); }
bool isOperator(char c) { return (c == '+' || c == '-' || c == '\\' || c == '*'); }
bool isEqual(char c) { return (c == '='); }
bool isSemicolon(char c) { return (c == ';'); }
bool isValidChar(char c) { return (isLetter(c) || isDigit(c) || isOperator(c) || isSemicolon(c)); }

int main() {
	readInput();
	if ( tokenize() ) {
		printSymbolTable();
	}
	
}