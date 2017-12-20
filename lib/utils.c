#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include "utils.h"

int32_t printBool(bool a) {
	printf("%s (bool)\n", a ? "true" : "false");
	return 0;
}

/************************************
	Node Methods
************************************/

/*
struct Node* createNode(int32_t id, int32_t type, ...) {
	struct Node* new = (struct Node*) malloc(sizeof(struct Node));
	new->id = id;
	new->type = type;

	va_list ap;
	va_start(ap, 1);
	switch (type) {
		case INT:
			new->a = va_arg(ap, int);	break;
		case FLOAT:
			new->b = va_arg(ap, double);	break;
		case BOOL:
			new->c = va_arg(ap, bool);	break;
		case STRING:
			new->d = va_arg(ap, char*);	break;
		default:
			break;
	}
  va_end(ap);
  return new;
}

void* nodeGetValue(struct Node* node, int32_t type) {
		if (node == NULL) {
			printf("[Error] Node doesn't exist!\n");
			exit(1);
		}
		void* res;
		switch (type) {
			case INT:
				if (node->type == INT)
					res = InttoVoid(node->a);
				else if (node->type == FLOAT)
					res = InttoVoid((int)node->b);
				else if (node->type == BOOL)
					res = InttoVoid( node->c ? 1 : 0 );
				else {
					res = InttoVoid(0);
				}
				break;
			case FLOAT:
				if (node->type == INT)
					res = FloattoVoid((double)node->a);
				else if (node->type == FLOAT)
					res = FloattoVoid(node->b);
				else if (node->type == BOOL)
					res = FloattoVoid( node->c ? 1 : 0 );
				else {
					res = FloattoVoid(0);
				}
				break;
			case BOOL:
				if (node->type == INT)
					res = BooltoVoid(node->a != 0);
				else if (node->type == FLOAT)
					res = BooltoVoid(node->b != 0);
				else if (node->type == BOOL)
					res = BooltoVoid(node->c);
				else {
					res = BooltoVoid(false);
				}
				break;
			case STRING:
				if (node->type == STRING)
					res = StringtoVoid(node->d);
				else{
					res = StringtoVoid("");
				}
				break;
			default:
				printf("[Error] Edge Value Type Error!\n");
				exit(1);
				break;
		}
		return res;
}

int32_t printNode(struct Node * node) {
	if (node == NULL) {
		printf("(null)\n");
		return 0;
	}
	switch (node->type) {
		case 0:
			printf("node%3d: %d\n", node->id, node->a);
			break;
		case 1:
			printf("node%3d: %f\n", node->id, node->b);
			break;
		case 2:
			printf("node%3d: %s\n", node->id, node->c ? "true" : "false");
			break;
		case 3:
			printf("node%3d: %s\n", node->id, node->d);
			break;
		default:
			printf("node%3d\n", node->id);
			break;
	}
	return 0;
}*/