#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#define MAX_LINE 2048

/* Linked List Application for Polynomial Equations
Menu:
1 → read/store equation
2 → print one equation
3 → print all equations
4 → add equations
5 → subtract equations
0 → quit
*/

typedef struct equNode *equPointer;
typedef struct equNode {
    int coef;
    char variable;
    int expon;
    equPointer link;
} EquNode;

typedef struct equHead *headPointer;
typedef struct equHead {
    int nodeNumber;
    char equNotation;
    equPointer link;
    headPointer hlink;
} EquHead;

static headPointer gEquations = NULL;

static void trim_newline(char *s) {
    size_t len = strlen(s);
    while (len > 0 && (s[len - 1] == '\n' || s[len - 1] == '\r')) {
        s[--len] = '\0';
    }
}

static void remove_spaces(const char *src, char *dst) {
    while (*src) {
        if (!isspace((unsigned char)*src)) {
            *dst++ = *src;
        }
        src++;
    }
    *dst = '\0';
}

static equPointer create_node(int coef, char variable, int expon) {
    equPointer node = (equPointer)malloc(sizeof(EquNode));
    if (!node) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }
    node->coef = coef;
    node->variable = variable;
    node->expon = expon;
    node->link = NULL;
    return node;
}

static void free_terms(equPointer p) {
    while (p) {
        equPointer next = p->link;
        free(p);
        p = next;
    }
}

static void free_all_equations(void) {
    while (gEquations) {
        headPointer next = gEquations->hlink;
        free_terms(gEquations->link);
        free(gEquations);
        gEquations = next;
    }
}

static int count_terms(equPointer p) {
    int count = 0;
    while (p) {
        count++;
        p = p->link;
    }
    return count;
}

static headPointer find_equation(char notation, headPointer *prevOut) {
    headPointer prev = NULL;
    headPointer cur = gEquations;
    while (cur) {
        if (cur->equNotation == notation) {
            if (prevOut) {
                *prevOut = prev;
            }
            return cur;
        }
        prev = cur;
        cur = cur->hlink;
    }
    if (prevOut) {
        *prevOut = NULL;
    }
    return NULL;
}

static void store_equation(char notation, equPointer terms) {
    headPointer prev = NULL;
    headPointer found = find_equation(notation, &prev);

    if (found) {
        free_terms(found->link);
        found->link = terms;
        found->nodeNumber = count_terms(terms);
        return;
    }

    headPointer head = (headPointer)malloc(sizeof(EquHead));
    if (!head) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }
    head->equNotation = notation;
    head->nodeNumber = count_terms(terms);
    head->link = terms;
    head->hlink = NULL;

    if (!gEquations) {
        gEquations = head;
    } else {
        headPointer cur = gEquations;
        while (cur->hlink) {
            cur = cur->hlink;
        }
        cur->hlink = head;
    }
}

static equPointer clone_terms(equPointer src) {
    equPointer head = NULL;
    equPointer tail = NULL;
    while (src) {
        equPointer node = create_node(src->coef, src->variable, src->expon);
        if (!head) {
            head = tail = node;
        } else {
            tail->link = node;
            tail = node;
        }
        src = src->link;
    }
    return head;
}

static void print_raw_equation(char notation, equPointer p) {
    printf("%c %d\n", notation, count_terms(p));
    while (p) {
        if (p->expon == 0) {
            printf("%d 0\n", p->coef);
        } else {
            printf("%d %d %c\n", p->coef, p->expon, p->variable);
        }
        p = p->link;
    }
}

static void print_single_term(int coef, char variable, int expon, int first) {
    int absCoef = (coef < 0) ? -coef : coef;

    if (first) {
        if (coef < 0) {
            putchar('-');
        }
    } else {
        putchar(coef < 0 ? '-' : '+');
    }

    if (expon == 0) {
        printf("%d", absCoef);
        return;
    }

    if (absCoef != 1) {
        printf("%d", absCoef);
    }
    putchar(variable);
    if (expon != 1) {
        printf("^%d", expon);
    }
}

static void print_polynomial(equPointer p) {
    int first = 1;
    if (!p) {
        printf("0");
        return;
    }
    while (p) {
        print_single_term(p->coef, p->variable, p->expon, first);
        first = 0;
        p = p->link;
    }
}

static void printEquation(char notation) {
    headPointer eq = find_equation(notation, NULL);
    if (!eq) {
        printf("NO EQUATION\n");
        return;
    }
    printf("%c=", notation);
    print_polynomial(eq->link);
    printf("\n");
}

static void printAll(void) {
    headPointer cur = gEquations;
    if (!cur) {
        printf("NO EQUATION\n");
        return;
    }
    while (cur) {
        printf("%c=", cur->equNotation);
        print_polynomial(cur->link);
        printf("\n");
        cur = cur->hlink;
    }
}

static int parse_equation_line(const char *line, char *notationOut, equPointer *termsOut) {
    char s[MAX_LINE];
    remove_spaces(line, s);

    if (strcmp(s, "0") == 0) {
        return 0;
    }

    size_t len = strlen(s);
    if (len < 3 || s[1] != '=') {
        return -1;
    }

    char notation = s[0];
    int i = 2;
    int prevExp = INT_MAX;
    equPointer head = NULL;
    equPointer tail = NULL;

    while (i < (int)len) {
        int sign = 1;
        int coef = 0;
        int expon = 0;
        char variable = '\0';
        int hasNumber = 0;

        if (s[i] == '+') {
            sign = 1;
            i++;
        } else if (s[i] == '-') {
            sign = -1;
            i++;
        }

        while (i < (int)len && isdigit((unsigned char)s[i])) {
            hasNumber = 1;
            coef = coef * 10 + (s[i] - '0');
            i++;
        }

        if (i < (int)len && isalpha((unsigned char)s[i])) {
            variable = s[i++];
            expon = 1;
            if (!hasNumber) {
                coef = 1;
            }
            coef *= sign;
            if (i < (int)len && s[i] == '^') {
                i++;
                if (i >= (int)len || !isdigit((unsigned char)s[i])) {
                    free_terms(head);
                    return -1;
                }
                expon = 0;
                while (i < (int)len && isdigit((unsigned char)s[i])) {
                    expon = expon * 10 + (s[i] - '0');
                    i++;
                }
            }
        } else if (hasNumber) {
            variable = '\0';
            expon = 0;
            coef *= sign;
        } else {
            free_terms(head);
            return -1;
        }

        if (coef == 0) {
            free_terms(head);
            return -1;
        }
        if (expon > prevExp) {
            free_terms(head);
            return -1;
        }

        equPointer node = create_node(coef, variable, expon);
        if (!head) {
            head = tail = node;
        } else {
            tail->link = node;
            tail = node;
        }
        prevExp = expon;

        if (i < (int)len && s[i] != '+' && s[i] != '-') {
            free_terms(head);
            return -1;
        }
    }

    *notationOut = notation;
    *termsOut = head;
    return 1;
}

static void eread_line(const char *line) {
    char notation;
    equPointer terms = NULL;
    int status = parse_equation_line(line, &notation, &terms);

    if (status == 0) {
        printf("quit\n");
        return;
    }
    if (status < 0) {
        printf("ERROR\n");
        return;
    }

    store_equation(notation, terms);
    print_raw_equation(notation, terms);
}

static void add_or_accumulate_term(equPointer *headRef, int coef, char variable, int expon) {
    if (coef == 0) {
        return;
    }

    equPointer prev = NULL;
    equPointer cur = *headRef;

    while (cur && cur->expon > expon) {
        prev = cur;
        cur = cur->link;
    }

    equPointer groupPrev = prev;
    equPointer groupCur = cur;
    while (groupCur && groupCur->expon == expon) {
        if (groupCur->variable == variable) {
            groupCur->coef += coef;
            if (groupCur->coef == 0) {
                if (groupPrev) {
                    groupPrev->link = groupCur->link;
                } else {
                    *headRef = groupCur->link;
                }
                free(groupCur);
            }
            return;
        }
        groupPrev = groupCur;
        groupCur = groupCur->link;
    }

    equPointer node = create_node(coef, variable, expon);
    if (groupPrev) {
        node->link = groupPrev->link;
        groupPrev->link = node;
    } else {
        node->link = *headRef;
        *headRef = node;
    }
}

static equPointer build_result(equPointer left, equPointer right, int signRight) {
    equPointer result = NULL;
    while (left) {
        add_or_accumulate_term(&result, left->coef, left->variable, left->expon);
        left = left->link;
    }
    while (right) {
        add_or_accumulate_term(&result, signRight * right->coef, right->variable, right->expon);
        right = right->link;
    }
    return result;
}

static int parse_operation(const char *line, char *target, char *lhs, char *rhs, char *op, int *hasTarget) {
    char s[MAX_LINE];
    remove_spaces(line, s);
    size_t len = strlen(s);

    if (len == 3 && (s[1] == '+' || s[1] == '-')) {
        *hasTarget = 0;
        *target = '\0';
        *lhs = s[0];
        *op = s[1];
        *rhs = s[2];
        return 1;
    }

    if (len == 5 && s[1] == '=' && (s[3] == '+' || s[3] == '-')) {
        *hasTarget = 1;
        *target = s[0];
        *lhs = s[2];
        *op = s[3];
        *rhs = s[4];
        return 1;
    }

    return 0;
}

static void handle_operation_line(const char *line, int isAddition) {
    char target, lhs, rhs, op;
    int hasTarget = 0;

    if (!parse_operation(line, &target, &lhs, &rhs, &op, &hasTarget)) {
        printf("ERROR\n");
        return;
    }
    if ((isAddition && op != '+') || (!isAddition && op != '-')) {
        printf("ERROR\n");
        return;
    }

    headPointer leftEq = find_equation(lhs, NULL);
    if (!leftEq) {
        printf("NO EQUATION %c\n", lhs);
        return;
    }

    headPointer rightEq = find_equation(rhs, NULL);
    if (!rightEq) {
        printf("NO EQUATION %c\n", rhs);
        return;
    }

    equPointer result = build_result(leftEq->link, rightEq->link, isAddition ? 1 : -1);

    if (hasTarget) {
        equPointer stored = clone_terms(result);
        store_equation(target, stored);
        printf("%c=", target);
        print_polynomial(result);
        printf("\n");
    } else {
        printf("%c%c%c=", lhs, isAddition ? '+' : '-', rhs);
        print_polynomial(result);
        printf("\n");
    }

    free_terms(result);
}

static int read_line(char *buf, size_t size) {
    if (!fgets(buf, (int)size, stdin)) {
        return 0;
    }
    trim_newline(buf);
    return 1;
}

int main(void) {
    char line[MAX_LINE];

    while (read_line(line, sizeof(line))) {
        char compact[MAX_LINE];
        remove_spaces(line, compact);
        if (compact[0] == '\0') {
            continue;
        }

        int choice = atoi(compact);
        if (choice == 0) {
            printf("quit\n");
            break;
        }

        switch (choice) {
            case 1:
                if (read_line(line, sizeof(line))) {
                    eread_line(line);
                }
                break;
            case 2:
                if (read_line(line, sizeof(line))) {
                    remove_spaces(line, compact);
                    if (compact[0] != '\0') {
                        printEquation(compact[0]);
                    }
                }
                break;
            case 3:
                printAll();
                break;
            case 4:
                if (read_line(line, sizeof(line))) {
                    handle_operation_line(line, 1);
                }
                break;
            case 5:
                if (read_line(line, sizeof(line))) {
                    handle_operation_line(line, 0);
                }
                break;
            default:
                break;
        }
    }

    free_all_equations();
    return 0;
}
