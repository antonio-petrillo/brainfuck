#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MEM_SIZE 30000

// see http://brainfuck.org/tests.b

typedef struct brainfuck_t {
  char memory[MEM_SIZE];
  unsigned short pointer;
  char *program;
  unsigned short ic;
} * brainfuck_t;

brainfuck_t init();
void destroy(brainfuck_t *bf);

void execute_program(brainfuck_t bf, char *program);
void execute_cycle(brainfuck_t bf, char instr);

int main(int argc, char **argv) {
  brainfuck_t bf = init();
  // hello world
  execute_program(bf, "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---"
                      ".+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.");

  execute_program(bf,
                  ">++++++++++>+>+[[+++++[>++++++++<-]>.<++++++[>--------<-]+<<"
                  "<]>.>>[[-]<[>+<-]>>[<<+>+>-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-["
                  ">+<-[>+<-[>+<-[>[-]>+>+<<<-[>+<-]]]]]]]]]]]+>>>]<<<]");

  destroy(&bf);
  return 0;
}

brainfuck_t init() {
  brainfuck_t bf = (brainfuck_t)malloc(sizeof(struct brainfuck_t));
  assert(bf);
  memset(bf->memory, 0, MEM_SIZE);
  bf->pointer = 0;
  bf->ic = 0;
  bf->program = NULL;
  return bf;
}

void destroy(brainfuck_t *bf) {
  assert(*bf);
  free(*bf);
  bf = NULL;
}

void execute_program(brainfuck_t bf, char *program) {
  assert(bf);
  if (bf->program) {
    free(bf->program);
    bf->program = NULL;
  }
  size_t program_len = strlen(program) + 1;
  bf->program = (char *)calloc(program_len, sizeof(char));
  assert(bf->program);
  strncpy(bf->program, program, program_len);
  char current_instruction;
  while ((current_instruction = bf->program[bf->ic]) != '\0') {
    execute_cycle(bf, current_instruction);
  }
}

void execute_cycle(brainfuck_t bf, char instr) {
  assert(bf);
  switch (instr) {
  case '+':
    bf->memory[bf->pointer] = (bf->memory[bf->pointer] + 1);
    break;
  case '-':
    bf->memory[bf->pointer] = (bf->memory[bf->pointer] - 1);
    break;
  case '.':
    putchar(bf->memory[bf->pointer]);
    break;
  case ',':
    bf->memory[bf->pointer] = getchar();
    getchar();
    break;
  case '>':
    bf->pointer++;
    break;
  case '<':
    bf->pointer--;
    break;
  case '[':
    if (bf->memory[bf->pointer] == 0) {
      unsigned short num_of_bracket = 0;
      char finished = 0;
      while (!finished) {
        bf->ic++;
        if (bf->program[bf->ic] == ']') {
          num_of_bracket == 0 ? finished = 1 : num_of_bracket--;
        } else if (bf->program[bf->ic] == '[') {
          num_of_bracket++;
        }
      }
    }
    break;
  case ']':
    if (bf->memory[bf->pointer] != 0) {
      unsigned short num_of_bracket = 0;
      char finished = 0;
      while (!finished) {
        bf->ic--;
        if (bf->program[bf->ic] == '[') {
          num_of_bracket == 0 ? finished = 1 : num_of_bracket--;
        } else if (bf->program[bf->ic] == ']') {
          num_of_bracket++;
        }
      }
    }
    break;
    // default:
    // line of comment do nothing
  }
  bf->ic++;
}
