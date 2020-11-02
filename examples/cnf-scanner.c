#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <assert.h>
#include <string.h>
#include <time.h>

int*
scan_line(char* line)
{
  char* buffer = calloc(255, sizeof(char));
  int buffer_idx = 0;
  int* tokens = calloc(65535, sizeof(int));
  int token_idx = 0;
  
  for (int i = 0; line[i]; i++)
    {
      if (isspace(line[i]))
	{
	  tokens[token_idx++] = atoi(buffer);
	  memset(buffer, 0, 255 * sizeof(char));
	  buffer_idx = 0;
	}
      else
	{
	  buffer[buffer_idx++] = line[i];
	}
    }
  
  memset(buffer, 0, 255 * sizeof(char));
  free(buffer);
  return tokens;
}

typedef struct String {
  size_t capacity;
  size_t length;
  char* body;
} String_t;

String_t*
String_init ()
{
  String_t* str = calloc(1, sizeof(String_t));
  str->capacity = 50;
  str->body = calloc(str->capacity, sizeof(char));
  return str;
}

String_t*
String_concat (const String_t* s1, const String_t* s2)
{
  String_t* dst = String_init ();
  dst->capacity = s1->capacity + s2->capacity;
  dst->length = s1->length;
  dst->body = realloc (dst->body, dst->capacity);

  strncpy (dst->body, s1->body, s1->length);
  strncpy (dst->body + dst->length, s2->body, s2->length);

  dst->length += s2->length;

  return dst;
}

void
String_double_capacity (String_t* str)
{
  str->capacity *= 2;
  str->body = realloc (str->body, str->capacity);
}

void
String_destroy (String_t* str)
{
  free(str->body);
  free(str);
}

String_t*
String_init_from_chars (const char* src)
{
  String_t* str = String_init();
  size_t len = strlen (src);

  while (len >= str->capacity)
    {
      String_double_capacity (str);
    }

  str->length = len;
  strncpy(str->body, src, len);

  return str;
}

String_t**
String_split (String_t* src, const char* sep)
{
  char* src_p = strndup (src->body, src->length + 1);
  String_t** arr = calloc (200, sizeof(String_t));
  size_t arr_idx = 0;

  char* current;

  while ( (current = strsep (&src_p, sep)) )
    {
      arr[arr_idx++] = String_init_from_chars (current);
    }

  free (src_p);
  return arr;
}

int
String_to_int (String_t* s)
{
  for (size_t i = 0; i < s->length; i++)
    {
      if (i == 0)
	{
	  assert (s->body[i] == '-' || isdigit(s->body[i]));
	  continue;
	}
      
      assert ( isdigit (s->body[i]) );
    }

  return atoi(s->body);
}

int*
better_scan_line(char* line)
{
  int* tokens = calloc (65535, sizeof(int));
  int token_idx = 0;
  char* line_p = strdup (line);
  
  char* current = strsep (&line_p, " ");
  tokens[token_idx++] = atoi(current);

  while ( (current = strsep (&line_p, " ")) )
    {
      tokens[token_idx++] = atoi(current);
    }

  return tokens;
}

int*
compiled_scan_line (char* line)
{
  String_t* line_str = String_init_from_chars (line);
  String_t** clauses = String_split (line_str, " ");
  
  int* tokens = calloc (600, sizeof(int));
  int tokens_idx = 0;
  
  for (int i = 0; clauses[i]; i++)
    {
      tokens[tokens_idx++] = String_to_int (clauses[i]);
    }

  free (clauses);
  free (line_str);
  
  return tokens;
}

int
main(int arg, char** argv)
{
  char* dimacs_lines = "-1 3 4 15 -16 0";
  //  int* tokens = scan_line (dimacs_lines);
  int* tokens;
  // better_scan_line(dimacs_lines);
  // int* tokens = compiled_scan_line (dimacs_lines);

  float start_time, end_time;
  float* times = calloc (100000, sizeof (float));

  for (int i = 0; i < 100000; i++)
    {
      start_time = (float) clock() / CLOCKS_PER_SEC;
      //      tokens = better_scan_line (dimacs_lines);
      tokens = compiled_scan_line (dimacs_lines);
      end_time = (float) clock() / CLOCKS_PER_SEC;
      
      free (tokens);
      times[i] = end_time - start_time;
    }

  double total = 0.0;
  for (int i = 0; i < 100000; i++)
    {
      total += times[i];
    }

  total = total / 100000.0;

  free (times);

  printf ("%f\n", total);

  return 0;
}
