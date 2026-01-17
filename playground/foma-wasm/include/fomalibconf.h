/*   Foma: a finite-state toolkit and library.                                 */
/*   Copyright © 2008-2021 Mans Hulden                                         */

/*   This file is part of foma.                                                */

/*   Licensed under the Apache License, Version 2.0 (the "License");           */
/*   you may not use this file except in compliance with the License.          */
/*   You may obtain a copy of the License at                                   */

/*      http://www.apache.org/licenses/LICENSE-2.0                             */

/*   Unless required by applicable law or agreed to in writing, software       */
/*   distributed under the License is distributed on an "AS IS" BASIS,         */
/*   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  */
/*   See the License for the specific language governing permissions and       */
/*   limitations under the License.                                            */

struct state_array {
    struct fsm_state *transitions;
};

struct fsm_trans_list {
    short int in;
    short int out;
    int target;
    struct fsm_trans_list *next;
};

struct fsm_state_list {
    _Bool used;
    _Bool is_final;
    _Bool is_initial;
    short int num_trans;
    int state_number;
    struct fsm_trans_list *fsm_trans_list;
};

struct fsm_sigma_list {
    char *symbol;
};

struct fsm_sigma_hash {
    char *symbol;
    short int sym;
    struct fsm_sigma_hash *next;
};

typedef void *fsm_read_binary_handle;

struct fsm_construct_handle {
    struct fsm_state_list *fsm_state_list;
    int fsm_state_list_size;
    struct fsm_sigma_list *fsm_sigma_list;
    int fsm_sigma_list_size;
    struct fsm_sigma_hash *fsm_sigma_hash;
    int fsm_sigma_hash_size;
    int maxstate;
    int maxsigma;
    int numfinals;
    int hasinitial;
    char *name;
};

struct apply_med_handle {
    struct astarnode {
	short int wordpos;
	int fsmstate;
	short int f;
	short int g;
	short int h;
	int in;
	int out;
	int parent;
    } *agenda;
    int bytes_per_letter_array;
    uint8_t *letterbits;
    uint8_t *nletterbits;
    int astarcount;
    int heapcount;
    int heap_size;
    int agenda_size;
    int maxdepth;
    int maxsigma;
    int wordlen;
    int utf8len;
    int cost;
    int nummatches;
    int curr_state;
    int curr_g;
    int curr_pos;
    int lines;
    int curr_agenda_offset;
    int curr_node_has_match;
    int med_limit;
    int med_cutoff;
    int med_max_heap_size;
    int nodes_expanded;
    int *cm;
    char *word;
    char *instring;
    int instring_length;
    char *outstring;
    int outstring_length;
    char *align_symbol;
    int *heap;
    int *intword;
    struct sh_handle *sigmahash;
    struct state_array *state_array;
    struct fsm *net;
    struct fsm_state *curr_ptr;
    _Bool hascm;
};

struct apply_handle {

    int ptr;
    int curr_ptr;
    int ipos;
    int opos;
    int mode;
    int printcount;
    int *numlines;
    int *statemap;
    int *marks;

    struct sigma_trie {
	int signum;
	struct sigma_trie *next;
    } *sigma_trie;

    struct sigmatch_array {
	int signumber ;
	int consumes ;
    } *sigmatch_array;

    struct sigma_trie_arrays {
	struct sigma_trie *arr;
	struct sigma_trie_arrays *next;
    } *sigma_trie_arrays;

    int binsearch;
    int indexed;
    int state_has_index;
    int sigma_size;
    int sigmatch_array_size;
    int current_instring_length;
    int has_flags;
    int obey_flags;
    int show_flags;
    int print_space;
    char *space_symbol;
    char *separator;
    char *epsilon_symbol;
    int print_pairs;
    int apply_stack_ptr;
    int apply_stack_top;
    int oldflagneg;
    int outstringtop;
    int iterate_old;
    int iterator;
    uint8_t *flagstates;
    char *outstring;
    char *instring;
    struct sigs {
	char *symbol;
	int length;
    } *sigs;
    char *oldflagvalue;

    struct fsm *last_net;
    struct fsm_state *gstates;
    struct sigma *gsigma;
    struct apply_state_index {
	int fsmptr;
	struct apply_state_index *next;
    } **index_in, **index_out, *iptr;

    struct flag_list {
	char *name;
	char *value;
	short neg;
	struct flag_list *next;
    } *flag_list;

    struct flag_lookup {
	int type;
	char *name;
	char *value;
    } *flag_lookup ;

    struct searchstack {
	int offset;
	struct apply_state_index *iptr;
	int state_has_index;
	int opos;
	int ipos;
	int visitmark;
	char *flagname;
	char *flagvalue;
	int flagneg;
    } *searchstack ;
};


/* Automaton functions operating on fsm_state */
int add_fsm_arc(struct fsm_state *fsm, int offset, int state_no, int in, int out, int target, int final_state, int start_state);
struct fsm_state *fsm_state_copy(struct fsm_state *fsm_state, int linecount);

/* Functions for constructing a FSM arc-by-arc */
/* At the end of the constructions, the flags are updated automatically */

/* Call fsm_state_init with the alphabet size to initialize the new machine */
struct fsm_state *fsm_state_init(int sigma_size);

/* Call set current state before calling fsm_state_add_arc */
void fsm_state_set_current_state(int state_no, int final_state, int start_state);

/* Add an arc */
void fsm_state_add_arc(int state_no, int in, int out, int target, int final_state, int start_state);

/* Call fsm_state_close() when done with arcs to a state */
void fsm_state_close(struct fsm *net);

/* Call this when done with entire FSM */
void fsm_state_end_state();

struct state_array *map_firstlines(struct fsm *net);

FEXPORT void fsm_count(struct fsm *net);

void fsm_sort_lines(struct fsm *net);
void fsm_update_flags(struct fsm *net, int det, int pru, int min, int eps, int loop, int completed);

int sort_cmp(const void *a, const void *b);

int find_arccount(struct fsm_state *fsm);

/* Internal int stack */
int int_stack_isempty();
int int_stack_isfull();
void int_stack_clear();
int int_stack_find (int entry);
void int_stack_push(int c);
int int_stack_pop();
int int_stack_status();
int int_stack_size();

/* Internal ptr stack */
int ptr_stack_isempty();
void ptr_stack_clear();
void *ptr_stack_pop();
int ptr_stack_isfull();
void ptr_stack_push(void *ptr);

/* Sigma functions */
FEXPORT int sigma_add (char *symbol, struct sigma *sigma);
FEXPORT int sigma_add_number(struct sigma *sigma, char *symbol, int number);
FEXPORT int sigma_add_special (int symbol, struct sigma *sigma);
FEXPORT struct sigma *sigma_remove(char *symbol, struct sigma *sigma);
FEXPORT struct sigma *sigma_remove_num(int num, struct sigma *sigma);

int sigma_find (char *symbol, struct sigma *sigma);
int sigma_find_number (int number, struct sigma *sigma);
int sigma_substitute(char *orig, char *sub, struct sigma *sigma);
FEXPORT char *sigma_string(int number, struct sigma *sigma);
int sigma_sort (struct fsm *net);
void sigma_cleanup (struct fsm *net, int force);
FEXPORT struct sigma *sigma_create ();
int sigma_size(struct sigma *sigma);
FEXPORT int sigma_max(struct sigma *sigma);
struct fsm_sigma_list *sigma_to_list(struct sigma *sigma);

/* Debug */
void xprintf(char *string);

/* UTF8 */
unsigned char *utf8code16tostr(char *str);
int utf8iscombining(unsigned char *s);
int utf8skip(char *str);
int utf8strlen(char *str);
int ishexstr(char *str);
void decode_quoted(char *s);
void dequote_string(char *s);
char *remove_trailing(char *s, char c);
char *escape_string(char *string, char chr);
char *xstrrev(char *str);

/* Flag-related */
int flag_check(char *sm);
char *flag_get_name(char *string);
char *flag_get_value(char *string);
int flag_get_type(char *string);

/* Misc */
char *trim(char *string);
void strip_newline(char *s);
char *streqrep(char *s, char *oldstring, char *newstring);
char *xxstrndup(const char *s, size_t n);
int next_power_of_two(int v);
unsigned int round_up_to_power_of_two(unsigned int v);
