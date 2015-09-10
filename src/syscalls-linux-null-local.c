#include <stdlib.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/time.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>
#include <malloc.h>
#include <mcheck.h>

int cur_id, shared_id;

#define num_entries 4
//#define PAGE_SIZE 4096
#define PAGE_ROUND_UP(x) ( (((ulong)(x)) + PAGE_SIZE-1)  & (~(PAGE_SIZE-1)) )

void analyze_();
int fd = 0;
int rank = 0;
sem_t *lock;
void * ptr;
size_t total_pages = 0;
unsigned long long offset = 0;
unsigned long long total_time=0;

struct usda {
	int outer;
	int time_loop;
	int finalized;
	unsigned long long filesize;
	size_t meta_offset;
	int n[4];
	double d[3];
	void *flux;
	void *v;
	int niter;
	int mine;
	id_t id;
};

struct usda *cte;

// Returns the current time in seconds, as a double
double
now_(double *time)
{
	struct timeval tv;
	gettimeofday(&tv, NULL);
	*time = ((double)tv.tv_sec + (double)tv.tv_usec * 1.e-6);
	return 1;
}


// make this global for now
/* Obtain a backtrace and print it to stdout. */
static void
print_trace (void)
{
	void *array[10];
	size_t size;
	char **strings;
	size_t i;

	size = backtrace (array, 10);
	strings = backtrace_symbols (array, size);

	printf ("Obtained %zd stack frames.\n", size);

	for (i = 0; i < size; i++)
		printf ("%s\n", strings[i]);

	free (strings);
}

void
abortfn(enum mcheck_status s)
{
	printf("mcheck aborting\n");
	switch(s) {
	case MCHECK_DISABLED:
		printf("mcheck disabled\n");
		break;
	case MCHECK_FREE:
		printf("mcheck free\n");
	case MCHECK_HEAD:
		printf("mcheck head\n");
	case MCHECK_OK:
		printf("mcheck ok\n");
	case MCHECK_TAIL:
		printf("mcheck tail\n");
	}

}

int mcheck_()
{
	//printf("MCHECKING\n");
	mcheck(abortfn);
	//printf("MCHECKED\n");
	return 1;
}
/* Prototypes for our hooks.  */
static void my_init_hook (void);
static void *my_malloc_hook (size_t, const void *);
static void my_free_hook (void*, const void *);

/* Override initializing hook from the C library. */
//void (*__MALLOC_HOOK_VOLATILE __malloc_initialize_hook) (void) = my_init_hook;
void  (*old_malloc_hook)(size_t size, const void *caller);
void (*old_free_hook) (void *ptr, const void *caller);

static void
my_init_hook (void)
{
	old_malloc_hook = __malloc_hook;
	old_free_hook = __free_hook;
	__malloc_hook = my_malloc_hook;
	__free_hook = my_free_hook;
}


static void *
my_malloc_hook (size_t size, const void *caller)
{
	void *result;
	/* Restore all old hooks */
	__malloc_hook = old_malloc_hook;
	__free_hook = old_free_hook;
	/* Call recursively */
	result = malloc (size);
	/* Save underlying hooks */
	old_malloc_hook = __malloc_hook;
	old_free_hook = __free_hook;
	/* printf might call malloc, so protect it too. */
	printf ("malloc: %p allocs %u returns %p\n", caller, (unsigned int) size, result);
	//print_trace();
	/* Restore our own hooks */
	__malloc_hook = my_malloc_hook;
	__free_hook = my_free_hook;
	return result;
}

static void
my_free_hook (void *ptr, const void *caller)
{
	/* Restore all old hooks */
	__malloc_hook = old_malloc_hook;
	__free_hook = old_free_hook;
	/* Call recursively */

	printf ("freeing pointer %p\n", ptr);
	free (ptr);
	/* Save underlying hooks */
	old_malloc_hook = __malloc_hook;
	old_free_hook = __free_hook;
	/* printf might call free, so protect it too. */
	printf ("freed pointer %p\n", ptr);
	/* Restore our own hooks */
	__malloc_hook = my_malloc_hook;
	__free_hook = my_free_hook;
}

int mmap_off_()
{
	if(mallopt(M_MMAP_MAX, 0) <= 0){
		printf("mallopt failed\n");
		exit(-1);
	}
	return 1;
}

int setup1_(int *nx, int *ny, int *nz, int *ng, char *segment) {
	return 0;
}

int setup2_(double *dx, double *dy, double *dz) {
	return 1;
}

int share_init_(int *iproc, char *segment) {
		return 1;
}

// so I need to know how big the flux and the v are.

int allocate_v_(unsigned long *size, void **data)
{
	*data = malloc(*size*sizeof(double));
	return 1;
}

int allocate_flux_(unsigned long *size, void **data)
{
	*data = malloc(*size*sizeof(double));
	return 1;
}

/*
 * you are passing the time loop, but where do you get cte?
 *
 * */

int publish_(int *tint) {
		return 1;
}

/* int remap_ (int *size, int *fd, void **data, int *offset) { */
/*  munmap(*data, *size); */
/*  *data = NULL; */
/*  *data = mmap(NULL, *size, PROT_READ | PROT_WRITE, MAP_PRIVATE, *fd, *offset); */
/* } */

/* int unmap_(int *size, void **data) { */
/*  munmap(*data, *size); */
/* } */

int deallocate_()
{
	return 1;
}

int finalize_()
{
	return 1;
}

int create_shared_() {
	return 1;
}
