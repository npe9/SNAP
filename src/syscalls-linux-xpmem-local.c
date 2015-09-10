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
#include <stdint.h>
#include <xpmem.h>
#include <malloc.h>
#include <mcheck.h>

int cur_id, shared_id;

#define num_entries 4
#define PAGE_SIZE 4096
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
	xpmem_segid_t segid;
	char sharename[64];
};

struct usda *cte;

// make this global for now
xpmem_segid_t seg, seg1;
xpmem_apid_t apid;

// Returns the current time in seconds, as a double
double
now_(double *time)
{
	struct timeval tv;
	gettimeofday(&tv, NULL);
	*time = ((double)tv.tv_sec + (double)tv.tv_usec * 1.e-6);
	return 1;
}


void
blowup_()
{
	*(volatile int*)0 = 1;
}

void
printboom_()
{
	printf("index value %llx\n", *(uint64_t*)0x04407a00);
}
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
	int *buf;
	int i, status;
	char newseg[64];

	id_t my_id;
	//printf("setup1_\n");
	//printf("jury rigging segment");
	segment[23]=0;
	//printf("GOT SEGMENT %s\n", segment);
	if(cte == NULL){
		fprintf(stderr, "cte not initialized");
		exit(1);
	}

	cte->n[0] = *nx;
	cte->n[1] = *ny;
	cte->n[2] = *nz;
	cte->n[3] = *ng;
	//printf("*nx %d *ny %d *nz %d *ng %d\n", *nx, *ny, *nz, *ng);
	// we need to know, a priori how large the arrays are going to be
	cte->filesize = (*ng)*8+(*nx)*(*ny)*(*nz)*(*ng)*8;
	//printf("cte->filesize %d\n", cte->filesize);
	status = posix_memalign((void **)&ptr, 4096, cte->filesize /* 4096*32 */ );
	if(status != 0) {
		printf("ERROR: posix_memalign() failed status=%d\n", status);
		return -1;
	}
	snprintf(newseg, 64, "%s-file", segment);

	//printf("XPMEM MAKE cte->filesize %x rounded %x\n", cte->filesize, PAGE_ROUND_UP(cte->filesize)*64);
	//cte->filesize = 8192*4;
	seg1 = xpmem_make(ptr, PAGE_ROUND_UP(cte->filesize)*64, XPMEM_PERMIT_MODE, (void*)0777);
	if(seg1 <= 0){
		fprintf(stderr, "couldn't allocate shared xpmem buffer\n");
		exit(1);
	}
	//printf("XPMEM seg1 %lld\n", seg1);
	//cte->filesize = 8*(*ng);
	cte->mine = 0;
	while(!cte->mine){
		printf("%lld not mine yet\n", seg1);
		sleep(1);
	}
	cte->mine = 0;
	//printf("GOT LISTENER\n");
	// do I need to synchronize this?
	return 0;
}

int setup2_(double *dx, double *dy, double *dz) {
	cte->d[0] = *dx;
	cte->d[1] = *dy;
	cte->d[2] = *dz;
	return 1;
}

int share_init_(int *iproc, char *segment) {
	id_t my_id;
	char filename[10];
	char lockname[10];
	int i;
	int status;

	//printf("share_init_ iproc %p segment %s\n", iproc, segment);

	// XXX: does snap use different page sizes here?
	//printf("memaligned\n");
	status = posix_memalign((void **)&cte, 4096, sizeof(struct usda));
	if(status != 0) {
		printf("ERROR: posix_memalign() failed status=%d\n", status);
		return -1;
	}
	//printf("xpmem make\n");
	segment[23]=0;
	//printf("making segment should be %x\n",  4096*128);
	seg = xpmem_make(cte, 4096*128, XPMEM_PERMIT_MODE, (void*)0777);
	//printf("share_init: xpmem_make seg %lld\n", seg);
	if(seg <= 0){
		fprintf(stderr, "could not allocate metadata structure using xpmem");
		exit(1);
	}
	cte->outer = 0;
	cte->time_loop = 0;
	cte->finalized = 0;
	cte->meta_offset = 4096*(sizeof(struct usda)/4096)+
		(sizeof(struct usda)%4096 ? 4096 : 0);
	cte->filesize = 0;
	cte->niter = 0;
	// cte->id = arena_map_backed_region(cte->id,
	int pagesz = 4096;
	int nbacking = 4;
	int npages = 1;
	//rgn.start = 0;

	//ptr = (8*4096);
	char segid[32];
	switch (fork()){
	case 0:
		snprintf(segid, 32, "%.16lld", seg);
		char *arg[5] = {"taskset", "0x100000", "../../SNAP-analytics/analyze_linux_xpmem",  segid, NULL};
		if(execv("/bin/taskset", arg) < 0)
			perror("taskset");
		exit(-1);
	}

	return 1;
}

// so I need to know how big the flux and the v are.

int allocate_v_(unsigned long *size, void **data)
{
	*data = ptr;
	cte->meta_offset = *size*8;
	//memset(ptr, 0, *size);
	return 1;
}

int allocate_flux_(unsigned long *size, void **data)
{
	//printf("ptr %p cte->meta_offset %lld *size %lld\n", ptr, cte->meta_offset, *size);
	//for(;;);
	*data = ptr+cte->meta_offset;
	cte->filesize = (cte->meta_offset + *size)*8;
	//memset(*data, 0, *size);
	//printf("share_init_: xpmem_making ptr %llx size %llx name %s", ptr, PAGE_ROUND_UP(cte->filesize)*32, cte->sharename);
	cte->segid = xpmem_make(ptr, PAGE_ROUND_UP(cte->filesize)*32, XPMEM_PERMIT_MODE, (void*)0777);
	if(cte->segid <= 0){
		fprintf(stderr, "share_init_: couldn't allocate shared xpmem buffer\n");
		exit(1);
	}
	return 1;
}

/*
 * you are passing the time loop, but where do you get cte?
 *
 * */

int publish_(int *tint) {
	unsigned long pages, ret;
	id_t src, dst;
	int status;
	xpmem_segid_t seg;

	cte->time_loop  = *tint;
	cte->niter++;
	cte->segid = xpmem_make(ptr, PAGE_ROUND_UP(cte->filesize)*128, XPMEM_PERMIT_MODE, (void*)0777);
	if(cte->segid <= 0){
		fprintf(stderr, "share_init_: couldn't allocate shared xpmem buffer\n");
		exit(1);
	}
	while(!cte->mine) {
		//printf("not mine yet again niter %d cte->mine %d\n", cte->niter, cte->mine);
		sleep(1);
	}
	cte->mine = 0;
	//printf("publish: got mine\n");
	//total_time += pages;
	/* if(rank == 0) */
	/*  printf("%d, %d, %lu\n", *tint, *oint, pages); */
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
	char filename[10];

	cte->finalized = 1;
	// TODO(npe) destroy the xpmem?
	return 1;
}

int finalize_()
{
	cte->finalized = 1;
	return 1;
}

int create_shared_() {
	return 1;
}
