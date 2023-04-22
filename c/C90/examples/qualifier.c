extern int getopt_long (int ___argc, char *const *___argv,
   const char *__shortopts,
          const struct option *__longopts, int *__longind)
       __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 3)));

int gpgrt_freopen (const char *__restrict__ path,
                              const char *__restrict__ mode,
                              int* __restrict__ stream);

typedef void (*evbuffer_file_segment_cleanup_cb)(
        struct evbuffer_file_segment const* seg, int flags, void* arg);

int *fts_open (char * const *, int,
               int (*)(const int **, const int **));