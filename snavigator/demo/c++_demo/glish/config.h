/* glish/config.h.  Generated automatically by configure.  */
/* custom config.h template - do not recreate using autoheader */

/* Define if you have <sys/wait.h> that is POSIX.1 compatible.  */
/* #undef HAVE_SYS_WAIT_H */

/* Define if you have <vfork.h>.  */
/* #undef HAVE_VFORK_H */

/* Define to `void*' if <sys/types.h> and <stdlib.h> don't define.  */
#define malloc_t void*

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef pid_t */

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define vfork as fork if vfork does not work.  */
#define vfork fork

/* Define if you have the gethostname function.  */
/* #undef HAVE_GETHOSTNAME */

#ifndef _AIX
/* Define if you have the setrlimit function.  */
/* #undef HAVE_SETRLIMIT */
#endif

/* Define if you have the setsockopt function.  */
/* #undef HAVE_SETSOCKOPT */

/* Define if you have the sigprocmask function.  */
/* #undef HAVE_SIGPROCMASK */

/* Define if you have the strdup function.  */
#define HAVE_STRDUP 1

/* Define if you have the uname function.  */
/* #undef HAVE_UNAME */

/* Define if you have the waitpid function.  */
/* #undef HAVE_WAITPID */

/* Define if you have the <X11/fd.h> header file.  */
/* #undef HAVE_X11_FD_H */

/* Define if you have the <libc.h> header file.  */
/* #undef HAVE_LIBC_H */

/* Define if you have the <sigLib.h> header file.  */
/* #undef HAVE_SIGLIB_H */

/* Define if you have the <sys/filio.h> header file.  */
/* #undef HAVE_SYS_FILIO_H */

/* Define if you have the <sys/select.h> header file.  */
/* #undef HAVE_SYS_SELECT_H */

/* Define if you have the <sys/signal.h> header file.  */
/* #undef HAVE_SYS_SIGNAL_H */

/* Define if you have the <unistd.h> header file.  */
#define HAVE_UNISTD_H 1
