extern "C" {
#include "snptools.h"
}

#include <cstdio>

#include "parser.hh"

using namespace cppbrowser;

/* We don't really want this (Clang does its own file management)
 * but it is part of the API.
 */
static FILE *foo;

static Parser pr;

static int
parse()
{
  if (foo) {
    fclose(foo);
    foo = 0;
  }
  return pr.parse(sn_current_file());
}

static void
reset()
{
  pr.reset();
}

int
main(int argc, char **argv)
{
  char group[] = "c++";
  return sn_main(argc, argv, group, &foo, parse, reset);
}
