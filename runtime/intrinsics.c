/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                     Greta Yorsh, Jane Street Europe                    */
/*                                                                        */
/*   Copyright 2020 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/


#define CAML_INTERNALS

#include "caml/alloc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

/* #include <stdint.h> */

#if defined(TARGET_amd64) || defined(TARGET_i386)
#if defined(__GNUC__)
static uint64_t rdpmc(uint32_t c)
{
  uint32_t a, d;
  __asm__ __volatile__ ("rdpmc" : "=a" (a), "=d" (d) : "c" (c));
  return ((uint64_t)a) | (((uint64_t)d)<<32);
}

static uint64_t rdtsc()
{
  uint32_t hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return ((uint64_t)lo) | (((uint64_t)hi)<<32);
}
#elif #defined(_MSC_VER)
#include <intrin.h>
#pragma intrinsic(__rdtsc)

static uint64_t rdtsc() { return __rdtsc(); }

// The rdpmc intrinsic is available in kernel mode only, and the
// routine is only available as an intrinsic.
static uint64_t rdpmc(uint32_t c) { return 0; }
#else
static uint64_t rdpmc(uint32_t c) { return 0; }
static uint64_t rdtsc() { return 0; }
#endif
#else
static uint64_t rdpmc(uint32_t c) { return 0; }
static uint64_t rdtsc() { return 0; }
#endif

value caml_rdpmc_unboxed(value v1)
{ return rdpmc((uint32_t) v1); }

CAMLprim value caml_rdpmc(value v1)
{ return caml_copy_int64(rdpmc((uint32_t) (Int32_val(v1)))); }

value caml_rdtsc_unboxed(void)
{ return rdtsc(); }

CAMLprim value caml_rdtsc(value v1)
{ return caml_copy_int64(rdtsc()); }


