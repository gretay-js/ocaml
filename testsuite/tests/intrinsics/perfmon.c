#include <stdint.h>

uint64_t rdtsc()
{
  uint32_t hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return ((uint64_t)lo) | (((uint64_t)hi)<<32);
}

uint64_t rdpmc(uint64_t v)
{
  uint32_t a, d, c;
  c = (uint32_t) v;
  __asm__ __volatile__ ("rdpmc" : "=a" (a), "=d" (d) : "c" (c));
  return ((uint64_t)a) | (((uint64_t)d)<<32);
}

CAMLprim value rdpmc_instruction(value val_counter)
{ return Val_long(rdpmc(Long_val(val_counter))); }

CAMLprim value rdtsc()
{ return Val_long(rdtsc()); }
