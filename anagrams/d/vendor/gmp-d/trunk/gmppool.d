/** \file gmppool.d 
 * \brief GMP memory management routines and object pools.
 *
 * Initializes gmp to use D's garbage collection memory management
 * and defines a template for managing pools of gmp objects for
 * high performance loops.
 */

/* Copyright Ben Hinkle 2003-2004 bhinkle4@juno.com, 
 * http://home.comcast.net/~benhinkle 
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Author makes no representations about
 * the suitability of this software for any purpose. It is provided
 * "as is" without express or implied warranty.
 */

import gmp;

//debug = MEM_DEBUG;

private import std.string;

/** 
 * \brief GMP object pool template.
 *
 * Each gmp class has two object pools. One pool is a list of 
 * recycled objects. Another is a list of temporary objects that
 * have been created as the result of an operation inside of a
 * <tt>begin</tt>/<tt>end</tt> section. See the mpz
 * class, for example, for typical usage of the mpz pool.
 */
class TGmpPool(T) {

  T[] freelist; // pool of objects ready to be recycled
  uint freen;   // number of objects in free pool

  /** Allocate an object from either D's allocator
   * or reuse an object that had been previously put in recycling.
   *
   * \return the allocated or recycled object
   */
  T allocate() {
    T result = null;
    if (freen > 0) {
      result = freelist[--freen];
      freelist[freen] = null;
      debug (MEM_DEBUG) {
	printf("recycling %x\n",result);
      }
    } else {
      result = new T();
      debug (MEM_DEBUG) {
	printf("allocated result %x\n",result);
      }
    }
    return result;
  }

  /** Put an object in the recycling pool.
   *
   * \param val the object to put in recycling
   */
  void recycle(T val) {
    if (val is null)
      return;
    debug (MEM_DEBUG) {
      printf("putting in recycling %x\n",val);
    }
    // add to freelist
    if (freen >= freelist.length) {
      freelist.length = freelist.length + 1;
    }
    freelist[freen++] = val;

  }
   
  /** Clear pool. Releases references to any allocated objects.
   */
  void clear_all() {
    freelist[] = null;
    freen = 0;
    freelist.length = 0;
  }
}

/* Garbage Collected allocation routines for GMP */

private extern (C) void *gmp_gc_alloc (uint len) {
  void *p = (void*)(&(new ubyte[len])[0]);
  debug (MEM_DEBUG) {
    printf("gmp_gc_alloc(%d) got %x \n", len, p);
  }
  return p;
}
private extern (C) void *gmp_gc_realloc (void*p,size_t oldlen, size_t len) {
  // can't use D's array resizing since we only have the pointer part.
  void *p2 = (void*)(&(new ubyte[len])[0]); 
  memcpy(p2,p,oldlen<len?oldlen:len);
  debug (MEM_DEBUG) {
    printf("gmp_gc_realloc(%x,%d,%d) got %x\n", p, oldlen, len, p2);
  }
  return p2;
}    
private extern (C) void gmp_gc_free (void *p, size_t len) {
  debug (MEM_DEBUG) {
    printf("gmp_gc_free(%x)\n",p);
  }
}

static this() {
  mp_set_memory_functions (&gmp_gc_alloc, &gmp_gc_realloc, &gmp_gc_free);
}
