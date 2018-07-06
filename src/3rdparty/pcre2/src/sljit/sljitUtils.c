/*
 *    Stack-less Just-In-Time compiler
 *
 *    Copyright Zoltan Herczeg (hzmester@freemail.hu). All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are
 * permitted provided that the following conditions are met:
 *
 *   1. Redistributions of source code must retain the above copyright notice, this list of
 *      conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright notice, this list
 *      of conditions and the following disclaimer in the documentation and/or other materials
 *      provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER(S) AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
 * SHALL THE COPYRIGHT HOLDER(S) OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* ------------------------------------------------------------------------ */
/*  Locks                                                                   */
/* ------------------------------------------------------------------------ */

#if (defined SLJIT_EXECUTABLE_ALLOCATOR && SLJIT_EXECUTABLE_ALLOCATOR) || (defined SLJIT_UTIL_GLOBAL_LOCK && SLJIT_UTIL_GLOBAL_LOCK)

#if (defined SLJIT_SINGLE_THREADED && SLJIT_SINGLE_THREADED)

#if (defined SLJIT_EXECUTABLE_ALLOCATOR && SLJIT_EXECUTABLE_ALLOCATOR)

static SLJIT_INLINE void allocator_grab_lock(void)
{
	/* Always successful. */
}

static SLJIT_INLINE void allocator_release_lock(void)
{
	/* Always successful. */
}

#endif /* SLJIT_EXECUTABLE_ALLOCATOR */

#if (defined SLJIT_UTIL_GLOBAL_LOCK && SLJIT_UTIL_GLOBAL_LOCK)

SLJIT_API_FUNC_ATTRIBUTE void SLJIT_CALL sljit_grab_lock(void)
{
	/* Always successful. */
}

SLJIT_API_FUNC_ATTRIBUTE void SLJIT_CALL sljit_release_lock(void)
{
	/* Always successful. */
}

#endif /* SLJIT_UTIL_GLOBAL_LOCK */

#elif defined(_WIN32) /* SLJIT_SINGLE_THREADED */

#include "windows.h"

#if (defined SLJIT_EXECUTABLE_ALLOCATOR && SLJIT_EXECUTABLE_ALLOCATOR)

static HANDLE allocator_mutex = 0;

static SLJIT_INLINE void allocator_grab_lock(void)
{
	/* No idea what to do if an error occures. Static mutexes should never fail... */
	if (!allocator_mutex)
		allocator_mutex = CreateMutex(NULL, TRUE, NULL);
	else
		WaitForSingleObject(allocator_mutex, INFINITE);
}

static SLJIT_INLINE void allocator_release_lock(void)
{
	ReleaseMutex(allocator_mutex);
}

#endif /* SLJIT_EXECUTABLE_ALLOCATOR */

#if (defined SLJIT_UTIL_GLOBAL_LOCK && SLJIT_UTIL_GLOBAL_LOCK)

static HANDLE global_mutex = 0;

SLJIT_API_FUNC_ATTRIBUTE void SLJIT_CALL sljit_grab_lock(void)
{
	/* No idea what to do if an error occures. Static mutexes should never fail... */
	if (!global_mutex)
		global_mutex = CreateMutex(NULL, TRUE, NULL);
	else
		WaitForSingleObject(global_mutex, INFINITE);
}

SLJIT_API_FUNC_ATTRIBUTE void SLJIT_CALL sljit_release_lock(void)
{
	ReleaseMutex(global_mutex);
}

#endif /* SLJIT_UTIL_GLOBAL_LOCK */

#elif defined(__OS2__)

/* Preserve BOOL definition in pcre2_internal.h */
#define BOOL __OS2_BOOL

#define INCL_BASE
#include <os2.h>

#undef BOOL

#if (defined SLJIT_EXECUTABLE_ALLOCATOR && SLJIT_EXECUTABLE_ALLOCATOR)

static HMTX allocator_mutex = NULLHANDLE;

static SLJIT_INLINE void allocator_grab_lock(void)
{
	/* No idea what to do if an error occures. Static mutexes should never fail... */
	if (allocator_mutex == NULLHANDLE)
		DosCreateMutexSem(NULL, &allocator_mutex, 0, TRUE);
	else
		DosRequestMutexSem(allocator_mutex, SEM_INDEFINITE_WAIT);
}

static SLJIT_INLINE void allocator_release_lock(void)
{
	DosReleaseMutexSem(allocator_mutex);
}

#endif /* SLJIT_EXECUTABLE_ALLOCATOR */

#if (defined SLJIT_UTIL_GLOBAL_LOCK && SLJIT_UTIL_GLOBAL_LOCK)

static HMTX global_mutex = NULLHANDLE;

SLJIT_API_FUNC_ATTRIBUTE void SLJIT_CALL sljit_grab_lock(void)
{
	/* No idea what to do if an error occures. Static mutexes should never fail... */
	if (global_mutex == NULLHANDLE)
		DosCreateMutexSem(NULL, &global_mutex, 0, TRUE);
	else
		DosRequestMutexSem(global_mutex, SEM_INDEFINITE_WAIT);
}

SLJIT_API_FUNC_ATTRIBUTE void SLJIT_CALL sljit_release_lock(void)
{
	DosReleaseMutexSem(global_mutex);
}

#endif /* SLJIT_UTIL_GLOBAL_LOCK */

#else /* __OS2__ */

#if (defined SLJIT_EXECUTABLE_ALLOCATOR && SLJIT_EXECUTABLE_ALLOCATOR)

#include <pthread.h>

static pthread_mutex_t allocator_mutex = PTHREAD_MUTEX_INITIALIZER;

static SLJIT_INLINE void allocator_grab_lock(void)
{
	pthread_mutex_lock(&allocator_mutex);
}

static SLJIT_INLINE void allocator_release_lock(void)
{
	pthread_mutex_unlock(&allocator_mutex);
}

#endif /* SLJIT_EXECUTABLE_ALLOCATOR */

#if (defined SLJIT_UTIL_GLOBAL_LOCK && SLJIT_UTIL_GLOBAL_LOCK)

#include <pthread.h>

static pthread_mutex_t global_mutex = PTHREAD_MUTEX_INITIALIZER;

SLJIT_API_FUNC_ATTRIBUTE void SLJIT_CALL sljit_grab_lock(void)
{
	pthread_mutex_lock(&global_mutex);
}

SLJIT_API_FUNC_ATTRIBUTE void SLJIT_CALL sljit_release_lock(void)
{
	pthread_mutex_unlock(&global_mutex);
}

#endif /* SLJIT_UTIL_GLOBAL_LOCK */

#endif /* __OS2__ */

/* ------------------------------------------------------------------------ */
/*  Stack                                                                   */
/* ------------------------------------------------------------------------ */

#if (defined SLJIT_UTIL_STACK && SLJIT_UTIL_STACK) || (defined SLJIT_EXECUTABLE_ALLOCATOR && SLJIT_EXECUTABLE_ALLOCATOR)

#ifdef _WIN32
#include "windows.h"
#elif !defined(__OS2__)
/* Provides mmap function. */
#include <sys/mman.h>
/* For detecting the page size. */
#include <unistd.h>

#ifndef MAP_ANON

#include <fcntl.h>

/* Some old systems does not have MAP_ANON. */
static sljit_s32 dev_zero = -1;

#if (defined SLJIT_SINGLE_THREADED && SLJIT_SINGLE_THREADED)

static SLJIT_INLINE sljit_s32 open_dev_zero(void)
{
	dev_zero = open("/dev/zero", O_RDWR);
	return dev_zero < 0;
}

#else /* SLJIT_SINGLE_THREADED */

#include <pthread.h>

static pthread_mutex_t dev_zero_mutex = PTHREAD_MUTEX_INITIALIZER;

static SLJIT_INLINE sljit_s32 open_dev_zero(void)
{
	pthread_mutex_lock(&dev_zero_mutex);
	/* The dev_zero might be initialized by another thread during the waiting. */
	if (dev_zero < 0) {
		dev_zero = open("/dev/zero", O_RDWR);
	}
	pthread_mutex_unlock(&dev_zero_mutex);
	return dev_zero < 0;
}

#endif /* SLJIT_SINGLE_THREADED */

#endif

#endif

#endif /* SLJIT_UTIL_STACK || SLJIT_EXECUTABLE_ALLOCATOR */

#if (defined SLJIT_UTIL_STACK && SLJIT_UTIL_STACK)

/* Planning to make it even more clever in the future. */
static sljit_sw sljit_page_align = 0;

SLJIT_API_FUNC_ATTRIBUTE struct sljit_stack* SLJIT_CALL sljit_allocate_stack(sljit_uw limit, sljit_uw max_limit, void *allocator_data)
{
	struct sljit_stack *stack;
	void *ptr;
#ifdef _WIN32
	SYSTEM_INFO si;
#endif

	SLJIT_UNUSED_ARG(allocator_data);
	if (limit > max_limit || limit < 1)
		return NULL;

#ifdef _WIN32
	if (!sljit_page_align) {
		GetSystemInfo(&si);
		sljit_page_align = si.dwPageSize - 1;
	}
#elif defined(__OS2__)
	if (!sljit_page_align) {
		ULONG pgsize = 0;
		if (DosQUerySysInfo(QSV_PAGE_SIZE, QSV_PAGE_SIZE, &pgsize, sizeof(pgsize)))
			pgsize = 4096; /* Should never happen. */
		sljit_page_align = pgsize - 1;
	}
#else
	if (!sljit_page_align) {
		sljit_page_align = sysconf(_SC_PAGESIZE);
		/* Should never happen. */
		if (sljit_page_align < 0)
			sljit_page_align = 4096;
		sljit_page_align--;
	}
#endif

	stack = (struct sljit_stack*)SLJIT_MALLOC(sizeof(struct sljit_stack), allocator_data);
	if (!stack)
		return NULL;

	/* Align max_limit. */
	max_limit = (max_limit + sljit_page_align) & ~sljit_page_align;

#ifdef _WIN32
	ptr = VirtualAlloc(NULL, max_limit, MEM_RESERVE, PAGE_READWRITE);
	if (!ptr) {
		SLJIT_FREE(stack, allocator_data);
		return NULL;
	}
	stack->max_limit = (sljit_u8 *)ptr;
	stack->base = stack->max_limit + max_limit;
	stack->limit = stack->base;
	if (sljit_stack_resize(stack, stack->base - limit)) {
		sljit_free_stack(stack, allocator_data);
		return NULL;
	}
#elif defined(__OS2__)
	APIRET arc = DosAllocMem(&ptr, max_limit, PAG_COMMIT | PAG_READ | PAG_WRITE | OBJ_ANY);
	if (arc)
		arc = DosAllocMem(&ptr, max_limit, PAG_COMMIT | PAG_READ | PAG_WRITE);
	if (arc) {
		SLJIT_FREE(stack, allocator_data);
		return NULL;
	}
	stack->max_limit = (sljit_u8 *)ptr;
	stack->base = stack->max_limit + max_limit;
	stack->limit = stack->base;
	if (sljit_stack_resize(stack, stack->base - limit)) {
		sljit_free_stack(stack, allocator_data);
		return NULL;
	}
#else
#ifdef MAP_ANON
	ptr = mmap(NULL, max_limit, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
#else
	if (dev_zero < 0) {
		if (open_dev_zero()) {
			SLJIT_FREE(stack, allocator_data);
			return NULL;
		}
	}
	ptr = mmap(NULL, max_limit, PROT_READ | PROT_WRITE, MAP_PRIVATE, dev_zero, 0);
#endif
	if (ptr == MAP_FAILED) {
		SLJIT_FREE(stack, allocator_data);
		return NULL;
	}
	stack->max_limit = (sljit_u8 *)ptr;
	stack->base = stack->max_limit + max_limit;
	stack->limit = stack->base - limit;
#endif
	stack->top = stack->base;
	return stack;
}

#undef PAGE_ALIGN

SLJIT_API_FUNC_ATTRIBUTE void SLJIT_CALL sljit_free_stack(struct sljit_stack *stack, void *allocator_data)
{
	SLJIT_UNUSED_ARG(allocator_data);
#ifdef _WIN32
	VirtualFree((void*)stack->max_limit, 0, MEM_RELEASE);
#elif defined(__OS2__)
	DosFreeMem((PVOID)stack->max_limit);
#else
	munmap((void*)stack->max_limit, stack->base - stack->max_limit);
#endif
	SLJIT_FREE(stack, allocator_data);
}

SLJIT_API_FUNC_ATTRIBUTE sljit_sw SLJIT_CALL sljit_stack_resize(struct sljit_stack *stack, sljit_u8 *new_limit)
{
	sljit_uw aligned_old_limit;
	sljit_uw aligned_new_limit;

	if ((new_limit < stack->max_limit) || (new_limit >= stack->base))
		return -1;
#ifdef _WIN32
	aligned_new_limit = (sljit_uw)new_limit & ~sljit_page_align;
	aligned_old_limit = ((sljit_uw)stack->limit) & ~sljit_page_align;
	if (aligned_new_limit != aligned_old_limit) {
		if (aligned_new_limit < aligned_old_limit) {
			if (!VirtualAlloc((void*)aligned_new_limit, aligned_old_limit - aligned_new_limit, MEM_COMMIT, PAGE_READWRITE))
				return -1;
		}
		else {
			if (!VirtualFree((void*)aligned_old_limit, aligned_new_limit - aligned_old_limit, MEM_DECOMMIT))
				return -1;
		}
	}
	stack->limit = new_limit;
	return 0;
#elif defined(__OS2__)
	aligned_new_limit = (sljit_uw)new_limit & ~sljit_page_align;
	aligned_old_limit = ((sljit_uw)stack->limit) & ~sljit_page_align;
	if (aligned_new_limit != aligned_old_limit) {
		if (aligned_new_limit < aligned_old_limit) {
			if (DosSetMem((PVOID)aligned_new_limit, aligned_old_limit - aligned_new_limit, PAG_COMMIT | PAG_DEFAULT))
				return -1;
		}
		else {
			if (DosSetMem((PVOID)aligned_old_limit, aligned_new_limit - aligned_old_limit, PAG_DECOMMIT))
				return -1;
		}
	}
	stack->limit = new_limit;
	return 0;
#else
	if (new_limit <= stack->limit) {
		stack->limit = new_limit;
		return 0;
	}
	aligned_new_limit = (sljit_uw)new_limit & ~sljit_page_align;
	aligned_old_limit = ((sljit_uw)stack->limit) & ~sljit_page_align;
	/* If madvise is available, we release the unnecessary space. */
#if defined(MADV_DONTNEED)
	if (aligned_new_limit > aligned_old_limit)
		madvise((void*)aligned_old_limit, aligned_new_limit - aligned_old_limit, MADV_DONTNEED);
#elif defined(POSIX_MADV_DONTNEED)
	if (aligned_new_limit > aligned_old_limit)
		posix_madvise((void*)aligned_old_limit, aligned_new_limit - aligned_old_limit, POSIX_MADV_DONTNEED);
#endif
	stack->limit = new_limit;
	return 0;
#endif
}

#endif /* SLJIT_UTIL_STACK */

#endif
