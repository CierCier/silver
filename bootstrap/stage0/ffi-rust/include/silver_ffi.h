#ifndef SILVER_FFI_H
#define SILVER_FFI_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define SILVER_FFI_ABI_VERSION 1

/* Status values returned by every fallible entry point. */
#define SILVER_FFI_OK 0
#define SILVER_FFI_ERR_INVALID_ARGUMENT 1
#define SILVER_FFI_ERR_NULL_POINTER 2
#define SILVER_FFI_ERR_INVALID_UTF8 3
#define SILVER_FFI_ERR_IO 4
#define SILVER_FFI_ERR_PROCESS 5
#define SILVER_FFI_ERR_NOT_FOUND 6
#define SILVER_FFI_ERR_CALLBACK_STOPPED 7
#define SILVER_FFI_ERR_INVALID_HANDLE 8
#define SILVER_FFI_ERR_PANIC 9

/*
 * This is a C ABI, not a Rust ABI. Consumers must use the exact declarations
 * below and must not construct Rust Vec, String, Result, or trait values.
 * Text and paths are UTF-8 pointer/length pairs; they are not NUL-terminated.
 *
 * Initialize caller-owned output buffers and errors before their first use.
 * Fallible calls replace an existing error message, and output-buffer calls
 * release a previous buffer produced by this ABI before writing a new one.
 */

/* Borrowed bytes. The producer keeps the memory alive only for the call. */
typedef struct SilverSlice {
    const uint8_t *data;
    int64_t len;
} SilverSlice;

/* Rust-owned bytes. Only silver_ffi_buffer_free may release this memory. */
typedef struct SilverBuffer {
    uint8_t *data;
    int64_t len;
    int64_t cap;
} SilverBuffer;

/* Error text is another SilverBuffer and follows the same free rule. */
typedef struct SilverError {
    int32_t code;
    SilverBuffer message;
} SilverError;

typedef int32_t (*SilverChunkCallback)(void *userdata,
                                       const SilverSlice *chunk);

int32_t silver_ffi_abi_version(void);
/* Init is for a new/zeroed slot; it does not release an old buffer. */
void silver_ffi_buffer_init(SilverBuffer *buffer);
void silver_ffi_buffer_free(SilverBuffer *buffer);
/* Init is for a new/zeroed error; clear releases its message. */
void silver_ffi_error_init(SilverError *error);
void silver_ffi_error_clear(SilverError *error);

/* Process-lifetime NUL-terminated static text for a status code.
 * Never freed or mutated; bindings may keep the pointer borrowed. */
const uint8_t *silver_ffi_status_message(int32_t code);

int32_t silver_path_join(const SilverSlice *base,
                         const SilverSlice *child,
                         SilverBuffer *out,
                         SilverError *error);

int32_t silver_fs_exists(const SilverSlice *path,
                         int32_t *out_exists,
                         SilverError *error);
int32_t silver_fs_read_file(const SilverSlice *path,
                            SilverBuffer *out,
                            SilverError *error);
int32_t silver_fs_write_file(const SilverSlice *path,
                             const SilverSlice *contents,
                             SilverError *error);
int32_t silver_fs_remove_file(const SilverSlice *path,
                              SilverError *error);
/* Creates the directory and every missing parent; idempotent. */
int32_t silver_fs_create_dir(const SilverSlice *path,
                             SilverError *error);
/* Removes one empty directory. */
int32_t silver_fs_remove_dir(const SilverSlice *path,
                             SilverError *error);
int32_t silver_fs_read_file_callback(const SilverSlice *path,
                                     SilverChunkCallback callback,
                                     void *userdata,
                                     SilverError *error);

int32_t silver_env_get(const SilverSlice *key,
                       SilverBuffer *out,
                       SilverError *error);
int32_t silver_env_current_dir(SilverBuffer *out, SilverError *error);

/* out_handle must point to a null slot; a successful call transfers ownership. */
int32_t silver_process_spawn(const SilverSlice *program,
                             const SilverSlice *args,
                             int64_t arg_count,
                             void **out_handle,
                             SilverError *error);
int32_t silver_process_wait(void *handle,
                            int32_t *out_exit_code,
                            SilverError *error);
void silver_process_free(void **handle);

/* A callback chunk is borrowed and is valid only until the callback returns.
 * A non-zero callback result stops the read and returns
 * SILVER_FFI_ERR_CALLBACK_STOPPED. */

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
_Static_assert(sizeof(SilverSlice) == 16, "SilverSlice ABI changed");
_Static_assert(sizeof(SilverBuffer) == 24, "SilverBuffer ABI changed");
_Static_assert(sizeof(SilverError) == 32, "SilverError ABI changed");
#endif

#ifdef __cplusplus
}
#endif

#endif /* SILVER_FFI_H */
