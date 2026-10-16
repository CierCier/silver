#include "silver_ffi.h"

#include <string.h>

int main(void) {
    SilverBuffer buffer = {0};
    SilverError error = {0};
    const char *dir_name = "silver_ffi_smoke_dir";

    if (silver_ffi_abi_version() != SILVER_FFI_ABI_VERSION) {
        return 1;
    }

    silver_ffi_buffer_init(&buffer);
    silver_ffi_error_init(&error);
    silver_ffi_buffer_free(&buffer);
    silver_ffi_error_clear(&error);

    const uint8_t *message = silver_ffi_status_message(SILVER_FFI_ERR_PANIC);
    if (message == NULL || strlen((const char *)message) == 0) {
        return 2;
    }

    SilverSlice path = {
        .data = (const uint8_t *)dir_name,
        .len = (int64_t)strlen(dir_name),
    };
    if (silver_fs_create_dir(&path, &error) != SILVER_FFI_OK) {
        return 3;
    }
    if (silver_fs_create_dir(&path, &error) != SILVER_FFI_OK) {
        return 4;
    }
    int32_t exists = 0;
    if (silver_fs_exists(&path, &exists, &error) != SILVER_FFI_OK ||
        exists != 1) {
        return 5;
    }
    if (silver_fs_remove_dir(&path, &error) != SILVER_FFI_OK) {
        return 6;
    }
    if (silver_fs_exists(&path, &exists, &error) != SILVER_FFI_OK ||
        exists != 0) {
        return 7;
    }
    silver_ffi_error_clear(&error);
    return 0;
}
