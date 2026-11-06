/* Define an exported symbol whose address is zero. dlsym returns NULL for it
 * without reporting an error. */
__asm__(".globl libltdl_test_null_symbol\n"
        ".set libltdl_test_null_symbol, 0");
