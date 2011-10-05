#
# GBM_CHECK_LIBRARY(LIB, FRAMEWORK, HEADER, FUNC)
#
# Find the given library/Mac OS X framework.
#
# Defines:
#
# $1_INCLUDE_DIRS
# $1_LIB_DIRS
# $1_HAVE_LIB
# $1_LIBS
# $1_HAVE_FRAMEWORK
# $1_FRAMEWORK
AC_DEFUN([GBM_CHECK_LIBRARY],
[define([GBM_INCLUDE_DIRS_NAME], translit($1_INCLUDE_DIRS, [[a-z *]], [[A-Z_P]]))
define([GBM_LIB_DIRS_NAME], translit($1_LIB_DIRS, [[a-z *]], [[A-Z_P]]))
define([GBM_HAVE_LIB_NAME], translit(HAVE_LIB_$1, [[a-z *]], [[A-Z_P]]))
define([GBM_LIBS_NAME], translit($1_LIBS, [[a-z *]], [[A-Z_P]]))
define([GBM_HAVE_FRAMEWORK_NAME], translit(HAVE_FRAMEWORK_$1, [[a-z *]], [[A-Z_P]]))
define([GBM_FRAMEWORK_NAME], translit($1_FRAMEWORK, [[a-z *]], [[A-Z_P]]))
AC_DEFINE([GBM_HAVE_LIB_NAME], [], [is the $1 library present])
AC_DEFINE([GBM_HAVE_FRAMEWORK_NAME], [], [is $1 the framework present])
AC_DEFINE([GBM_LIBS_NAME], [], [the $1 libraries])
AC_DEFINE([GBM_FRAMEWORK_NAME], [], [the $1 framework])
AC_MSG_CHECKING(for $1 library)
gbm_check_library_save_cppflags="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $X_CFLAGS"
gbm_check_library_save_ldflags="$LDFLAGS"
LDFLAGS="$LDFLAGS $X_LDFLAGS"
dnl
dnl Allow user to specify location of includes
dnl
AC_ARG_WITH([$1-includes],
  [AC_HELP_STRING([--with-$1-includes],
    [directory containing $3])],
    [gbm_check_library_include_dirs="$withval"; CPPFLAGS="$CPPFLAGS -I$withval"],
    [gbm_check_library_include_dirs=])
dnl
dnl Allow user to specify location of libraries
dnl
AC_ARG_WITH([$1-libraries],
  [AC_HELP_STRING([--with-$1-libraries],
    [directory containing $1 library])],
    [gbm_check_library_extra_libs="$withval"; LDFLAGS="$LDFLAGS -L$withval"],
    [gbm_check_library_extra_libs=])
dnl
dnl Find the header
dnl
AC_CHECK_HEADER([$3], , [gbm_check_library_have_lib=NO; gbm_check_library_have_framework=NO; gbm_check_library_libs=])
dnl
dnl Find the library
dnl
AC_CHECK_LIB([$1],  [$4],
             [gbm_check_library_have_lib=YES; gbm_check_library_libs=$1],
             [gbm_check_library_have_lib=NO;  gbm_check_library_libs=])
dnl
dnl Find the framework
dnl
case $target_os in
  darwin*)
    AC_MSG_CHECKING([for $2.framework])
    save_libs="$LIBS"
    LIBS="-framework $2"
    AC_TRY_LINK_FUNC($4,
      [gbm_check_library_have_framework=YES; gbm_check_library_framework=$2; gbm_check_library_libs=],
      [gbm_check_library_have_framework=NO])
    LIBS="$save_libs"
    AC_MSG_RESULT([$gbm_check_library_have_framework])
    ;;
esac
dnl
dnl Restore flags
dnl
CPPFLAGS="$gbm_check_library_save_cppflags"
LDFLAGS="$gbm_check_library_save_ldflags"
GBM_INCLUDE_DIRS_NAME=$gbm_check_library_include_dirs
GBM_EXTRA_LIB_DIRS_NAME=$gbm_check_library_libs
GBM_LIBS_NAME=$gbm_check_library_libs
GBM_FRAMEWORK_NAME=$gbm_check_library_framework
AC_DEFINE_UNQUOTED(GBM_HAVE_LIB_NAME, $gbm_check_library_have_lib, [])
AC_DEFINE_UNQUOTED(GBM_LIBS_NAME, $gbm_check_library_libs, [])
AC_DEFINE_UNQUOTED(GBM_HAVE_FRAMEWORK_NAME, $gbm_check_library_have_framework, [])
AC_DEFINE_UNQUOTED(GBM_FRAMEWORK_NAME, $gbm_check_library_framework, [])
undefine([GBM_INCLUDE_DIRS_NAME])dnl
undefine([GBM_LIB_DIRS_NAME])dnl
undefine([GBM_HAVE_LIB_NAME])dnl
undefine([GBM_LIBS_NAME])dnl
undefine([GBM_HAVE_FRAMEWORK_NAME])dnl
undefine([GBM_FRAMEWORK_NAME])dnl
])
