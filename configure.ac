AC_INIT([Haskell libltdl interface], [0.1], [mainland@eecs.harvard.edu], [libltdl])

AC_CONFIG_SRCDIR([Foreign/LibLTDL.hs])

dnl--------------------------------------------------------------------
dnl Extra include and library directories
dnl--------------------------------------------------------------------
AC_ARG_WITH([extra-includes],
  [AC_HELP_STRING([--with-extra-includes],
    [directory containing includes])],
    [EXTRA_INCLUDE_DIRS=$withval; CPPFLAGS="$CPPFLAGS -I$withval"],
    [EXTRA_INCLUDE_DIRS=])

AC_ARG_WITH([extra-libraries],
  [AC_HELP_STRING([--with-extra-libraries],
    [directory containing libraries])],
    [EXTRA_LIB_DIRS=$withval; LDFLAGS="$LDFLAGS -L$withval"],
    [EXTRA_LIB_DIRS=])

AC_SUBST(EXTRA_INCLUDE_DIRS)
AC_SUBST(EXTRA_LIB_DIRS)

dnl--------------------------------------------------------------------
dnl Find libltdl
dnl--------------------------------------------------------------------
GBM_CHECK_LIBRARY(ltdl,,ltdl.h,lt_dlinit)

AC_SUBST(LTDL_INCLUDE_DIRS)
AC_SUBST(LTDL_LIB_DIRS)
AC_SUBST(HAVE_LIB_LTDL)
AC_SUBST(LTDL_LIBS)
AC_SUBST(HAVE_FRAMEWORK_LTDL)
AC_SUBST(LTDL_FRAMEWORK)

dnl--------------------------------------------------------------------
dnl Finish up
dnl--------------------------------------------------------------------
AC_CONFIG_FILES([libltdl.buildinfo])

AC_OUTPUT
