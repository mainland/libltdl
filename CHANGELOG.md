# Changelog

## Unreleased

- Raise `DLException` in `dlSetSearchPath` and `dlForEachFile` for a directory
  whose name contains the path separator or NUL. Previously, libltdl split it
  at the separator into several directories, and a NUL silently truncated the
  search path.
- Raise `DLException` with a fixed message when libltdl fails without an error
  message, as `dlSym` does for a symbol whose address is zero. Previously, the
  bindings dereferenced a null pointer.
- Raise `DLException` for a file name that is empty or contains NUL in
  `dlOpen`, `dlOpenExt`, `dlOpenAdvise`, and `withDL`. Previously, `dlOpen`
  aborted the process on a libltdl assertion for an empty name or one beginning
  with NUL, and a NUL elsewhere silently truncated the name.
- Document the required process-wide synchronization contract, scoped handle
  lifetime, and callback locking constraints. Include a caller-owned lock
  example. The bindings do not acquire a lock.
- Document the directories that `dlForEachFile Nothing` searches.
- Append module-name extensions in `dlOpenAdvise` only when `Ext` is requested.
- Destroy advice even when an asynchronous exception arrives immediately after
  it is initialized. Earlier releases already destroyed it when setting a
  requested flag or opening the module failed.
- Catch exceptions from `dlForEachFile` callbacks and rethrow them after
  returning from C. Run each callback in the masking state of the thread that
  called `dlForEachFile`. Free callback pointers on all exits, including thread
  cancellation and exceptions in lazy callback results.
- Add `withDL` to initialize libltdl, open a module, and release resources when
  the callback returns or raises an exception. It closes ordinary handles and
  leaves resident handles open, because they cannot be closed. Use the wrapper
  in the sine example.
- Fix `dlExit` to call `lt_dlexit` instead of `lt_dlinit`. Earlier releases
  never shut libltdl down, so modules stayed loaded. The `dlExit` call that
  balances the last `dlInit` now unloads every module that is not resident.
  Resident modules stay loaded for the life of the process. Handles and symbol
  pointers obtained before that call must not be used after it, even those for
  resident modules. Calling through a pointer into an unloaded module can crash
  the program. A `dlExit` call without a matching `dlInit` now raises
  `DLException`.
- Report every nonzero status from libltdl as `DLException`, including positive
  error statuses that were previously ignored. Closing a resident module,
  including the main program opened by `dlOpen Nothing`, now reports failure.
- Add lifecycle, search-path, module-opening, and error tests on GHC 8.0+.
  Test scoped cleanup on normal return, exceptions, and thread cancellation.
  Run them sequentially because libltdl maintains process-wide state.
- Detect libltdl with Cabal instead of an Autoconf script. A missing libltdl
  now fails at configure time with an error naming the header and library.
  Previously, a missing header failed when hsc2hs preprocessed the bindings,
  and a missing library failed at link time. Cabal ignores options meant for
  the removed script, such as `--configure-option=--with-extra-includes=DIR`.
  Pass `--extra-include-dirs` and `--extra-lib-dirs` instead.
- Require GHC 8.0 or later (`base >= 4.9`). Remove compatibility code for
  older compilers.
- Update GitHub Actions CI to cover GHC 8.0 through 9.14 and enable library
  warnings.
- Use HTTPS for the source repository, correct package documentation, and
  include the README and changelog in source distributions.
