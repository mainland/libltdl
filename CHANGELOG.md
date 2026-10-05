# Changelog

## Unreleased

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
