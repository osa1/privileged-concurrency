0.7.0
-----

* Added `Control.Concurrent...Class` modules with typeclasses for overloaded
  operations. Functions like `putMVar` now work on both `WriteOnlyMVar` and
  `MVar`.

* Export list updated. Importing `Control.Concurrent.Privilege` should now be
  enough for all use cases.

0.6.2
-----

* Added `tryReadTChan`, `peekTChan` and `tryPeekTChan` for read-only `TChan`s.

0.6.1
-----

* Added `tryReadMVar` for read-only `MVar`s.

0.6
---

* Write-only types are no longer `Eq`, but are now contravariant functors.

0.5
---

* Read-only types are no longer `Eq`, but are now functors.

0.4
---

* Functions are low lifted to `MonadBase IO`.

0.3
---

* Initial version with read-only and write-only versions of `Chan`, `MVar`,
  `TChan`, `TMVar` and `TVar`.
