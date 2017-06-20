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
