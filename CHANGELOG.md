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
