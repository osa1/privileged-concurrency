{ mkDerivation, base, contravariant, lifted-base, monad-control
, stdenv, stm, transformers-base
}:
mkDerivation {
  pname = "privileged-concurrency";
  version = "0.6";
  sha256 = "0ns24fvxjdjlhqb0f9fh73r6q8al9ixi197nc30g5b2b7csnixv7";
  libraryHaskellDepends = [
    base contravariant lifted-base monad-control stm transformers-base
  ];
  description = "Provides privilege separated versions of the concurrency primitives";
  license = stdenv.lib.licenses.bsd3;
}
