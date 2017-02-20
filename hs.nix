{ mkDerivation, base, lifted-base, monad-control, stdenv, stm
, transformers-base
}:
mkDerivation {
  pname = "privileged-concurrency";
  version = "0.5";
  src = ./.;
  libraryHaskellDepends = [
    base lifted-base monad-control stm transformers-base
  ];
  description = "Provides privilege separated versions of the concurrency primitives";
  license = stdenv.lib.licenses.bsd3;
}
