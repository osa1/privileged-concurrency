{ mkDerivation, base, contravariant, lifted-base, monad-control
, stdenv, stm, transformers-base
}:
mkDerivation {
  pname = "privileged-concurrency";
  version = "0.6.1";
  sha256 = "0dky434kdnb84a4wxlx3jcg1f7c7g4xh0llfiqv48wpk7lwkaic2";
  libraryHaskellDepends = [
    base contravariant lifted-base monad-control stm transformers-base
  ];
  description = "Provides privilege separated versions of the concurrency primitives";
  license = stdenv.lib.licenses.bsd3;
}
