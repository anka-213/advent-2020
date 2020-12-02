{ mkDerivation, base, relude, stdenv }:
mkDerivation {
  pname = "advent2020";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base relude ];
  executableHaskellDepends = [ base relude ];
  testHaskellDepends = [ base relude ];
  doCheck = false;
  homepage = "https://github.com/anka-213/advent-2020";
  description = "My Haskell solutions to Advent of Code 2020";
  license = stdenv.lib.licenses.mit;
}
