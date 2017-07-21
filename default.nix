{ mkDerivation, base, directory, doctest, filepath, foldl, hindent
, hlint, MissingH, shellmate, split, stdenv, system-filepath, text
, transformers
}:
mkDerivation {
  pname = "acme-run";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base directory filepath foldl MissingH shellmate split text
    transformers
  ];
  executableHaskellDepends = [
    base hindent shellmate system-filepath text transformers
  ];
  testHaskellDepends = [ base doctest hlint ];
  homepage = "https://github.com/ericbmerritt/acme-run";
  description = "CLI for the metadrift client";
  license = stdenv.lib.licenses.asl20;
}
