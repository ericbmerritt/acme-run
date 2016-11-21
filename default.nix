{ mkDerivation, base, doctest, filepath, hindent, hlint, MissingH
, shelly, stdenv, text, transformers
}:
mkDerivation {
  pname = "acme-run";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base filepath MissingH shelly text transformers
  ];
  executableHaskellDepends = [
    base hindent shelly text transformers
  ];
  testHaskellDepends = [ base doctest hlint ];
  homepage = "https://github.com/ericbmerritt/acme-run";
  description = "CLI for the metadrift client";
  license = stdenv.lib.licenses.asl20;
}
