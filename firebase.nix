{ mkDerivation, aeson, attoparsec, base, bytestring, criterion
, data-default, directory, either, firebase, generic-random
, hashids, HsOpenSSL, hspec, http-client, http-client-tls
, http-streams, http-types, io-streams, lens, MissingH, mtl
, nano-http, QuickCheck, safe, servant, servant-client
, servant-server, SHA, split, stdenv, string-conversions, text
, text-show, time, unordered-containers, uuid, wai, warp
}:
mkDerivation {
  pname = "firebase";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base bytestring data-default directory either
    firebase generic-random hashids HsOpenSSL hspec http-client
    http-client-tls http-streams http-types io-streams lens MissingH
    mtl nano-http QuickCheck safe servant servant-client servant-server
    SHA split string-conversions text text-show time
    unordered-containers uuid wai warp
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring data-default directory either
    firebase generic-random hashids HsOpenSSL hspec http-client
    http-client-tls http-streams http-types io-streams lens MissingH
    mtl nano-http QuickCheck safe servant servant-client servant-server
    SHA split string-conversions text text-show time
    unordered-containers uuid wai warp
  ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "https://github.com/nghamilton/firebase";
  description = "Google Firebase Database API";
  license = stdenv.lib.licenses.bsd3;
}
