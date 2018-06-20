{ mkDerivation, aeson, attoparsec, base, bytestring, criterion
, data-default, directory, either, generic-random, hashids
, HsOpenSSL, hspec, http-client, http-client-tls, http-streams
, http-types, io-streams, lens, MissingH, mtl, nano-http
, QuickCheck, safe, SHA, split, stdenv, string-conversions, text
, text-show, time, unordered-containers, uuid, wai, warp
}:
mkDerivation {
  pname = "firebase-database";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring data-default directory either
    generic-random hashids HsOpenSSL hspec http-client http-client-tls
    http-streams http-types io-streams lens MissingH mtl nano-http
    QuickCheck safe SHA split string-conversions text text-show time
    unordered-containers uuid wai warp
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring data-default directory either
    generic-random hashids HsOpenSSL hspec http-client http-client-tls
    http-types lens MissingH mtl nano-http QuickCheck safe SHA split
    string-conversions text text-show time unordered-containers uuid
  ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "https://github.com/nghamilton/firebase-database";
  description = "Google Firebase Database API";
  license = stdenv.lib.licenses.bsd3;
}
