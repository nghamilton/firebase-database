{ mkDerivation, aeson, base, bytestring, fetchgit, http-types, lens
, mtl, nano-http, stdenv, text
}:
mkDerivation {
  pname = "firebase";
  version = "0.2.0";
  src = fetchgit {
    url = "https://github.com/collegevine/firebase.git";
    sha256 = "0chh5fd8b5xn4lcl8z79xb7yiyfyli9ay9v6kz61vs478rzgkhl7";
    rev = "4cf78ef769749b13d1b853c37c1672fa5e489b69";
  };
  libraryHaskellDepends = [
    aeson base bytestring http-types lens mtl nano-http text
  ];
  testHaskellDepends = [ aeson base lens mtl nano-http ];
  homepage = "https://github.com/collegevine/firebase#readme";
  description = "Firebase REST client";
  license = stdenv.lib.licenses.bsd3;
}
