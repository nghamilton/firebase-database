{ mkDerivation, aeson, base, bytestring, conduit, fetchgit
, http-client, http-conduit, http-types, json-stream, lens, mtl
, resourcet, stdenv
}:
mkDerivation {
  pname = "nano-http";
  version = "0.1.3";
  src = fetchgit {
    url = "https://github.com/ralphmorton/nano-http";
    sha256 = "13wgx7nnccsxwcrhb2j8lv2fmqhz1zm5a7y7d2i52h99mwzy444d";
    rev = "65c68b4a01c07b9afa110744d012bb1e411800b4";
  };
  libraryHaskellDepends = [
    aeson base bytestring conduit http-client http-conduit http-types
    json-stream lens mtl resourcet
  ];
  homepage = "https://github.com/ralphmorton/nano-http#readme";
  description = "A lightweight wrapper around conduit";
  license = stdenv.lib.licenses.bsd3;
}
