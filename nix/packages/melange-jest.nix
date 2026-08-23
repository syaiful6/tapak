{
  fetchFromGitHub,
  buildDunePackage,
  melange,
}:

let
  version = "0.2.0";
in

buildDunePackage {
  pname = "melange-jest";
  version = version;
  src = fetchFromGitHub {
    owner = "melange-community";
    repo = "melange-jest";
    rev = version;
    sha256 = "sha256-H0Y0CLY6t/aLzMg4MD4hdWV9DUu8oN3q7SAps073C34=";
  };

  nativeBuildInputs = [ melange ];
  propagatedBuildInputs = [ melange ];
  doCheck = false;
}
