{ mkDerivation, base, directory, filepath, fsnotify, hpack
, http-types, mtl, stdenv, wai, wai-app-static, warp
}:
mkDerivation {
  pname = "watchman";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base directory filepath fsnotify http-types mtl wai wai-app-static
    warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base directory filepath fsnotify http-types mtl wai wai-app-static
    warp
  ];
  testHaskellDepends = [
    base directory filepath fsnotify http-types mtl wai wai-app-static
    warp
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/pamu/watchman#readme";
  description = "Serve static html content";
  license = stdenv.lib.licenses.bsd3;
}
