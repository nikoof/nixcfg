{
  flutter,
  fetchFromGitHub,
  lib,
  mpv-unwrapped,
}:
flutter.buildFlutterApplication {
  pname = "coppelia";
  version = "v0.0.11-alpha";

  src = fetchFromGitHub {
    owner = "j6k4m8";
    repo = "coppelia";
    rev = "cd98258321a07b55c76ac6fcc697bd2e8ef0ab2b";
    hash = "sha256-KIC7YL+d8Hbm1gGVl7YS+cDo3IDH3+m2PW9cyRii8go=";
  };

  runtimeDependencies = [mpv-unwrapped];

  pubspecLock = lib.importJSON ./pubspec.lock.json;
}
