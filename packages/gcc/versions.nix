let
  majorMinorToVersionMap = {
    "16" = "16.0.1.20260201";
  };

  fromMajorMinor = majorMinorVersion: majorMinorToVersionMap."${majorMinorVersion}";

  srcHashForVersion = version:
    {
      # 3 digits: releases (14.2.0)
      # 4 digits: snapshots (14.2.1.20250322)
      "16.0.1.20260201" = "sha256-C5OxqxTX+/DYDJDC5k8qC/3isFRL457kYxGm3meUdb8=";
    }
    ."${version}";
in {
  inherit fromMajorMinor;
  inherit srcHashForVersion;
  allMajorVersions = builtins.attrNames majorMinorToVersionMap;
}
