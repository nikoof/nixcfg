{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: {
  age.secrets.wireless = {file = ./wireless.conf.age;};
  age.secrets.smb-fw2b = {file = ./smb-fw2b.age;};
}
