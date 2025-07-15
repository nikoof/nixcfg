let
  nitrokey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILyDNgRiiZjy0gHd4vdl6etlGYDKcN115aqWnlZoj4Cp";
  hofstadterNikoof = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK0CnaDwAP100xrQd6b2qbrlwdFVbxV6S4Ilwvxz8XZN";
  hofstadterHost = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII26fC+patJIgigp7s/jDeZul8Yk9dABo70RHcnjpdLZ";
  keys = [nitrokey hofstadterHost hofstadterNikoof];
in {
  "wireless.conf.age".publicKeys = keys;
  "smb-fw2b.age".publicKeys = keys;
}
