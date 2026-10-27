{
  inputs,
  lib,
  ...
}: {
  nvim = lib.modules.importApply ./nvim.nix inputs;
}
