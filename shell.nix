{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  name = "scheme-env-${version}";
  version = "1.0";
  buildInputs = [ pkgs.guile
                  ( pkgs.emacsWithPackages (p:
                      [p.htmlize]))
                  pkgs.texlive.combined.scheme-full
                ];
}
