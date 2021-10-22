{ buildLatexDoc }:

buildLatexDoc {
  name = "lazy-machine";
  src = ./.;
  texFiles = [ "lazy-zerepoch-core.tex" ];
  description = "lazy machine discussion";
}
