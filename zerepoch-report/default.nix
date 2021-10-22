{ buildLatexDoc }:

buildLatexDoc {
  name = "zerepoch";
  src = ./.;
  texFiles = [ "zerepoch.tex" ];
  description = "zerepoch report";
}
