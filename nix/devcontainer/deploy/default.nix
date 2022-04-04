{ pkgs ? import <nixpkgs> { }
, zerepoch
, dockerImage ? import ../zerepoch-devcontainer.nix { inherit pkgs zerepoch; }
}:
let
  imageRef = dockerImage.imageName + ":" + dockerImage.imageTag;
  dockerHubRepoName = "tbco/zerepoch-starter-devcontainer";
in
pkgs.writeScript "docker-build-push-devcontainer" ''
  #!${pkgs.runtimeShell}
  set -euo pipefail

  echo "Loading the docker image ..."
  docker load -i ${dockerImage}

  tag="''${BUILDKITE_TAG:-}"
  echo "Git tag: ''${tag}."
    
  # Pick out only the version component of a tag like:
  # "zerepoch-starter-devcontainer/v1.0" -> "v1.0"
  version="$(echo $tag | sed -e 's/.*[\/]//')"

  # Construct a tag to push up to dockerHub
  docker tag "${imageRef}" "${dockerHubRepoName}:''${version}"
  docker tag "${imageRef}" "${dockerHubRepoName}:latest"

  docker push "${dockerHubRepoName}:''${version}"
  docker push "${dockerHubRepoName}:latest"
''
