{ nixpkgs, declInput }: let pkgs = import nixpkgs {}; in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    {
        "tasty-hedgehog": {
            "enabled": 1,
            "hidden": false,
            "description": "tasty-hedgehog",
            "nixexprinput": "tasty-hedgehog",
            "nixexprpath": "ci/ci.nix",
            "checkinterval": 300,
            "schedulingshares": 1,
            "enableemail": false,
            "emailoverride": "",
            "keepnr": 5,
            "inputs": {
                "tasty-hedgehog": { "type": "git", "value": "https://github.com/qfpl/tasty-hedgehog", "emailresponsible": false },
                "nixpkgs": { "type": "git", "value": "https://github.com/NixOS/nixpkgs.git master", "emailresponsible": false }
            }
        }
    }
    EOF
  '';
}
