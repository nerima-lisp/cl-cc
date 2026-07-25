{ pkgs, sbclWithCLCC, ... }:
# treefmt is configured in flake.nix via treefmt-nix.lib.evalModule, not here:
# without flake-parts there is no module to hand a `treefmt` attribute to, and
# `checks.formatting` needs the evaluated config at the top level anyway.
{
  devShells.default = pkgs.mkShell {
    packages = [
      sbclWithCLCC
      pkgs.rlwrap
    ];
    shellHook = ''
      cat <<'EOF'

        CL-CC Development Shell
        ------------------------

        Apps:
          nix run .#test      Run the canonical fast unit test plan
          nix run .#bench     Run the benchmark suite (diagnostic, not a gate)
          nix run .#load      Load :cl-cc non-interactively
          nix run .#repl      rlwrap'd SBCL with :cl-cc loaded  (nix run default)

        Nix:
          nix flake check    Run checks.default + .formatting + .docs + .build
          nix build          Build the standalone binary at ./result/bin/cl-cc
          nix fmt            Format the Nix sources (nixfmt, via treefmt)
          nix flake show     List all flake outputs

        CLI (after `nix build`, binary at ./result/bin/cl-cc):
          cl-cc run example/hello.lisp
          cl-cc eval "(+ 1 2)"
          cl-cc compile example/hello.lisp -o hello
          cl-cc help

      EOF
    '';
  };
}
