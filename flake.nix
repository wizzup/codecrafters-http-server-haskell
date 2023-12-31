{
  description = "hs-grep";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    cc-tester = {
      url = "github:codecrafters-io/http-server-tester";
      flake = false;
    };
  };

  nixConfig.bash-prompt-suffix = "🚀";
  outputs = {
    self,
    nixpkgs,
    cc-tester,
  }: (let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    drv = pkgs.haskellPackages.developPackage {root = ./.;};

    hsPcks = with pkgs.haskellPackages; [
      cabal-install
      ghcid
      haskell-language-server
      implicit-hie
      hpack
      doctest
    ];

    ccyml = pkgs.writeTextDir "/data/codecrafters.yml" (builtins.readFile ./codecrafters.yml);
    ccsh = pkgs.writeTextFile {
      name = "your_grep.sh";
      text = ''
        #!/bin/sh

        exec hs-http-server-clone-exe "$@"
      '';
      executable = true;
      destination = "/data/your_server.sh";
    };

    env = pkgs.buildEnv {
      name = "env";
      paths = [tester ccyml ccsh drv];
    };

    dev = drv.env.overrideAttrs (attr: {
      buildInputs =
        attr.buildInputs
        ++ hsPcks
        ++ [pkgs.helix];
    });

    # from codecrafters-io/http-server-tester/main/internal/tester_definition.go
    # TODO: auto generate
    test_case = ''
      [{"slug":"connect-to-port","tester_log_prefix":"1/8","title":"connect-to-port"}
      ,{"slug":"respond-with-200","tester_log_prefix":"2/8","title":"respond-with-200"}
      ,{"slug":"respond-with-404","tester_log_prefix":"3/8","title":"respond-with-404"}
      ,{"slug":"respond-with-content","tester_log_prefix":"4/8","title":"respond-with-content"}
      ,{"slug":"parse-headers","tester_log_prefix":"5/8","title":"parse-headers"}
      ,{"slug":"concurrent-connections","tester_log_prefix":"6/8","title":"concurrent-connections"}
      ,{"slug":"get-file","tester_log_prefix":"7/8","title":"get-file"}
      ,{"slug":"post-file","tester_log_prefix":"8/8","title":"post-file"}
      ]
    '';

    tester = pkgs.buildGoModule {
      name = "tester-http-server";
      src = cc-tester;
      vendorHash = "sha256-u0Y2Y5ku8GbsU+JbQVldFU05fqHasxNEMUB27ElZAUI=";
      nativeBuildInputs = [pkgs.python3];
      doCheck = false;
    };

    run_test = pkgs.stdenvNoCC.mkDerivation {
      name = "run-test";
      src = env;
      buildInputs = [drv env];

      CODECRAFTERS_SUBMISSION_DIR = "./data";
      CODECRAFTERS_TEST_CASES_JSON = test_case;

      installPhase = ''
        ${tester}/bin/tester 2>&1 | tee $out
      '';
    };
  in
    with pkgs; {
      formatter.${system} = alejandra;
      packages.${system} = {
        # inherit drv env run_test;
        default = run_test;
      };
      devShells.${system}.default = dev;
    });
}
