(import ./reflex-platform.nix).project ({ pkgs, ... }: {
  packages = {
    webapi-console = ./src/..;
    
    webapi-contract = ((pkgs.fetchFromGitHub {
      owner = "byteally";
      repo = "webapi";
      rev = "50d511c5a89aacf6b9be24c2cc84fa2eb641463b";
      sha256 = "0h8qdji3p2m0sc5h77mcfad968ygkjb31bbrlj51nr07w99y6bn0";
    }) + "/webapi-contract");

    webapi = ((pkgs.fetchFromGitHub {
      owner = "byteally";
      repo = "webapi";
      rev = "50d511c5a89aacf6b9be24c2cc84fa2eb641463b";
      sha256 = "0h8qdji3p2m0sc5h77mcfad968ygkjb31bbrlj51nr07w99y6bn0";
    }) + "/webapi");
  };

  shells = {
    ghc = [ "webapi-console"
          ];
    ghcjs = [ "webapi-console"
            ];
  };

  android.webapi-console = {
    executableName = "WebApiConsole";
    applicationId = "com.byteally";
    displayName = "WebApi Console";
  };

  ios.frontend = {
    executableName = "frontend";
    bundleIdentifier = "org.example.frontend";
    bundleName = "Example iOS App";
  };
})
