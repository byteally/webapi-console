(import ./reflex-platform.nix).project ({ pkgs, ... }: {
  packages = {
    webapi-console = ./src/..;
    
    webapi-contract = ((pkgs.fetchFromGitHub {
      owner = "byteally";
      repo = "webapi";
      rev = "137c4a0548606180ab801315f70367d88cc69587";
      sha256 = "19symg4lldxwpaajdnlihsbx2kmmvwpb05lsd1lq8g7sw6j27250";
    }) + "/webapi-contract");

    webapi = ((pkgs.fetchFromGitHub {
      owner = "byteally";
      repo = "webapi";
      rev = "137c4a0548606180ab801315f70367d88cc69587";
      sha256 = "19symg4lldxwpaajdnlihsbx2kmmvwpb05lsd1lq8g7sw6j27250";
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
