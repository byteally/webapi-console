let
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "7991ea07b766d560c477a92bd63235ee74514198";
      sha256 = "03hmkarvfg3zmpz3b977gkznligii5myyqzd8cg5bh6qmmjqcvsd";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform
