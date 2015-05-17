#!/usr/bin/env bash

# Print out any module names with boot files in the given package (NAME-VERSION).
printPkgBoots ()
{
  PKG=$1

  # ghc doesn't exist on hackage, but it has boot files
  [[ $PKG =~ ^ghc-[0-9] ]] && echo "(ghc has some boot files)" && return 0

  # 1) Get the HTML of the package contents.
  # 2) Extract each boot file link to a separate line
  # 3) Strip off the 'src/' and '.hs-boot'
  # 4) Convert slashes to dots a la module names
  # MATCHES=$(wget -qO - --no-check-certificate https://hackage.haskell.org/package/${PKG}/src/ |
  #           grep -oE 'src/[^ "]+\.hs-boot') || return 1
  # echo "$MATCHES" |
  #   sed -nE 's|src/(.+)\.hs-boot|\1|p' |
  #   sed -n 's|/|.|gp'
  wget -qO - --no-check-certificate https://hackage.haskell.org/package/${PKG}/src/ |
    grep -oE '"[^ "]+\.hs-boot"'
}


# If a single argument, print its boot files.
if [ "$#" -eq 1 ]; then
  printPkgBoots $1
else
  # Otherwise, read STDIN for pkg names to check for boot files.
  while read PKGNAME
  do
    [[ ! $PKGNAME =~ ^[a-zA-Z]+(-[a-zA-Z]+)* ]] && continue

    PKGVERSION=$(cabal sandbox hc-pkg -- --simple-output field ${PKGNAME} version)
    PKG="$PKGNAME-$PKGVERSION"
    printPkgBoots $PKG > /dev/null || continue
    
    


    # print the absolute path to the .hi files for PKG
    # ASSERT: there can be only one
    LIBDIR=$(cabal sandbox hc-pkg -- --simple-output field ${PKGNAME} library-dirs)

    # if there are .hi-boot files there, good!
    if [ "$(find ${LIBDIR} -name '*.hi-boot')" ]; then
      echo "  $PKG has boot files... and they're installed."
    else
      echo "! $PKG has boot files... and they're *NOT* installed."
    fi
  done
fi



