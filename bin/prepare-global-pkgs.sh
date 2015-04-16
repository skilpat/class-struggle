#!/bin/bash

# For packages globally available, we need to build them locally and
# copy .hi-boot files into the global package repository on the file system.
# This script uses cabal to fetch the source code for a package (into
# a temporary local directory) and build it in place; then it copies all
# .hi-boot files from the local build **into the existing global package
# database**.
#
# (For packages not globally available, we should install into them into the
# sandbox and then copy the .hi-boot files into the sandbox repository. This
# is outside the scope of this script.)

# the directory where to locally download and build these packages
LOCAL_BUILDS_DIR="./pkg-builds"

# ----------------------------------------------------------------------

# don't touch this
ALL_COPIED_FILES=""

die() { echo >&2 -e "\nRUN ERROR: $@\n"; printResults; exit 1; }
run() { $*; code=$?; [ $code -ne 0 ] && die "command [$*] failed with error code $code"; }
printResults() { echo "!!!!! ALL COPIED FILES BELOW !!!!!!"; echo "$ALL_COPIED_FILES"; }


# get the list of packages in the global repos
GLOBAL_PKGS=`ghc-pkg --simple-output list`
#GLOBAL_PKGS="Cabal-1.18.1.5 array-0.5.0.0 base-4.7.0.2"

for PKG in $GLOBAL_PKGS
do

  # FOR NOW, it seems only base has recursive modules, so just skip all other
  # packages!
  [[ ! $PKG =~ ^base-[0-9] ]] && continue

  # skip builtin packages that aren't in hackage
  # [[ $PKG =~ ^bin-package-db-[0-9] ]] && continue
  # [[ $PKG =~ ^ghc-[0-9]            ]] && continue
  # [[ $PKG =~ ^integer-gmp-[0-9]    ]] && continue
  # [[ $PKG =~ ^rts-[0-9]            ]] && continue

  echo "Preparing $PKG..."

  # path to this package build dir
  PKG_LOCAL_DIR="$LOCAL_BUILDS_DIR/$PKG"

  # fetch the source code if we don't already have it
  if [ ! -d "$PKG_LOCAL_DIR" ]; then
    echo "Fetching source for $PKG..."
    run cabal get -d $LOCAL_BUILDS_DIR $PKG
  fi

  # enter the build dir
  run pushd $PKG_LOCAL_DIR 1> /dev/null

  # -------------------------------------------------
  # TODO: skip the rest if there aren't any .hs-boot source files!!!
  # -------------------------------------------------

  # configure and build it if isn't already built
  if [ ! -d "dist/build" ]; then
    # try to configure by disabling some extra stuff. if that fails, just do a
    # default configure
    echo "Configuring $PKG..."
    cabal configure --disable-tests --disable-coverage --disable-optimization --disable-profiling
    [ $? -ne 0 ] && run cabal configure --disable-optimization

    echo "Building $PKG..."
    run cabal build
  fi

  # print the absolute path to the .hi files for PKG
  # ASSERT: there can be only one
  PKG_GLOBAL_LIB_DIR=`ghc-pkg --simple-output field $PKG library-dirs`

  # if there are .hi-boot files here, then copy
  if [ -n "$(find dist/build -name '*.hi-boot')" ]; then

    # copy all .hi-boot files in local build to libdir for this (global) package
    echo "Copying .hi-boot files for $PKG to $PKG_GLOBAL_LIB_DIR..."
    RSYNC_OUT=$(rsync -i --prune-empty-dirs -r --include="*.hi-boot" --include="*/" --exclude="*" dist/build/ $PKG_GLOBAL_LIB_DIR/) ||
    if [ "$?" -ne 0 ]; then echo "!! Failed to copy boot files from '$PKG_LOCAL_DIR/dist/build' to '$PKG_GLOBAL_LIB_DIR'"; printResults; exit 1; fi

    # get the list of files copied over
    COPIED_FILES=$(echo "$RSYNC_OUT" | sed -E  "s|.+ (.+)|$PKG_GLOBAL_LIB_DIR/\1|")

    # -------------------------------------------------
    # TODO: test whether copy was successful!
    # -------------------------------------------------

    # add this to the master list if non-null
    ALL_COPIED_FILES="$ALL_COPIED_FILES"$'\n'"$COPIED_FILES"
  fi

  # go back to original dir and delete this local junk
  popd 1> /dev/null
  #rm -rf $PKG_LOCAL_DIR
  echo "Done with $PKG."

done

# print final results
printResults

