# Backpack: Class Struggle
Calculating stats about "worlds" and orphan instances in Haskell, in accordance with the
analysis in Scott Kilpatrick's PhD thesis.

## World Semantics

The worlds of packages and all the modules defined in them can be analyzed by running the
`calc-worlds` executable in this Cabalized package. Send a list of package names, one per file,
to the stdin of the executable in order to analyze those packages' worlds. Pass in the `--islands`
argument to additionally print, for each such package, the "islands", i.e., a mapping of all the
modules defining instances that are known to any modules in the package. For example,

```cat data/pkgs/platform.txt | cabal run calc-worlds -- --islands```

To print the entire world of particular modules, pass in those modules, without
package identifiers, to the `calc-worlds` program. For example,

```echo -e "base\ncontainers" | cabal run calc-worlds -- Prelude Data.Map```

will output worlds for the `Prelude` and `Data.Map` modules defined in
`base` and `containers` respectively. One will see the entire mapping of
where their instances come from, along with their consistency and total
instance count:

```
base:Prelude -> {...}
                (1375 ; consistent)
containers-0.5.5.1:Data.Map -> {...}
                               (1621 ; consistent)
```

Note that this `base` is version `4.7.0.2`; due to an implementation oddity,
the program does not show the version of the `base` package but does for every
other.

### Results

See `data/worlds` for the analysis of the type class "worlds" inhabited by modules defined in the
various packages, along with the worlds inhabited by the entire packages themselves. Each world
is a collection of "islands"; an "island" is an upstream module that defines some new instances
(the number of which is generally specified in parentheses in the data files).

For example, [data/worlds/ghc-pkg-world.txt](data/worlds/ghc-pkg-world.txt) has the output of
the analysis for the worlds of modules defined in the package `ghc-7.8.4` (i.e., the GHC API).
Notably, this package's world is *inconsistent* because two of the modules it defines inhabit
inconsistent worlds themselves, i.e., they transitively import conflicting instances, and thus
so do many other modules in the package.

As another example, check out [data/worlds/popular-pkg-worlds.txt](data/worlds/popular-pkg-worlds.txt)
to see the worlds of "popular" packages (the top 100 most downloaded, from early 2015). Notably,
several of these have inconsistent worlds stemming from modules defined in five of them.

### Worlds file contents description

In the first section of the output, you'll see the detected cabal sandbox of installed packages,
with the chosen packages (from stdin) selected. Then you'll see output from processing the modules
in the selected packages (and any modules they depend on). When a processed module has an
inconsistent world, you'll either see that the module "inherited" the inconsistency from one of
its upstream modules, or you'll see that it "created" a new inconsistency in one of three ways:

* there are conflicting instances among its own locally defined instances;
* there are conflicting instances between its imported modules; or
* there are conflicting instances between its imported modules and its locally defined instances.

For example, here's a line expressing a new inconsistency:

```
  ! warning: ghc-7.8.4:CmmNode : creating inconsistency @ imports+locals:
   - local instance(s): [instance CmmExpr.UserOfRegs
                                    r CmmExpr.CmmExpr =>
                                  CmmExpr.UserOfRegs r CmmNode.ForeignTarget
                           -- Defined in ‘ghc-7.8.4:CmmNode’]
   - imported ghc-7.8.4:CmmExpr instance: instance GHC.Classes.Ord
                                                     r =>
                                                   CmmExpr.UserOfRegs r r
                                            -- Defined in ‘ghc-7.8.4:CmmExpr’
```

This means that the `CmmNode` module in package `ghc-7.8.4` had the third kind of new
inconsistency.

And here's a line expressing an inherited inconsistency in the `GHC` module:

```
  ! warning: ghc-7.8.4:GHC : inheriting inconsistency from: {ghc-7.8.4:CmmExpr, ghc-7.8.4:CmmNode}
```

After the output from processing each module, you'll see the worlds of the selected packages.
If you passed in the `--islands` flag, then first you'll see, for each package, a collection of
all the islands in that package's world. Here's an example of the islands for `ghc-7.8.4`:

```
ghc-7.8.4 => {array-0.5.0.0:Data.Array.Base (44), 
              array-0.5.0.0:Data.Array.IO.Internals (19), 
              ...
              Cabal-1.18.1.5:Distribution.ParseUtils (10), 
              ... 
              containers-0.5.5.1:Data.IntMap.Base (11), 
              ...
              deepseq-1.3.0.2:Control.DeepSeq (33), 
              directory-1.2.1.0:System.Directory (4), 
              ghc-7.8.4:Avail (3), 
              ghc-7.8.4:Bag (3), 
              ...
              ghc-7.8.4:TypeRep (14), 
              -- ghc-7.8.4:TypeRep[boot] (1),
              ...
              template-haskell:Language.Haskell.TH.Ppr (26)}
             (3921 ; inconsistent)
```

Note that it contains islands corresponding to modules defined in other packages as well; those
modules are upstream of modules in `ghc`. Next to each module name is the number of local instances
define in that module. Some module lines are "commented out" and say `[boot]` after the name. These
are the "boot files" for recursive modules. They have their own instance count, for the instances
declared (but not defined) in the boot file, but their instances do not count toward the total for
the whole world, denoted by the number 3921, hence being "commented out". (The analyzer indeed checks that the instances
declared in boot files have matching instance definitions in the corresponding implementing module.)

And finally, no matter whether you enabled `--islands`, you'll see a brief description of each
selected package's world that indicates whether it was consistent -- and if so, which modules
created the inconsistency of its world -- and the total instance count that it knows about.
Note that the latter is *not* the number of instances defined in modules defined by this
package, but the number of instances defined in its modules and in those upstream in other packages.
Here's the summary for the world of the `ghc-7.8.4` package:

```
ghc-7.8.4 => inconsistent! {ghc-7.8.4:CmmExpr,
                            ghc-7.8.4:CmmNode} (3921)
```


## Orphans

See [most-downloaded.txt](most-downloaded.txt) for stats on
[Hackage's most downloaded packages](https://hackage.haskell.org/packages/top)
(all those above 1000 downloads at time of writing).

See [platform-and-most-downloaded.txt](platform-and-most-downloaded.txt) for stats on the above
and the packages [included in the Haskell platform](https://github.com/haskell/haskell-platform/blob/74c3a90290f23adfeeb342f4e97122fd735f9c64/hptool/src/Releases2013.hs).

### Orphans file contents description

Each file lists the modules defined in each package and counts the number of
orphan instances (and non-orphan instances) defined in each module. For those
that do define orphans, those orphans are listed (with full class name followed
by the type constructor of each of the class's arguments). e.g.,
```
Text.Parsec.ByteString
  * Text.Parsec.Prim.Stream [Just Data.ByteString.Internal.ByteString,
                             Nothing, Just GHC.Types.Char]
  = orphans         :   1 of   1
```
means that the module `Text.Parsec.ByteString` defines a single orphan instance
for the class `Text.Parsec.Prim.Stream` whose three parameters are: (1) a type headed by
the type constructor `Data.ByteString.Internal.ByteString`, (2) a type variable, and (3) the
type headed by the type constructor `GHC.Types.Char`.

The bottom of each file lists some quick stats for each package: the number of
orphan modules (out of total modules) defined in that package, the number of
orphan instances (out of total instances) defined in that package, and the number
of modules that define no instances at all (out of total modules). Finally,
these stats are summed up for all such packages.

### Methodology

It's ugly. Basically I just grabbed lists of desired packages and then installed them
into a local sandbox at some default version. No guarantee that the selected versions
are consistent with each other. The code in [ReadOrphans.hs](ReadOrphans.hs) does a lot
of nastiness to look into the local repository's file system and analyze the binary
interface (`.hi`) file for every module in every installed package. (Note that since
each has been installed, each has been typechecked, and thus we have a binary interface
file for every module in every package.)

TODO: Use http://www.stackage.org/ to select a consistent cut of packages to locally install.
TODO: Calculate how many distinct sets of "static knowledge" (i.e., type class instances,
family instances, and rules) exist among the packages.
