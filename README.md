# orphans
Calculating stats about orphan instances in Haskell. Work in progress.

See [most-downloaded.txt](most-downloaded.txt) for stats on
[Hackage's most downloaded packages](https://hackage.haskell.org/packages/top)
(all those above 1000 downloads at time of writing).

See [platform-and-most-downloaded.txt](platform-and-most-downloaded.txt) for stats on the above
and the packages [included in the Haskell platform](https://github.com/haskell/haskell-platform/blob/74c3a90290f23adfeeb342f4e97122fd735f9c64/hptool/src/Releases2013.hs).

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
