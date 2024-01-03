{% assign versions = site.data.versions %}

# perspective: Higher kinded data in Scala
perspective aims to provide many of the tools you need to program with Higher Kinded Data (HKD) in Scala.
Everything from deriving the categorical typeclasses themselves to deriving
general typeclasses for ADTs using HKD instead of HLists as a backbone.

**NOTE: ** perspective is still in development, and is as such
prone to changing as I figure out the best ways to do stuff.

If you want a good primer for working with HKD, check out this [talk](https://www.youtube.com/watch?v=oWXxtfTBlM0).


Add perspective to your project by adding these statements to your `build.sbt` file.
```scala
// For Typeclasses
libraryDependencies += "net.katsstuff" %% "perspective" % "{{versions.perspective}}"

// For Derivation
libraryDependencies += "net.katsstuff" %% "perspective-derivation" % "{{versions.perspective}}"
```

# More information

For more information, either see the examples or the ScalaDoc.
