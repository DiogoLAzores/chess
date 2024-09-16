## Scala Chess Implementation - WHG Assessment

### Setup

In order for this project to work as intended, please fill out the absolute path for both the required library and the
moves' file.

For that, refer to the following places and add the aforementioned information:
- `build.sbt` - Change `[ADD PATH TO userinput.jar HERE]` by the absolute path of the userinput.jar. If not at hand,
consider using the one in `/libs/userinput.jar`
```sbt
    "com.whitehatgaming" % "UserInputFile" % "1.0" from "file:///[ADD PATH TO userinput.jar HERE]"
```
- `src/main/scala/chess/Main.scala` - Change `[ADD FILE'S ABSOLUTE PATH HERE]` by the absolute path of the moves' file.
If not at hand, consider using one of the move files in `/data`, like `sample-moves.txt`
```scala
    private val movesFile = new UserInputFile("[ADD FILE'S ABSOLUTE PATH HERE]")
```

---

### Execution

To run, open sbt CLI inside the project's directory and execute `sbt run`

Note: To use other moves' files, remember to replace the absolute path of `movesFile` in `Main.scala`