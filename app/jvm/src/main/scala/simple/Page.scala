package simple

import scalatags.Text.all._

object Page {

  val boot = "simple.Client().main(document.getElementById('contents'))"

  def resourceLastModified(name: String): Long = {
    val url = getClass().getResource(name)
    if (url == null) -1L else url.openConnection().getLastModified
  }

  def newestResource(names: String*): String = {
    val mods = names map resourceLastModified
    val zipped = mods zip names
    val newest = zipped.max
    println(s"""newestResource: ${zipped mkString ", "} => ${newest}""")
    newest._2
  }

  def scalaJsResource: String = newestResource("/app-opt.js", "/app-fastopt.js")

  val skeleton =
    html(
      head(
        script(src:=scalaJsResource),
        script(src:="//localhost:12345/workbench.js"),
        link(
          rel:="stylesheet",
          href:="http://yui.yahooapis.com/pure/0.6.0/pure-min.css"
        )
      ),
      body (
        onload:=boot,
        h1("Hello, Juergen!"),
        div(id:="contents")
      )
    )
}
