package me.rjfarmer.rlh.client

import scalatags.Text.all._
import scalatags.Text.tags2

object Page {

  val boot =
    """me.rjfarmer.rlh.client.LittleHelper().main(document.getElementById('rlhMain'));"""

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
        tags2.title("Random's Little Helper"),
        script(src:="/es5-shim.min.js"),
        script(src:="/es5-sham.min.js"),
        script(src:=scalaJsResource),
        script(src:="//localhost:12345/workbench.js"),
        link(
          rel:="stylesheet",
          href:="/webjars/pure/0.6.0/pure.css"
        )
      ),
      body(padding:="24px",
        div(id:="rlhMenu", cls:="pure-menu pure-menu-horizontal",
          style:="display: inline; position: absolute; top: 0px; right: 0px; width: 5cm; text-align: right;",
          ul(cls:="pure-menu-list",
            li(cls:="pure-menu-item pure-menu-selected",a(href:="#rlhMain", cls:="pure-menu-link", "Main")),
            li(cls:="pure-menu-item",a(href:="#rlhLogging", cls:="pure-menu-link", "Logging"))
          )
        ),
        div(id:="rlhMain"),
        div(id:="rlhLogging", hidden,
          h2("Log Messages"),
          button(id:="clearLogButton", cls:="pure-button pure-button-primary", `type`:="button", "Clear Log"),
          br(),
          table(cls:="pure-table pure-table-striped", width:="100%",
            col(width:="10%"), col(width:="10%"), col(width:="80%"),
            thead(
              th("ms"), th("Level"), th("Message")),
            tbody(id:="logMessages")
          )
        ),

        script(boot)
      )
    )

}
