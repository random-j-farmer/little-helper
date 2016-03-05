package me.rjfarmer.rlh.server

import me.rjfarmer.rlh.shared.{SharedConfig, ClientConfig}

import scalatags.Text.all._
import scalatags.Text.{TypedTag, tags2}

object Page {

  def boot(clientConfig: ClientConfig, jwt: Option[String]) = {
    val config = upickle.default.write(clientConfig)
    val jwtJson = upickle.default.write(jwt)
    raw(s"""me.rjfarmer.rlh.client.LittleHelper().main(document.getElementById('body'), $config, $jwtJson);""")
  }

  def resourceLastModified(name: String): Long = {
    val url = getClass.getResource(name)
    if (url == null) -1L else url.openConnection().getLastModified
  }

  def newestResource(names: String*): String = {
    val mods = names map resourceLastModified
    val zipped = mods zip names
    val newest = zipped.max
    println( s"""newestResource: ${zipped mkString ", "} => $newest""")
    newest._2
  }

  def scalaJsResource: String = newestResource("/little-helper-opt.js", "/little-helper-fastopt.js")

  def skeleton(jwt: Option[String]): TypedTag[String] =
    html(
      head(
        tags2.title("Random's Little Helper"),
        script(src := "/es5-shim.min.js"),
        script(src := "/es5-sham.min.js"),
        script(src := scalaJsResource),
        // script(src:="//localhost:12345/workbench.js"),
        link(rel := "stylesheet", href := "/webjars/pure/0.6.0/pure.css"),
        link(rel := "stylesheet", href := "/little-helper.css")
      ),
      body(id := "body", padding := "24px",
        script(boot(SharedConfig.client, jwt))
      )
    )

}
