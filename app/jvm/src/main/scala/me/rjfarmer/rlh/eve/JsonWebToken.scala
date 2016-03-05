package me.rjfarmer.rlh.eve

import java.nio.charset.Charset
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

object JsonWebToken {

  private[this] val CS = Charset.forName("UTF-8")
  private[this] val ALG = "HmacSHA256"

  def sign(payload: Payload, secret: String): String = {
    val hs256 = Mac.getInstance(ALG)
    val key = new SecretKeySpec(secret.getBytes(CS), ALG)
    hs256.init(key)

    val ehdr = encodeBase64[Header](Header("HS256", "JWT"))
    val epay = encodeBase64[Payload](payload)
    val signBytes = (ehdr + "." + epay).getBytes(CS)
    val esig = Base64.getEncoder.encodeToString(hs256.doFinal(signBytes))

    ehdr + "." + epay + "." + esig
  }

  def verify(jwtString: String, secret: String): Option[JsonWebToken] = {
    val Array(ehdr, epay, esig) = jwtString.split('.')
    val header = decodeBase64[Header](ehdr)
    val payload = decodeBase64[Payload](epay)

    header match {
      case Header("HS256", "JWT") =>
        val Array(_, _, sig2) = sign(payload, secret).split('.')
        if (sig2 == esig) {
          Some(new JsonWebToken(header, payload, esig))
        } else {
          System.err.println("JsonWebToken.verify: invalid signature!")
          None
        }
      case _ =>
        System.err.println("JsonWebToken.verify: Unrecognized header: " + header)
        None
    }
  }

  private def encodeBase64[T: upickle.default.Writer](t: T): String = {
    val str = upickle.default.write[T](t)
    Base64.getEncoder.encodeToString(str.getBytes(CS))
  }

  private def decodeBase64[T: upickle.default.Reader](s: String): T = {
    val str = new String(Base64.getDecoder.decode(s), CS)
    upickle.default.read[T](str)
  }


  final case class Header(alg: String, typ: String)

  final case class Payload(username: String, crestToken: CrestToken)

}

final case class JsonWebToken(header: JsonWebToken.Header,
                              payload: JsonWebToken.Payload,
                              signature: String)
