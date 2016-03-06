package me.rjfarmer.rlh.eve

import java.nio.charset.Charset
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import me.rjfarmer.rlh.shared.JwtPayload

object JsonWebToken {

  private[this] val CS = Charset.forName("UTF-8")
  private[this] val ALG = "HmacSHA256"

  def sign(payload: JwtPayload, secret: String): String = {
    val hs256 = Mac.getInstance(ALG)
    val key = new SecretKeySpec(secret.getBytes(CS), ALG)
    hs256.init(key)

    val ehdr = pickleBase64[Header](Header("HS256", "JWT"))
    val epay = pickleBase64[JwtPayload](payload)
    val signBytes = (ehdr + "." + epay).getBytes(CS)
    val esig = Base64.getEncoder.encodeToString(hs256.doFinal(signBytes))

    ehdr + "." + epay + "." + esig
  }

  def verify(jwtString: String, secret: String): Option[JsonWebToken] = {
    val Array(ehdr, epay, esig) = jwtString.split('.')
    val header = unpickleBase64[Header](ehdr)
    val payload = unpickleBase64[JwtPayload](epay)

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

  def encodeBase64(str: String): String = Base64.getEncoder.encodeToString(str.getBytes(CS))

  def decodeBase64(str: String): String = new String(Base64.getDecoder.decode(str), CS)

  private def pickleBase64[T: upickle.default.Writer](t: T): String = {
    val str = upickle.default.write[T](t)
    encodeBase64(str)
  }

  private def unpickleBase64[T: upickle.default.Reader](s: String): T = {
    val str = decodeBase64(s)
    upickle.default.read[T](str)
  }


  final case class Header(alg: String, typ: String)

}

final case class JsonWebToken(header: JsonWebToken.Header,
                              payload: JwtPayload,
                              signature: String)
