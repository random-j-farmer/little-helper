package me.rjfarmer.rlh.eve

import java.nio.charset.Charset
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec


object JsonWebToken {

  private[this] val CS = Charset.forName("UTF-8")
  private[this] val ALG = "HmacSHA256"

  /** sign a json web token into a signed jwt token */
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

  /** verify a signed json web token string */
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

/**
 * Json Web Token
 *
 * Currently, these are not actually given out but cached by their signature.
 * Only the signature is on the client.  This is because the refresh token
 * is so sensitive.
 *
 * @param header header
 * @param payload payload
 * @param signature signature
 */
final case class JsonWebToken(header: JsonWebToken.Header,
                              payload: JwtPayload,
                              signature: String)

/**
 * CrestToken as returned by the Crest API
 *
 * the underscores are used because the input is like that
 *
 * @param accessToken  access token
 * @param expireTs long timestamp for expiration
 * @param refreshToken refresh token, can be used to get a new token
 */
final case class CrestToken(accessToken: String,
                            expireTs: Long,
                            refreshToken: String)

/**
 * JwtPayload
 *
 * @param characterName character name of authenticated character
 * @param characterID character id of authenticated characters
 * @param crestToken crestToken
 */
final case class JwtPayload(characterName: String, characterID: Long, crestToken: CrestToken)

