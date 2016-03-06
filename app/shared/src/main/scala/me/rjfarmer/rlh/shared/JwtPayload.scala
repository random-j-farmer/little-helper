package me.rjfarmer.rlh.shared

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

