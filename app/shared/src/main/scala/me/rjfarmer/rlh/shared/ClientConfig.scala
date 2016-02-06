package me.rjfarmer.rlh.shared


/**
 * Client configuration.
 *
 * these are not exchanged via a web request, but are filled in in the initial client download.
 * Otherwise the software version would always be the one we got from a web request and
 * would not actually match the deployed client version.
 *
 * @param clientSoftwareVersion version on the client, needs to match the server version in the web requests
 * @param staleOlderThanMillis cached data older than this is considered stale.
 */
final case class ClientConfig(clientSoftwareVersion: String, staleOlderThanMillis: Long)
