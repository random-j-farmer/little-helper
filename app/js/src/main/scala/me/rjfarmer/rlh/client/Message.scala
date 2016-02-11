package me.rjfarmer.rlh.client

object Message {

  sealed trait MessageType {
    def messageClass: String
  }

  case object Info extends MessageType {
    val messageClass = "message-info"
  }

  case object Warning extends MessageType {
    val messageClass = "message-warning"
  }

  case object Error extends MessageType {
    val messageClass = "message-error"
  }

  def info(msg: String): Message = Message(msg, Info)

  def warning(msg: String): Message = Message(msg, Warning)

  def error(msg: String): Message = Message(msg, Error)

}

final case class Message(msg: String, msgType: Message.MessageType)
