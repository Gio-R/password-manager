package is.clipperz.backend.exceptions

class ConflictualRequestException(error: String) extends Exception(error):
  def this(message: String, cause: Throwable) =
    this(message)
    initCause(cause)
