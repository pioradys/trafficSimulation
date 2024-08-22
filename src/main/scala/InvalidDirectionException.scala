
case class InvalidDirectionException(private val message: String = "Invalid direction",
                                     private val cause: Throwable = None.orNull)
  extends Exception(message, cause)
