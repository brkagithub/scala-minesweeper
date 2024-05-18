import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

case class ScoreEntry(dateTime: LocalDateTime, score: Double) {
  override def toString: String = {
    val formatter = DateTimeFormatter.ofPattern("yyyy-dd-MM HH:mm:ss")
    s"${dateTime.format(formatter)}: $score"
  }
}