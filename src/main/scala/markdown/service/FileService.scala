package markdown.service

import zio.*
import java.io.{BufferedReader, FileReader, PrintWriter}

object FileService:

  /** Читает файл как строку. Использует acquireReleaseWith для безопасного закрытия. */
  def readFile(path: String): Task[String] =
    ZIO.acquireReleaseWith(
      acquire = ZIO.attempt(new BufferedReader(new FileReader(path)))
    )(
      release = reader => ZIO.succeed(reader.close())
    )(
      use = reader => ZIO.attempt {
        val sb = new StringBuilder
        var line = reader.readLine()
        while line != null do
          sb.append(line).append("\n")
          line = reader.readLine()
        sb.toString
      }
    )

  /** Записывает строку в файл. */
  def writeFile(path: String, content: String): Task[Unit] =
    ZIO.acquireReleaseWith(
      acquire = ZIO.attempt(new PrintWriter(path))
    )(
      release = writer => ZIO.succeed(writer.close())
    )(
      use = writer => ZIO.attempt(writer.write(content))
    )

  /** Логирует сообщение. */
  def log(message: String): UIO[Unit] =
    ZIO.succeed(println(message))