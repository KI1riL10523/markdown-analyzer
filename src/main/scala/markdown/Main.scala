package markdown

import zio.*
import markdown.service.Pipeline

object Main extends ZIOAppDefault:

  def run: ZIO[ZIOAppArgs, Any, Any] =
    for
      args <- ZIOAppArgs.getArgs
      _    <- args.toList match
        case input :: html :: json :: _ =>
          Pipeline.analyzeFile(input, html, json)
        case _ =>
          Console.printLine("Usage: markdown-analyzer <input.md> <output.html> <output.json>")
    yield ()