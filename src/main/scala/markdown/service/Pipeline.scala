package markdown.service

import zio.*
import markdown.parser.BlockParser
import markdown.analysis.Analyzer
import markdown.renderer.{HtmlRenderer, JsonRenderer}

object Pipeline:

  /** Полный конвейер: чтение → парсинг → анализ → рендеринг → запись. */
  def analyzeFile(
                   inputPath: String,
                   htmlPath: String,
                   jsonPath: String
                 ): Task[Unit] =
    for
      content <- FileService.readFile(inputPath)
      _       <- FileService.log(s"Read ${content.length} chars from $inputPath")

      blocks = BlockParser.parse(content)
      _      <- FileService.log(s"Parsed ${blocks.length} blocks")

      stats = Analyzer.analyze(blocks)

      _ <- FileService.log("=== Statistics ===")
      _ <- FileService.log(s"Headings:      ${stats.headings}")
      _ <- FileService.log(s"Paragraphs:    ${stats.paragraphs}")
      _ <- FileService.log(s"Lists:         ${stats.lists}")
      _ <- FileService.log(s"Quotes:        ${stats.quotes}")
      _ <- FileService.log(s"Code blocks:   ${stats.codeBlocks}")
      _ <- FileService.log(s"Tables:        ${stats.tables}")
      _ <- FileService.log(s"Bold:          ${stats.bold}")
      _ <- FileService.log(s"Italic:        ${stats.italic}")
      _ <- FileService.log(s"Code (inline): ${stats.code}")
      _ <- FileService.log(s"Links:         ${stats.links}")
      _ <- FileService.log(s"Images:        ${stats.images}")
      _ <- FileService.log(s"Words:         ${stats.words}")

      html = HtmlRenderer.render(blocks)
      json = JsonRenderer.render(blocks)

      _ <- FileService.writeFile(htmlPath, html)
      _ <- FileService.writeFile(jsonPath, json)
      _ <- FileService.log(s"Written to $htmlPath and $jsonPath")
    yield ()