package markdown.renderer

import markdown.model.Block
import markdown.model.Inline
import markdown.analysis.Analyzer

object JsonRenderer:

  /** Рендерит список блоков в JSON-документ со статистикой. */
  def render(blocks: List[Block]): String =
    val stats = Analyzer.analyze(blocks)
    val blocksJson = blocks.map(renderBlock).mkString(",\n  ")
    s"""{
       |  "version": "1.0",
       |  "stats": {
       |    "headings": ${stats.headings},
       |    "paragraphs": ${stats.paragraphs},
       |    "lists": ${stats.lists},
       |    "quotes": ${stats.quotes},
       |    "code_blocks": ${stats.codeBlocks},
       |    "tables": ${stats.tables},
       |    "bold": ${stats.bold},
       |    "italic": ${stats.italic},
       |    "code": ${stats.code},
       |    "links": ${stats.links},
       |    "images": ${stats.images},
       |    "words": ${stats.words}
       |  },
       |  "blocks": [
       |  $blocksJson
       |  ]
       |}""".stripMargin

  /** Рендерит один блок в JSON-объект. */
  private def renderBlock(block: Block): String = block match
    case Block.Heading(level, content) =>
      s"""{
         |    "type": "heading",
         |    "level": $level,
         |    "content": [${renderInlines(content)}]
         |  }""".stripMargin

    case Block.Paragraph(content) =>
      s"""{
         |    "type": "paragraph",
         |    "content": [${renderInlines(content)}]
         |  }""".stripMargin

    case Block.UnorderedList(items) =>
      val itemsJson = items.map(item => s"[${renderInlines(item)}]").mkString(", ")
      s"""{
         |    "type": "unordered_list",
         |    "items": [$itemsJson]
         |  }""".stripMargin

    case Block.OrderedList(items) =>
      val itemsJson = items.map(item => s"[${renderInlines(item)}]").mkString(", ")
      s"""{
         |    "type": "ordered_list",
         |    "items": [$itemsJson]
         |  }""".stripMargin

    case Block.Quote(content) =>
      val inner = content.map(renderBlock).mkString(",\n    ")
      s"""{
         |    "type": "quote",
         |    "content": [
         |    $inner
         |    ]
         |  }""".stripMargin

    case Block.CodeBlock(language, code) =>
      val langJson = language.map(l => s""""$l"""").getOrElse("null")
      s"""{
         |    "type": "code_block",
         |    "language": $langJson,
         |    "code": "${escapeJson(code)}"
         |  }""".stripMargin

    case Block.Table(headers, rows) =>
      val headersJson = headers.map(renderInline).mkString(", ")
      val rowsJson = rows.map { row =>
        val cells = row.map(renderInline).mkString(", ")
        s"[$cells]"
      }.mkString(", ")
      s"""{
         |    "type": "table",
         |    "headers": [$headersJson],
         |    "rows": [$rowsJson]
         |  }""".stripMargin

  /** Рендерит список inline-элементов в JSON-строку (через запятую). */
  private def renderInlines(inlines: List[Inline]): String =
    inlines.map(renderInline).mkString(", ")

  /** Рендерит один inline-элемент. */
  private def renderInline(inline: Inline): String = inline match
    case Inline.Text(value) =>
      s"""{"type": "text", "value": "${escapeJson(value)}"}"""

    case Inline.Bold(content) =>
      s"""{"type": "bold", "content": [${renderInlines(content)}]}"""

    case Inline.Italic(content) =>
      s"""{"type": "italic", "content": [${renderInlines(content)}]}"""

    case Inline.Code(value) =>
      s"""{"type": "code", "value": "${escapeJson(value)}"}"""

    case Inline.Link(text, url) =>
      s"""{"type": "link", "text": [${renderInlines(text)}], "url": "${escapeJson(url)}"}"""

    case Inline.Image(alt, url) =>
      s"""{"type": "image", "alt": "${escapeJson(alt)}", "url": "${escapeJson(url)}"}"""

  /** Экранирует спецсимволы JSON. */
  private def escapeJson(s: String): String =
    s.replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")