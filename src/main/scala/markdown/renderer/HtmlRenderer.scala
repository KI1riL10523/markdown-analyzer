package markdown.renderer

import markdown.model.Block
import markdown.model.Inline

object HtmlRenderer:

  /** Рендерит список блоков в HTML-документ. */
  def render(blocks: List[Block]): String =
    val body = blocks.map(renderBlock).mkString("\n")
    s"""<!DOCTYPE html>
       |<html>
       |<head>
       |  <meta charset="UTF-8">
       |  <title>Markdown Document</title>
       |  <style>
       |    body { font-family: sans-serif; max-width: 800px; margin: 2em auto; padding: 0 1em; }
       |    pre { background: #f4f4f4; padding: 1em; border-radius: 4px; overflow-x: auto; }
       |    code { background: #f4f4f4; padding: 0.2em 0.4em; border-radius: 3px; }
       |    blockquote { border-left: 4px solid #ccc; margin-left: 0; padding-left: 1em; color: #555; }
       |    table { border-collapse: collapse; margin: 1em 0; }
       |    th, td { border: 1px solid #ddd; padding: 0.5em 1em; }
       |    th { background: #f4f4f4; }
       |    img { max-width: 100%; height: auto; }
       |  </style>
       |</head>
       |<body>
       |$body
       |</body>
       |</html>""".stripMargin

  /** Рендерит один блок. */
  private def renderBlock(block: Block): String = block match
    case Block.Heading(level, content) =>
      s"<h$level>${renderInlines(content)}</h$level>"

    case Block.Paragraph(content) =>
      s"<p>${renderInlines(content)}</p>"

    case Block.UnorderedList(items) =>
      val lis = items.map(item => s"  <li>${renderInlines(item)}</li>").mkString("\n")
      s"<ul>\n$lis\n</ul>"

    case Block.OrderedList(items) =>
      val lis = items.map(item => s"  <li>${renderInlines(item)}</li>").mkString("\n")
      s"<ol>\n$lis\n</ol>"

    case Block.Quote(content) =>
      val inner = content.map(renderBlock).mkString("\n")
      s"<blockquote>\n$inner\n</blockquote>"

    case Block.CodeBlock(language, code) =>
      val langAttr = language.map(l => s""" class="language-$l"""").getOrElse("")
      s"<pre><code$langAttr>${escapeHtml(code)}</code></pre>"

    case Block.Table(headers, rows) =>
      val headerHtml = headers.map(h => s"<th>${renderInline(h)}</th>").mkString
      val rowsHtml = rows.map { row =>
        val cells = row.map(c => s"<td>${renderInline(c)}</td>").mkString
        s"<tr>$cells</tr>"
      }.mkString("\n")
      s"""<table>
         |  <thead><tr>$headerHtml</tr></thead>
         |  <tbody>
         |$rowsHtml
         |  </tbody>
         |</table>""".stripMargin

  /** Рендерит список inline-элементов в строку. */
  private def renderInlines(inlines: List[Inline]): String =
    inlines.map(renderInline).mkString

  /** Рендерит один inline-элемент. */
  private def renderInline(inline: Inline): String = inline match
    case Inline.Text(value) =>
      escapeHtml(value)

    case Inline.Bold(content) =>
      s"<strong>${renderInlines(content)}</strong>"

    case Inline.Italic(content) =>
      s"<em>${renderInlines(content)}</em>"

    case Inline.Code(value) =>
      s"<code>${escapeHtml(value)}</code>"

    case Inline.Link(text, url) =>
      s"""<a href="${escapeHtml(url)}">${renderInlines(text)}</a>"""

    case Inline.Image(alt, url) =>
      s"""<img src="${escapeHtml(url)}" alt="${escapeHtml(alt)}" />"""

  /** Экранирует HTML-спецсимволы. */
  private def escapeHtml(s: String): String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")