package markdown.parser

import markdown.model.Block
import markdown.model.Inline

object BlockParser:

  def parse(input: String): List[Block] =
    val lines = input.split("\r?\n", -1).toList
    parseBlocks(lines)

  private def parseBlocks(lines: List[String]): List[Block] =
    lines match
      case Nil => Nil

      // Пропускаем пустые строки
      case line :: rest if line.trim.isEmpty =>
        parseBlocks(rest)

      // Заголовок: #, ##, ###, ...
      case line :: rest if line.startsWith("#") =>
        val level = line.takeWhile(_ == '#').length
        val content = line.drop(level).trim
        val heading = Block.Heading(level, InlineParser.parse(content))
        heading :: parseBlocks(rest)

      // Код-блок: ```lang ... ```
      case line :: rest if line.trim.startsWith("```") =>
        val lang = line.trim.stripPrefix("```").trim
        val language = if lang.isEmpty then None else Some(lang)
        val (codeLines, remaining) = rest.span(!_.trim.startsWith("```"))
        val code = codeLines.mkString("\n")
        val codeBlock = Block.CodeBlock(language, code)
        // Пропускаем закрывающий ```
        val after = remaining match
          case _ :: tail => tail
          case Nil       => Nil
        codeBlock :: parseBlocks(after)

      // Неупорядоченный список: - item
      case line :: _ if line.trim.startsWith("- ") =>
        val (items, remaining) = collectListItems(lines, "- ")
        val list = Block.UnorderedList(items)
        list :: parseBlocks(remaining)

      // Упорядоченный список: 1. item
      case line :: _ if isOrderedListItem(line) =>
        val (items, remaining) = collectOrderedItems(lines)
        val list = Block.OrderedList(items)
        list :: parseBlocks(remaining)

      // Цитата: > text
      case line :: _ if line.trim.startsWith(">") =>
        val (quoteLines, remaining) = collectQuoteLines(lines)
        // Убираем `>` из каждой строки и парсим рекурсивно
        val innerText = quoteLines.map(_.trim.stripPrefix(">").trim).mkString("\n")
        val quote = Block.Quote(parseBlocks(innerText.split("\n", -1).toList))
        quote :: parseBlocks(remaining)

      // Таблица: | a | b |
      case line :: _ if line.trim.startsWith("|") =>
        val (tableLines, remaining) = lines.span(_.trim.startsWith("|"))
        val table = parseTable(tableLines)
        table :: parseBlocks(remaining)
      // Абзац: всё остальное
      case _ =>
        val (paragraphLines, remaining) = collectParagraphLines(lines)
        val content = paragraphLines.mkString(" ")
        val paragraph = Block.Paragraph(InlineParser.parse(content))
        paragraph :: parseBlocks(remaining)

  // ============ Вспомогательные функции ============

  /** Собирает все пункты неупорядоченного списка. */
  private def collectListItems(lines: List[String], marker: String): (List[List[Inline]], List[String]) =
    val (itemLines, remaining) = lines.span(_.trim.startsWith(marker))
    val items = itemLines.map { line =>
      val content = line.trim.stripPrefix(marker).trim
      InlineParser.parse(content)
    }
    (items, remaining)

  /** Проверяет, является ли строка пунктом упорядоченного списка (1. , 2. , ...). */
  private def isOrderedListItem(line: String): Boolean =
    line.trim.matches("^\\d+\\.\\s.*")

  /** Собирает все пункты упорядоченного списка. */
  private def collectOrderedItems(lines: List[String]): (List[List[Inline]], List[String]) =
    val (itemLines, remaining) = lines.span(isOrderedListItem)
    val items = itemLines.map { line =>
      val content = line.trim.replaceFirst("^\\d+\\.\\s*", "").trim
      InlineParser.parse(content)
    }
    (items, remaining)

  /** Собирает строки цитаты (все подряд идущие `> ...`). */
  private def collectQuoteLines(lines: List[String]): (List[String], List[String]) =
    lines.span(_.trim.startsWith(">"))

  /** Собирает строки абзаца (до пустой строки или начала нового блока). */
  private def collectParagraphLines(lines: List[String]): (List[String], List[String]) =
    val (paragraphLines, remaining) = lines.span { line =>
      line.trim.nonEmpty &&
        !line.startsWith("#") &&
        !line.trim.startsWith("```") &&
        !line.trim.startsWith("- ") &&
        !isOrderedListItem(line) &&
        !line.trim.startsWith(">")
    }
    (paragraphLines, remaining)

  /** Парсит таблицу: первая строка — заголовки, вторая — разделители, остальные — данные. */
  private def parseTable(lines: List[String]): Block.Table =
    val cells = lines.map { line =>
      line.trim.stripPrefix("|").stripSuffix("|").split("\\|").map(_.trim).toList
    }

    // Первая строка — заголовки, вторая — разделители (---), остальные — данные
    val headers = cells.headOption.getOrElse(Nil).map(h => Inline.Text(h): Inline)
    val rows = cells.drop(2).map(_.map(r => Inline.Text(r): Inline))

    Block.Table(headers, rows)
