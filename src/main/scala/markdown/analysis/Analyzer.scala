package markdown.analysis

import markdown.model.Block
import markdown.model.Inline
import markdown.analysis.Monoid.statsMonoid

object Analyzer:

  /** Анализирует список блоков и возвращает статистику. */
  def analyze(blocks: List[Block]): Stats =
    blocks.foldLeft(Stats.empty)((acc, block) =>
      statsMonoid.combine(acc, analyzeBlock(block))
    )

  /** Статистика одного блока. */
  private def analyzeBlock(block: Block): Stats = block match
    case Block.Heading(_, content) =>
      statsMonoid.combine(
        Stats.empty.copy(headings = 1, words = countWords(content)),
        analyzeInlines(content)
      )

    case Block.Paragraph(content) =>
      statsMonoid.combine(
        Stats.empty.copy(paragraphs = 1, words = countWords(content)),
        analyzeInlines(content)
      )

    case Block.UnorderedList(items) =>
      val itemStats = items.map(analyzeInlines).foldLeft(Stats.empty)(statsMonoid.combine)
      statsMonoid.combine(
        Stats.empty.copy(lists = 1, words = items.map(countWords).sum),
        itemStats
      )

    case Block.OrderedList(items) =>
      val itemStats = items.map(analyzeInlines).foldLeft(Stats.empty)(statsMonoid.combine)
      statsMonoid.combine(
        Stats.empty.copy(lists = 1, words = items.map(countWords).sum),
        itemStats
      )

    case Block.Quote(content) =>
      val innerStats = content.map(analyzeBlock).foldLeft(Stats.empty)(statsMonoid.combine)
      statsMonoid.combine(Stats.empty.copy(quotes = 1), innerStats)

    case Block.CodeBlock(_, _) =>
      Stats.empty.copy(codeBlocks = 1)

    case Block.Table(headers, rows) =>                  // ← НОВОЕ
      val headerStats = analyzeInlines(headers)
      val rowStats = rows.map(analyzeInlines).foldLeft(Stats.empty)(statsMonoid.combine)
      statsMonoid.combine(
        Stats.empty.copy(tables = 1),
        statsMonoid.combine(headerStats, rowStats)
      )

  /** Статистика inline-элементов. */
  private def analyzeInlines(inlines: List[Inline]): Stats =
    inlines.foldLeft(Stats.empty)((acc, inline) =>
      statsMonoid.combine(acc, analyzeInline(inline))
    )

  /** Статистика одного inline-элемента. */
  private def analyzeInline(inline: Inline): Stats = inline match
    case Inline.Text(_) =>
      Stats.empty

    case Inline.Bold(content) =>
      statsMonoid.combine(Stats.empty.copy(bold = 1), analyzeInlines(content))

    case Inline.Italic(content) =>
      statsMonoid.combine(Stats.empty.copy(italic = 1), analyzeInlines(content))

    case Inline.Code(_) =>
      Stats.empty.copy(code = 1)

    case Inline.Link(text, _) =>
      statsMonoid.combine(Stats.empty.copy(links = 1), analyzeInlines(text))

    case Inline.Image(_, _) =>                           // ← НОВОЕ
      Stats.empty.copy(images = 1)

  /** Извлекает весь текст из inline-элемента (рекурсивно). */
  private def extractText(inline: Inline): String = inline match
    case Inline.Text(value)     => value
    case Inline.Bold(content)   => content.map(extractText).mkString(" ")
    case Inline.Italic(content) => content.map(extractText).mkString(" ")
    case Inline.Code(_)         => ""
    case Inline.Link(text, _)   => text.map(extractText).mkString(" ")
    case Inline.Image(alt, _)   => alt                   // ← НОВОЕ

  /** Считает количество слов в inline-элементах. */
  private def countWords(inlines: List[Inline]): Int =
    inlines
      .map(extractText)
      .mkString(" ")
      .split("\\s+")
      .count(_.nonEmpty)