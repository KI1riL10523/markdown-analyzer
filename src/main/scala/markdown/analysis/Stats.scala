package markdown.analysis

/** Статистика документа. */
case class Stats(
                  headings: Int,
                  paragraphs: Int,
                  lists: Int,
                  quotes: Int,
                  codeBlocks: Int,
                  tables: Int,
                  bold: Int,
                  italic: Int,
                  code: Int,
                  links: Int,
                  images: Int,
                  words: Int
                )

object Stats:
  /** Нейтральный элемент моноида — все нули. */
  val empty: Stats = Stats(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)