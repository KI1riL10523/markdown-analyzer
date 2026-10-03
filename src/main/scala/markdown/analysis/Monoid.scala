package markdown.analysis

/** Тайп-класс моноида. */
trait Monoid[A]:
  def empty: A
  def combine(a: A, b: A): A

object Monoid:
  /** Моноид для Stats: покомпонентное сложение. */
  given statsMonoid: Monoid[Stats] with
    def empty: Stats = Stats.empty
    def combine(a: Stats, b: Stats): Stats = Stats(
      a.headings   + b.headings,
      a.paragraphs + b.paragraphs,
      a.lists      + b.lists,
      a.quotes     + b.quotes,
      a.codeBlocks + b.codeBlocks,
      a.tables     + b.tables,
      a.bold       + b.bold,
      a.italic     + b.italic,
      a.code       + b.code,
      a.links      + b.links,
      a.images     + b.images,
      a.words      + b.words
    )