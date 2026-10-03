package markdown.model

enum Markdown:
  case Document(blocks: List[Block])

enum Block:
  case Heading(level: Int, content: List[Inline])
  case Paragraph(content: List[Inline])
  case UnorderedList(items: List[List[Inline]])
  case OrderedList(items: List[List[Inline]])
  case Quote(content: List[Block])
  case CodeBlock(language: Option[String], code: String)
  case Table(headers: List[Inline], rows: List[List[Inline]])

enum Inline:
  case Text(value: String)
  case Bold(content: List[Inline])
  case Italic(content: List[Inline])
  case Code(value: String)
  case Link(text: List[Inline], url: String)
  case Image(alt: String, url: String)   