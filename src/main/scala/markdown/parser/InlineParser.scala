package markdown.parser

import markdown.model.Inline

object InlineParser:

  def parse(input: String): List[Inline] =
    val result = scala.collection.mutable.ListBuffer[Inline]()
    var text = new StringBuilder
    var i = 0

    def flushText(): Unit =
      if text.nonEmpty then
        result += Inline.Text(text.toString)
        text = new StringBuilder

    while i < input.length do

      // Жирный текст: **text**
      if input.startsWith("**", i) then
        val end = input.indexOf("**", i + 2)

        if end >= 0 then
          flushText()
          val content = input.substring(i + 2, end)
          result += Inline.Bold(parse(content))
          i = end + 2
        else
          text.append(input(i))
          i += 1

      // Код: `text`
      else if input(i) == '`' then
        val end = input.indexOf('`', i + 1)

        if end >= 0 then
          flushText()
          val content = input.substring(i + 1, end)
          result += Inline.Code(content)
          i = end + 1
        else
          text.append(input(i))
          i += 1

      // Изображение: ![alt](url)
      else if input.startsWith("![", i) then
        val middle = input.indexOf("](", i + 2)
        if middle >= 0 then
          val end = input.indexOf(')', middle + 2)
          if end >= 0 then
            flushText()
            val alt = input.substring(i + 2, middle)
            val url = input.substring(middle + 2, end)
            result += Inline.Image(alt, url)
            i = end + 1
          else
            text.append(input(i))
            i += 1
        else
          text.append(input(i))
          i += 1

      // Ссылка: [text](url)
      else if input(i) == '[' then
        val middle = input.indexOf("](", i + 1)

        if middle >= 0 then
          val end = input.indexOf(')', middle + 2)

          if end >= 0 then
            flushText()

            val label = input.substring(i + 1, middle)
            val url = input.substring(middle + 2, end)

            result += Inline.Link(parse(label), url)
            i = end + 1
          else
            text.append(input(i))
            i += 1
        else
          text.append(input(i))
          i += 1

      // Курсив: *text*
      else if input(i) == '*' &&
        !input.startsWith("**", i) then
        val end = input.indexOf('*', i + 1)

        if end >= 0 then
          flushText()
          val content = input.substring(i + 1, end)
          result += Inline.Italic(parse(content))
          i = end + 1
        else
          text.append(input(i))
          i += 1

      // Обычный символ
      else
        text.append(input(i))
        i += 1

    flushText()
    result.toList
