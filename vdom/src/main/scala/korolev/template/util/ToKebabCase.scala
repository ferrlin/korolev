package korolev.template.util

/**
  * Evidence of fact that [[T]] can be converted
  * to kebab-case
  *
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
trait ToKebabCase[T] {
  def convert(value: T): T
}

object ToKebabCase {
  def apply[T: ToKebabCase]: ToKebabCase[T] =
    implicitly[ToKebabCase[T]]

  /** CamelCase string can be converted to kebab-case.
    * Note what the fact, that string is camelCase string,
    * is not proved.
    */
  implicit val camelCaseStringToKebabCase: ToKebabCase[String] = new ToKebabCase[String] {
    def convert(s: String): String = s.replaceAll("([A-Z]+)", "-$1").toLowerCase
  }
}
