import korolev.template.{TemplateContext, TemplateDsl}

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
object templateTest {
  val dsl = new TemplateDsl(new TemplateContext[Any]())
  import dsl._

  implicit val rc: templateContext.RenderContext = templateContext.RenderContext.Dummy

  val tpl0 = 'h1("I am cow")

  val tpl1 = template { 'h1("Hello world") }

  val tpl2 = template {
    def innerComponent(x: Int) = template {
      'li ('class /= "govno", x.toString)
    }

    'div(
      tpl0,
      tpl1,
      innerComponent(1)
//      Seq(1, 2, 3) map { x =>
//        'ul(
//          Seq(x+1, x+2, x+3) map { y =>
//            innerComponent(y)
//          }
//        )
//      }
    )
  }

  println("hello world!")
}
