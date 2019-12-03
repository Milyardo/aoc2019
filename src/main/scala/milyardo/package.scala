import java.io._

import cats.Show
import cats.effect.{IO, Resource}

package object milyardo {
  def resource(name: String)(implicit cl: ClassLoader = getClass.getClassLoader): Resource[IO, InputStream] =
    Resource.make(IO(cl.getResourceAsStream(name)))(is => IO(is.close()))
  def loadResource(name: String)(implicit cl: ClassLoader = getClass.getClassLoader): Resource[IO, String] =
    resource(name).evalMap({is =>
      IO(new String(is.readAllBytes()))
    })

  def debug[A: Show](a: A): IO[Unit] = {
    val msg = Show[A].show(a)
    IO(println(s"> $msg"))
  }

  def console[A: Show](a: A): IO[Unit] = {
    IO(println(Show[A].show(a)))
  }
}
