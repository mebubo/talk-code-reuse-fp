import cats.Monad
import cats.data.{ReaderT, State}
import cats.implicits._
import cats.effect.IO
import cats.mtl.{ApplicativeAsk, MonadState}
import cats.mtl.implicits._
import cats.mtl.instances._

object Reuse {

  object A1 {

    def sayHello(names: List[String]): List[String] = names match {
      case Nil => Nil
      case name :: rest => "Hello " + name :: sayHello(rest)
    }
  }

  object A2 {

    def prependAll(prefix: String, names: List[String]): List[String] = names match {
      case Nil => Nil
      case name :: rest => prefix + name :: prependAll(prefix, rest)
    }

    def sayHello(names: List[String]): List[String] =
      prependAll("Hello, ", names)

    def sayGoodbye(names: List[String]): List[String] =
      prependAll("Goodbye, ", names)
  }

  object A3 {

    def transform(editFunc: String => String, names: List[String]): List[String] = names match {
      case Nil => Nil
      case name :: rest => editFunc(name) :: transform(editFunc, names)
    }

    def prependAll(prefix: String, names: List[String]): List[String] =
      transform(prefix + _, names)

    def appendAll(suffix: String, names: List[String]): List[String] =
      transform(_ + suffix, names)
  }

  object A4 {

    def transform[A](editFunc: A => A, values: List[A]): List[A] = values match {
      case Nil => Nil
      case first :: rest => editFunc(first) :: transform(editFunc, rest)
    }

    def prependAll(prefix: String, names: List[String]): List[String] =
      transform[String](prefix + _, names)

    def appendAll(suffix: String, names: List[String]): List[String] =
      transform[String](_ + suffix, names)

    def negateAll(bools: List[Boolean]): List[Boolean] =
      transform[Boolean](!_, bools)

    def addAll(num: Int, nums: List[Int]): List[Int] =
      transform[Int](_ + num, nums)

  }

  object A5 {

    def transform[A, B](editFunc: A => B, values: List[A]): List[B] = values match {
      case Nil => Nil
      case first :: rest => editFunc(first) :: transform(editFunc, rest)
    }

    def formatAll(numbers: List[Int]): List[String] =
      transform[Int, String](_.toString, numbers)
  }

  object A6 {

    def transform[F[_], A, B](f: A => B, fa: F[A]): F[B] = ???

    type Transform[F[_], A, B] = (A => B, F[A]) => F[B]

    def transformList[A, B]: Transform[List, A, B] = (f, fa) => fa match {
      case Nil => Nil
      case x :: xs => f(x) :: transformList(f, xs)
    }

    def transformArray[A, B]: Transform[Array, A, B] = ???

  }


  object A7 {
    trait Transform[F[_]] {
      def transform[A, B](f: A => B, fa: F[A]): F[B]
    }

    val transformList: Transform[List] = new Transform[List] {
      def transform[A, B](f: A => B, fa: List[A]): List[B] = fa match {
        case Nil => Nil
        case x :: xs => f(x) :: transform(f, xs)
      }
    }

    def transformArray: Transform[Array] = ???

    def formatAll[F[_]](transform: Transform[F], numbers: F[Int]): F[String] =
      transform.transform[Int, String](_.toString, numbers)

    def negateAll[F[_]](transform: Transform[F], bools: F[Boolean]): F[Boolean] =
      transform.transform[Boolean, Boolean](!_, bools)

    def addAll[F[_]](transform: Transform[F], num: Int, nums: F[Int]): F[Int] =
      transform.transform[Int, Int](_ + num, nums)

    def sayHello[F[_]](transform: Transform[F], names: F[String]): F[String] =
      transform.transform("Hello, " + _, names)
  }

  object A8 {
    trait Transform[F[_]] {
      def transform[A, B](f: A => B, fa: F[A]): F[B]
    }

    implicit val transformList: Transform[List] = new Transform[List] {
      def transform[A, B](f: A => B, fa: List[A]): List[B] = fa match {
        case Nil => Nil
        case x :: xs => f(x) :: transform(f, xs)
      }
    }

    implicit val transformArray: Transform[Array] = ???

    def formatAll[F[_]](numbers: F[Int])(implicit transform: Transform[F]): F[String] =
      transform.transform[Int, String](_.toString, numbers)

    def negateAll[F[_]](bools: F[Boolean])(implicit transform: Transform[F]): F[Boolean] =
      transform.transform[Boolean, Boolean](!_, bools)

    def addAll[F[_]](num: Int, nums: F[Int])(implicit transform: Transform[F]): F[Int] =
      transform.transform[Int, Int](_ + num, nums)

    addAll(1, List(1, 2, 3))
    addAll(1, Array(1, 2, 3))
  }

  object FS {
    def readFile(path: String): IO[Option[String]] = ???
    def writeFile(path: String, content: String): IO[Unit] = ???
  }
  object HTTP {
    def get(url: String): IO[String] = ???
  }

  object B1 {
    def realCode: IO[String] = for {
      cached <- FS.readFile("/cache")
      res <- cached match {
        case None => for {
          content <- HTTP.get("https://typelevel.org/")
          _ <- FS.writeFile("/cache", content)
        } yield content
        case Some(c) => IO.pure(c)
      }
    } yield res
  }

  object B2 {
    def getUrlCached(url: String): IO[String] = for {
      cached <- FS.readFile("/cache")
      res <- cached match {
        case None => for {
          content <- HTTP.get(url)
          _ <- FS.writeFile("/cache", content)
        } yield content
        case Some(c) => IO.pure(c)
      }
    } yield res
  }

  object B3 {
    def getUrlWithCache(path: String, url: String): IO[String] = for {
      cached <- FS.readFile(path)
      res <- cached match {
        case None => for {
          content <- HTTP.get(url)
          _ <- FS.writeFile(path, content)
        } yield content
        case Some(c) => IO.pure(c)
      }
    } yield res
  }

  object B4 {
    def getUrlWithCache(path: String, url: String)
                       (fsReadFile: String => IO[Option[String]],
                        fsWriteFile: (String, String) => IO[Unit],
                        httpGet: String => IO[String]): IO[String] = for {
      cached <- fsReadFile(path)
      res <- cached match {
        case None => for {
          content <- httpGet(url)
          _ <- fsWriteFile(path, content)
        } yield content
        case Some(c) => IO.pure(c)
      }
    } yield res
  }

  object B5 {
    trait Effects {
      def fsReadFile: String => IO[Option[String]]
      def fsWriteFile: (String, String) => IO[Unit]
      def httpGet: String => IO[String]
    }

    def getUrlWithCache(path: String, url: String)
                       (effects: Effects): IO[String] = for {
      cached <- effects.fsReadFile(path)
      res <- cached match {
        case None => for {
          content <- effects.httpGet(url)
          _ <- effects.fsWriteFile(path, content)
        } yield content
        case Some(c) => IO.pure(c)
      }
    } yield res
  }

  object B6 {
    trait Effects[F[_]] {
      def fsReadFile: String => F[Option[String]]
      def httpGet: String => F[String]
      def fsWriteFile: (String, String) => F[Unit]
    }

    def getUrlWithCache[F[_]: Monad](path: String, url: String)
                       (effects: Effects[F]): F[String] = for {
      cached <- effects.fsReadFile(path)
      res <- cached match {
        case None => for {
          content <- effects.httpGet(url)
          _ <- effects.fsWriteFile(path, content)
        } yield content
        case Some(c) => Monad[F].pure(c)
      }
    } yield res
  }

  object B7 {
    trait Effects[F[_], Req] {
      def cacheRead: String => F[Option[String]]
      def cacheWrite: (String, String) => F[Unit]
      def request: Req => F[String]
    }

    def requestWithCache[F[_]: Monad, Req](path: String, url: Req)
                                          (effects: Effects[F, Req]): F[String] = for {
      cached <- effects.cacheRead(path)
      res <- cached match {
        case None => for {
          content <- effects.request(url)
          _ <- effects.cacheWrite(path, content)
        } yield content
        case Some(c) => Monad[F].pure(c)
      }
    } yield res
  }

  object B8 {
    trait Cache[F[_]] {
      def read(key: String): F[Option[String]]
      def write(key: String, value: String): F[Unit]
    }

    trait Request[F[_], Req] {
      def request(req: Req): F[String]
    }

    def requestWithCache[F[_]: Monad, Req](path: String, url: Req)
                                          (implicit cache: Cache[F], request: Request[F, Req]): F[String] = for {
      cached <- cache.read(path)
      res <- cached match {
        case None => for {
          content <- request.request(url)
          _ <- cache.write(path, content)
        } yield content
        case Some(c) => Monad[F].pure(c)
      }
    } yield res

    def realCode[F[_]: Monad](implicit cache: Cache[F], req: Request[F, String]): F[String] =
      requestWithCache[F, String]("/cache", "https://typelevel.org")

  }

  object B9 {
    import B8._

    type AppMonad[A] = IO[A]

    implicit val cacheIO: Cache[IO] = new Cache[IO] {
      override def read(key: String): IO[Option[String]] = FS.readFile(key)
      override def write(key: String, value: String): IO[Unit] = FS.writeFile(key, value)
    }
    implicit val requestIO: Request[IO, String] = (req: String) => HTTP.get(req)

    def realCodeApp: AppMonad[String] = realCode[AppMonad]
  }

  object B10 {
    import B8._

    type S[A] = State[Map[String, String], A]
    type TestMonad[A] = ReaderT[S, String, String]

    implicit def cacheTest[F[_]: Monad](implicit S: MonadState[F, Map[String, String]]): Cache[F] = new Cache[F] {
      override def read(key: String): F[Option[String]] = S.get.map(_.get(key))
      override def write(key: String, value: String): F[Unit] = S.modify(_ + (key -> value))
    }

    implicit def requestTest[F[_]: Monad](implicit A: ApplicativeAsk[F, String]): Request[F, String] = new Request[F, String] {
      override def request(req: String): F[String] = A.ask.map(s => if (req == "https://typelevel.org/") s else "Not Found")
    }

    //    def realCodeTest: TestMonad[String] = realCode[TestMonad]
    implicitly[Monad[S]]
    //    implicitly[Monad[TestMonad]]

  }

  object B11 {
    import B8._

    type TestMonad[A] = State[Map[String, String], A]

    implicit def cacheTest[F[_]: Monad](implicit S: MonadState[F, Map[String, String]]): Cache[F] = new Cache[F] {
      override def read(key: String): F[Option[String]] = S.get.map(_.get(key))
      override def write(key: String, value: String): F[Unit] = S.modify(_ + (key -> value))
    }

    implicit def requestTest[F[_]: Monad]: Request[F, String] = new Request[F, String] {
      override def request(req: String): F[String] = Monad[F].pure(req match {
        case "http://typelevel.org/" => "Success"
        case _ => "Not Found"
      })
    }

    def realCodeTest: TestMonad[String] = realCode[TestMonad]

  }

  def main(args: Array[String]): Unit = {
    println(A1.sayHello(List("Bob", "Gladys", "Geoffrey")))
  }
}