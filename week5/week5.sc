

def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
  case Nil => throw new Error ("Element not found")
  case x::rest if (n == 0) => rest
  case x::rest => x :: removeAt(n-1, rest)
}

removeAt(1, List('a', 'b', 'c', 'd'))  // List(a, c, d)
removeAt(0, List('a', 'b', 'c', 'd'))  // List(a, c, d)
removeAt(3, List('a', 'b', 'c', 'd'))  // List(a, c, d)


def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil => Nil
  case (head:List[Any])::tail => flatten(head) ::: flatten(tail)
  case y::ys => y :: flatten (ys)
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))


def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
  case (Nil, ys) => ys
  case (xs, Nil) => xs
  case (x::xs, y::ys) if (x < y) => x::merge(xs, y::ys)
  case (x, y::ys) => y::merge(x, ys)
}

merge (List(1,3,5,7,9,11), List(2,4,6,8, 10, 12, 14, 16))



def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil     => Nil
  case y :: ys => y*y :: squareList(ys)
}

def squareList2(xs: List[Int]): List[Int] =
  xs map (x => x*x)


val fixList = List(1,2,3,4,5)
squareList(fixList)
squareList2(fixList)

val data = List("a", "a", "a", "b", "c", "c", "a")

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil      => Nil
  case x :: xs1 => {
    val result = xs.span(y => y == x)
    result._1:: pack(result._2)
  }
}

pack(data)

def encode[T](xs:List[T]): List[(T, Int)] = {
  pack(xs) map (x => (x.head, x.size))
}

encode(data)


def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( f(_) :: _)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0) ((x, y) => y +1)

lengthFun(data)


val nat = List[Int](1,2,3,4,5,6)

mapFun[Int, Int](nat, (x => x+2))





