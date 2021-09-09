//Week 5
val list = List(1, 2, 3, 4, 5)
val list2 = 1 :: 2 :: 3 :: 4 :: Nil
val l1 = list(1) //מחזיר את האיבר במקום השני 2
val l2 = list.init //List(1, 2, 3, 4) מחזיר את הרשימה ללא האיבר האחרון
val l3 = list.take(2) //List(1, 2) מחזיר את 2 האיברים הראשונים של הרשימה
val l4 = list.drop(3) //List(4, 5) מחזיר את מה שנשאר מהרשימה לאחר שהוסרו 3 איברים
val l5 = list ++ l2 //List(1, 2, 3, 4, 5, 1, 2, 3, 4) חיבור 2 רשימות
val l6 = list.reverse //List(5, 4, 3, 2, 1)
val l7 = list.updated(3, 2) //List(1, 2, 3, 2, 5) מעדכן את האיבר במיקום 3 למספר 2
val l8 = list.indexOf(4) //מחזיר את מיקום האלמנט  3
val l9 = list.contains(3) //true  מחזיר האם האלמנט קיים


//פונקציה המסירה את האיבר שנמצא במקום הN
def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
  case Nil => Nil
  case y :: ys =>
    if (n == 0) ys
    else removeAt(n - 1, ys)
}

//פונקציה שמשטחת רשימות
def flatten(xs: Any): List[Any] = xs match {
  case Nil => Nil
  case y :: ys => flatten(y) ++ flatten(ys)
  case _ => xs :: Nil
}

//פונקציה שחותכת רשימה ל2 במיקום של n
def splitAt[T](xs: List[T], n: Int) =
  (xs.take(n), xs.drop(n))

val list3 = List(1, 2, List(3, 3, 3), 4, 5, List(2, 3))

val ll = flatten(list3)

val pp = splitAt(ll, 3)

//פונקציה מיון על רשימה של מספרים
def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (x < y) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }

    val (fst, snd) = xs.splitAt(n)
    merge(msort(fst), msort(snd))
  }
}

//פונקציית מיון על כל רשימה שהיא
def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge[T](xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
    
    val (fst, snd) = xs.splitAt(n)
    merge(msort(fst)(lt), msort(snd)(lt))
  }
}

////נושאים נוספים שנלמדו בשבוע:
// merge sort
// map
// filter
// reduceLeft / reduceRight
// foldleft / foldright
// tupple = pair
// x*y = _*_


