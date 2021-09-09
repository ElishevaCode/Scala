//Week 5//
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

val oo = List(5, 3, 2, 1, 4, 3)
val oo2 = msort(oo)

val ee = List(5, 3, 2, 1, 4, 3)
val ee2 = oo.sorted

//פונקציית מיון על כל רשימה שהיא
def msortT[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def mergeT(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (lt(x, y)) x :: mergeT(xs1, ys)
        else y :: mergeT(xs, ys1)
    }

    val (fst, snd) = xs.splitAt(n)
    mergeT(msortT(fst)(lt), msortT(snd)(lt))
  }
}

////נושאים נוספים שנלמדו בשבוע:
// xs.map -- מבצע על איבר מהרשימה פעולה מסוימת
// xs.filter -- ממיין את הרשימה עפ תנאי מסוים
// xs.filterNot -- ממיין את הרשימה עפ כל מה שלא עונה על התנאי
// xs.partition -- יוצר 2 רשימות: רשימה אחת עם כל האיברים שעונים על תנאי ורשימה שניה עם כל האיברים שלא עונים על התנאי
// xs.takeWhile -- מחזיר את רצף האיברים הארוך ביותר שעונה על תנאי מסוים
// xs.dropWhile -- מחזיר את רצף האיברים הארוך ביותר שלא עונה על התנאי
// xs.span -- מחזירה 2 רשימות האחת עם רצף האיברים הארוך שעונה על התנאי והשניה כל שאר האיברים???
// reduceLeft / reduceRight
// foldleft / foldright
// tupple = pair
// x * y = _ * _


//Week 6//
// vector (x +: xs / xs :+ x)
// Array (val xs: Array[Int] = Array(1,2,3)
// String (val ys: String = "Hello World!")
// Range (until - עד המספר האחרון, to - כולל המס האחרון, by - קפיצות)
//xs.exsist(p) -- האם יש איבר ברצף שעונה על התנאי
//xs.forall(p) -- האם כל האיברים ברצף עונים על התנאי
//xs.zip(ys) -- ys ואיבר אחד מ xs מחלק את הרשימה לזוגות - כל זוג מכיל איבר אחד מ
//xs.unzip --  
//xs.flatmap(f) --
//xs.sum -- סכום האיברים
//xs,product -- מכפלת האיברים
//xs.max -- האיבר המקסימלי
//xs.min -- האיבר המינמלי
// for s yield e
// map
// sorted - מיון האיברים
// sortWith - מיון האיברים עפ תנאי שאנחנו נותנים
// groupBy - כיווץ האיברים עפ תנאי מסוים


val m1 = Map("blue" -> 2, "red" -> 1)
val m2 = m1 + ("grin" -> 3)
val m3 = m1 ++ m2
val m4 = Map("jj" -> 5, "njn" -> 8)
val m5 = m1 ++ m4
val m6 = m5.toList
m6.sortBy(x=>x._2)