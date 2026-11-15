import scala.util.Random

object Lab1 {
  def add(x: Int, y: Int): Int = x + y

  def maxOfTwo(x: Int, y: Int): Int = if (x >= y) x else y

  def joinWords(first: String, second: String): String = first + " " + second

  // Відбираю числа за умовою через for yield.
  def selectNumbers(values: List[Int]): List[Int] = {
    for (number <- values if number > 5 && number < 10 && number != 8) yield number
  }

  // Те саме завдання, але через filter.
  def selectNumbersFilter(values: List[Int]): List[Int] = {
    values.filter(number => number > 5 && number < 10 && number != 8)
  }

  // headOption не дає помилки, якщо список порожній.
  def firstWord(values: List[String]): Option[String] = values.headOption

  // Обчислюю основні характеристики масиву.
  def arrayStats(values: Array[Int]): (Int, Int, Option[Int], Option[Int], Int) = {
    val evenCount = values.count(number => number % 2 == 0)
    (values.length, values.sum, values.minOption, values.maxOption, evenCount)
  }

  def main(args: Array[String]): Unit = {
    println("=== 1. Методи ===")
    println(s"add(1, 2) = ${add(1, 2)}")
    println(s"add(-5, 3) = ${add(-5, 3)}")
    println(s"maxOfTwo(2, 5) = ${maxOfTwo(2, 5)}")
    println(s"maxOfTwo(-3, -7) = ${maxOfTwo(-3, -7)}")
    println(s"maxOfTwo(4, 4) = ${maxOfTwo(4, 4)}")
    println(s"joinWords = ${joinWords("Hello", "Scala")}")

    println("\n=== 2. Відбір чисел ===")
    val numbers = (1 to 10).toList
    val selectedByFor = selectNumbers(numbers)
    val selectedByFilter = selectNumbersFilter(numbers)
    println(s"for yield: $selectedByFor")
    println(s"filter: $selectedByFilter")
    println(s"Результати однакові: ${selectedByFor == selectedByFilter}")
    val testNumbers = List(5, 6, 6, 8, 9, 10)
    println(s"З повторами: ${selectNumbers(testNumbers)}")
    println(s"Порожній список: ${selectNumbers(Nil)}")
    println(s"Порожній список (filter): ${selectNumbersFilter(Nil)}")

    println("\n=== 3. Перше слово ===")
    val wordLists = List(List("apples", "avocado", "oranges"), List.empty[String], List("Empty"))
    wordLists.foreach { words =>
      firstWord(words) match {
        case Some(word) => println(s"$words -> Some($word)")
        case None => println(s"$words -> елемент відсутній (None)")
      }
    }

    println("\n=== 4. Масиви ===")
    // Створюю 10 випадкових чисел від 0 до 99.
    val randomArray = Array.fill(10)(Random.nextInt(100))
    val testArray = Array(4, 7, 2, 9, 4)
    val emptyArray = Array.empty[Int]
    List(randomArray, testArray, emptyArray).foreach { values =>
      val (length, sum, minimum, maximum, evenCount) = arrayStats(values)
      println(s"Елементи: ${values.mkString(", ")}")
      println(s"Довжина: $length; сума: $sum; мінімум: $minimum; максимум: $maximum; парних: $evenCount")
    }

    println("\n=== 5. Множини ===")
    // Перевіряю операції з множинами.
    val a = Set(1, 2, 3, 4, 5)
    val b = Set(4, 5, 6, 7)
    println(s"Об'єднання: ${(a ++ b).toList.sorted.mkString(", ")}")
    println(s"Перетин: ${a.intersect(b).toList.sorted.mkString(", ")}")
    println(s"Різниця: ${a.diff(b).toList.sorted.mkString(", ")}")
    println(s"Є число, кратне 3: ${a.exists(number => number % 3 == 0)}")
    println(s"Знайти 3: ${a.find(_ == 3)}")
    val emptySet = Set.empty[Int]
    println(s"Об'єднання з порожньою множиною: ${(a ++ emptySet).toList.sorted}")
    println(s"Перетин з порожньою множиною: ${a.intersect(emptySet)}")
    println(s"Пошук у порожній множині: ${emptySet.find(_ == 3)}")

    println("\n=== Додаткове завдання ===")
    // Порівнюю список із повторами та множину без повторів.
    val repeated = List(1, 1, 2, 2, 3)
    val unique = repeated.toSet
    println(s"Список: $repeated; кількість: ${repeated.length}")
    println(s"Множина: ${unique.toList.sorted}; кількість: ${unique.size}")
    println("Після перетворення на Set інформація про повтори втрачається.")
  }
}
