// Лабораторна робота №1. Основи програмування мовою Scala.
// У програмі продемонстровано методи, списки, масиви, множини та перевірку порожніх колекцій.

// Імпортуємо генератор випадкових чисел для четвертого завдання.
import scala.util.Random

// object містить методи програми; main є точкою її запуску.
object Lab1 {

  // ЗАВДАННЯ 1. Методи для чисел і рядків.
  // x та y — параметри типу Int; після двокрапки вказано тип результату.
  // Останній вираз методу автоматично стає його результатом.
  def add(x: Int, y: Int): Int = {
    x + y
  }

  // if у Scala може повертати значення: вибираємо більше з двох чисел.
  // Якщо числа рівні, повертається x (це не змінює числовий результат).
  def maxOfTwo(x: Int, y: Int): Int = {
    if (x >= y) x else y
  }

  // Об'єднуємо два рядки, вставляючи між ними рівно один пробіл.
  def joinWords(first: String, second: String): String = {
    first + " " + second
  }

  // ЗАВДАННЯ 2. Відбір чисел двома способами.
  // List[Int] — список цілих чисел; метод повертає новий список.
  // for yield перебирає елементи та збирає ті, що задовольняють умову.
  def selectNumbers(values: List[Int]): List[Int] = {
    for (number <- values if number > 5 && number < 10 && number != 8)
      yield number
  }

  // filter перевіряє кожне число за тією самою умовою.
  // Обидва способи зберігають порядок елементів і повтори.
  def selectNumbersFilter(values: List[Int]): List[Int] = {
    values.filter(number => number > 5 && number < 10 && number != 8)
  }

  // ЗАВДАННЯ 3. Безпечний доступ до першого слова.
  // Option[String] означає: Some(слово), якщо елемент є, або None, якщо список порожній.
  // headOption безпечний на відміну від head, який для порожнього списку викликає помилку.
  def firstWord(values: List[String]): Option[String] = {
    values.headOption
  }

  // ЗАВДАННЯ 4. Обчислення статистики масиву.
  // Unit означає, що метод виконує дію (друкує), а не повертає корисне значення.
  def printArrayStats(values: Array[Int]): Unit = {
    // mkString перетворює елементи масиву на рядок із роздільником ", ".
    println(s"Елементи: ${values.mkString(", ")}")
    println(s"Довжина: ${values.length}") // Кількість елементів.
    println(s"Сума: ${values.sum}")       // Для порожнього масиву сума дорівнює 0.

    // minOption і maxOption повертають None для порожнього масиву, тому не виникає помилки.
    println(s"Мінімум: ${values.minOption}")
    println(s"Максимум: ${values.maxOption}")

    // count рахує елементи, для яких остача від ділення на 2 дорівнює нулю.
    println(s"Парних чисел: ${values.count(number => number % 2 == 0)}")
  }

  // Допоміжний метод для множин: сортуємо лише для зручного виведення.
  // Сама множина Set не гарантує певного порядку елементів.
  def displaySet(values: Set[Int]): String = {
    values.toList.sorted.mkString("Set(", ", ", ")")
  }

  // main запускається при старті програми.
  def main(args: Array[String]): Unit = {
    println("=== Завдання 1. Числа та рядки ===")
    // Перевіряємо звичайні, від'ємні й однакові числа.
    println(s"add(1, 2) = ${add(1, 2)}")
    println(s"add(-4, 2) = ${add(-4, 2)}")
    println(s"add(3, 3) = ${add(3, 3)}")
    println(s"maxOfTwo(2, 5) = ${maxOfTwo(2, 5)}")
    println(s"maxOfTwo(-2, -5) = ${maxOfTwo(-2, -5)}")
    println(s"maxOfTwo(3, 3) = ${maxOfTwo(3, 3)}")
    println(s"joinWords = ${joinWords("Hello", "Scala")}")

    println("\n=== Завдання 2. Фільтрація списку ===")
    val numbers = (1 to 10).toList // Створюємо список чисел від 1 до 10.
    val withRepeats = List(5, 6, 6, 8, 9, 10) // Перевіряємо межі та повтори.
    // val не дозволяє переприсвоїти змінній інший список.
    for (values <- List(numbers, withRepeats, List.empty[Int])) {
      println(s"Вхід: $values")
      println(s"for yield: ${selectNumbers(values)}")
      println(s"filter: ${selectNumbersFilter(values)}")
      // == порівнює результати двох реалізацій.
      println(s"Результати збігаються: ${selectNumbers(values) == selectNumbersFilter(values)}")
    }

    println("\n=== Завдання 3. Перше слово ===")
    // Окремо перевіряємо звичайний, порожній список і список зі словом "Empty".
    val wordLists = List(List("apples", "avocado", "oranges"), List.empty[String], List("Empty"))
    // foreach виконує дії для кожного списку, але не формує нову колекцію.
    wordLists.foreach { words =>
      val result = firstWord(words)
      println(s"Список: $words, результат: $result")
      // None означає відсутність елемента, а Some("Empty") — наявність слова "Empty".
      if (result.isEmpty) println("Першого елемента немає")
    }

    println("\n=== Завдання 4. Масиви ===")
    // Array.fill створює 10 елементів; nextInt(100) дає число від 0 до 99.
    val randomNumbers = Array.fill(10)(Random.nextInt(100))
    println("Випадковий масив:")
    printArrayStats(randomNumbers)
    println("Контрольний масив:")
    printArrayStats(Array(4, 7, 2, 9, 4))
    println("Порожній масив:")
    printArrayStats(Array.empty[Int])

    println("\n=== Завдання 5. Множини ===")
    // Set зберігає тільки унікальні значення.
    val a = Set(1, 2, 3, 4, 5)
    val b = Set(4, 5, 6, 7)
    println(s"a = ${displaySet(a)}")
    println(s"b = ${displaySet(b)}")
    println(s"Об'єднання: ${displaySet(a ++ b)}") // Усі унікальні числа з a і b.
    println(s"Перетин: ${displaySet(a.intersect(b))}") // Спільні числа.
    println(s"Різниця a.diff(b): ${displaySet(a.diff(b))}") // Є в a, але немає в b.
    println(s"Є кратне 3: ${a.exists(number => number % 3 == 0)}") // true або false.
    println(s"Пошук числа 3: ${a.find(number => number == 3)}") // Some(3) або None.
    // Перевіряємо операції з порожньою множиною.
    println(s"Об'єднання з порожньою множиною: ${displaySet(a ++ Set.empty[Int])}")
    println(s"Перетин із порожньою множиною: ${displaySet(a.intersect(Set.empty[Int]))}")
    println(s"Пошук у порожній множині: ${Set.empty[Int].find(number => number == 3)}")

    println("\n=== Додаткове завдання ===")
    val repeated = List(1, 1, 2, 2, 3)
    val unique = repeated.toSet // Видаляємо повтори, перетворюючи List на Set.
    println(s"Список: $repeated, кількість: ${repeated.size}")
    println(s"Множина: ${displaySet(unique)}, кількість: ${unique.size}")
    println("Під час перетворення на Set втрачається інформація про повтори та порядок.")
  }
}
