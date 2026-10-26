import scala.util.Random

// Лабораторна №2. Класи та колекції у Scala.
// Один файл містить модель людей, студентів, викладачів і перевірки роботи методів.

// Адреса складається з країни та міста. Порожні рядки не допускаються.
class Address(val country: String, val city: String) {
  require(country.trim.nonEmpty, "Країна не може бути порожньою")
  require(city.trim.nonEmpty, "Місто не може бути порожнім")

  override def toString: String = s"$country, $city"
}

// Батьківський клас для студента і викладача.
class Human(private val _name: String, private val _age: Int, val address: Address) {
  require(_name.trim.nonEmpty, "Ім'я не може бути порожнім")
  require(_age >= 0, "Вік не може бути від'ємним")

  // Методи доступу дозволяють читати приватні поля з інших класів.
  def name: String = _name
  def age: Int = _age
  def show(): String = s"Ім'я: $name, вік: $age, адреса: $address"
}

// Student успадковує поля й метод show від Human.
class Student(name: String, age: Int, val group: String, address: Address)
    extends Human(name, age, address) {
  require(group.trim.nonEmpty, "Група не може бути порожньою")

  // Оцінка генерується один раз при створенні об'єкта: від 1 до 5.
  val estimation: Int = Random.nextInt(5) + 1

  override def show(): String = s"${super.show()}, група: $group, оцінка: $estimation"
}

// Teacher зберігає студентів у приватному списку.
class Teacher(name: String, age: Int, val salary: Int, val limit: Int, address: Address)
    extends Human(name, age, address) {
  require(salary >= 0, "Зарплата не може бути від'ємною")
  require(limit >= 0, "Ліміт не може бути від'ємним")

  private var _students: List[Student] = List.empty[Student]
  def students: List[Student] = _students
  def studentCount: Int = _students.size

  def addStudent(student: Student): Unit = {
    // require перевіряє умови ДО зміни списку.
    require(studentCount < limit, "Досягнуто ліміту студентів")
    // eq порівнює посилання: той самий об'єкт не можна додати двічі.
    require(!_students.exists(_ eq student), "Цього студента вже додано")
    _students = _students :+ student // Створюємо новий List і зберігаємо його.
  }

  // filter повертає новий список і не змінює початковий.
  def findStudentsByGroup(group: String): List[Student] =
    _students.filter(_.group == group)

  // Option дає None для порожнього списку, щоб не ділити на нуль.
  def averageEstimation: Option[Double] =
    if (_students.isEmpty) None
    else Some(_students.map(_.estimation).sum.toDouble / studentCount)

  override def show(): String =
    s"${super.show()}, зарплата: $salary, студентів: $studentCount/$limit"
}

// Клас для пошуку та сортування викладачів.
class DataList(val teachers: List[Teacher]) {
  def printAllInformation(): Unit = {
    teachers.foreach { teacher =>
      println(teacher.show())
      teacher.students.foreach(student => println("  " + student.show()))
    }
  }

  // Строге порівняння: більше, а не більше або дорівнює.
  def findTeachers(count: Int): List[Teacher] =
    teachers.filter(_.studentCount > count)

  // sortBy повертає новий список, не змінюючи teachers.
  def sortByStudentCount(): List[Teacher] =
    teachers.sortBy(t => -t.studentCount)

  def findTeachersByCity(city: String): List[Teacher] =
    teachers.filter(_.address.city == city)

  // Рахує включення студентів до списків, а не унікальних людей.
  def totalStudentAssignments: Int = teachers.map(_.studentCount).sum
}

object Lab2 {
  // Кожну очікувану помилку перевіряємо окремо, щоб програма йшла далі.
  def checkError(label: String)(action: => Unit): Unit = {
    try {
      action
      println(s"$label: помилки НЕ було (перевірте умову)")
    } catch {
      case e: IllegalArgumentException => println(s"$label: ${e.getMessage}")
    }
  }

  // Для порожніх результатів друкуємо зрозуміле повідомлення.
  def showNames(people: List[Teacher]): String =
    if (people.isEmpty) "Нічого не знайдено"
    else people.map(t => s"${t.name}: ${t.studentCount}").mkString("; ")

  def main(args: Array[String]): Unit = {
    val kherson = new Address("Ukraine", "Kherson")
    val kyiv = new Address("Ukraine", "Kyiv")
    val lviv = new Address("Ukraine", "Lviv")

    val std1 = new Student("Ivan", 19, "131 M", kherson)
    val std2 = new Student("Valerii", 20, "131 M", kherson)
    val std3 = new Student("Olha", 18, "132 M", kherson)
    val std4 = new Student("Petro", 19, "132 M", kyiv)
    val std5 = new Student("Anna", 21, "133 M", kyiv)

    // Початкові дані: Ruslan має 3 студентів, Andry — 2.
    val teach1 = new Teacher("Ruslan", 40, 15000, 10, kherson)
    val teach2 = new Teacher("Andry", 35, 12000, 11, kyiv)
    List(std1, std2, std3).foreach(teach1.addStudent)
    List(std4, std5).foreach(teach2.addStudent)
    val data = new DataList(List(teach1, teach2))

    println("=== Початкові дані ===")
    data.printAllInformation()
    println("Оцінки п'ятьох студентів: " + List(std1, std2, std3, std4, std5).map(_.estimation).mkString(", "))

    println("\n=== Пошук за кількістю студентів ===")
    List(0, 2, 3, 10).foreach(n => println(s"Поріг $n: ${showNames(data.findTeachers(n))}"))

    println("\n=== Сортування за спаданням ===")
    println(showNames(data.sortByStudentCount()))
    println("Початковий порядок: " + data.teachers.map(_.name).mkString(", "))

    println("\n=== Поліморфізм ===")
    // Тип Human спільний, але викликається show конкретного класу.
    val people: List[Human] = List(std1, teach1)
    people.foreach(person => println(person.show()))

    println("\n=== Пошук студентів за групою ===")
    val group131 = teach1.findStudentsByGroup("131 M")
    println("131 M: " + group131.map(_.name).mkString(", "))
    println("Невідома група: " + teach1.findStudentsByGroup("999 M").map(_.name).mkString("[", ", ", "]"))

    println("\n=== Середня оцінка ===")
    println(s"Ruslan: ${teach1.averageEstimation}")
    println(s"Перевірка: ${teach1.students.map(_.estimation).mkString(" + ")} / ${teach1.studentCount}")
    val emptyTeacher = new Teacher("Empty", 30, 10000, 0, lviv)
    println(s"Викладач без студентів: ${emptyTeacher.averageEstimation}")

    println("\n=== Пошук викладачів за містом ===")
    val extraTeacher = new Teacher("Olena", 35, 12000, 2, lviv)
    val extendedData = new DataList(data.teachers :+ extraTeacher)
    List("Kherson", "Lviv", "Odesa").foreach(city =>
      println(s"$city: ${showNames(extendedData.findTeachersByCity(city))}"))

    println("\n=== Кількість записів про студентів ===")
    println(s"Початкові дані: ${data.totalStudentAssignments}")
    println(s"Порожній DataList: ${new DataList(Nil).totalStudentAssignments}")

    println("\n=== Перевірка некоректних даних ===")
    checkError("Від'ємний вік") { new Student("Test", -1, "131 M", kherson); () }
    checkError("Від'ємна зарплата") { new Teacher("Test", 30, -1, 1, kyiv); () }
    checkError("Від'ємний ліміт") { new Teacher("Test", 30, 100, -1, kyiv); () }
    checkError("Порожнє ім'я") { new Student("", 19, "131 M", kherson); () }
    checkError("Ім'я з пробілів") { new Student("   ", 19, "131 M", kherson); () }
    checkError("Порожня група") { new Student("Test", 19, "", kherson); () }
    checkError("Група з пробілів") { new Student("Test", 19, "   ", kherson); () }
    checkError("Порожня країна") { new Address("", "Kyiv"); () }
    checkError("Країна з пробілів") { new Address("   ", "Kyiv"); () }
    checkError("Порожнє місто") { new Address("Ukraine", ""); () }
    checkError("Місто з пробілів") { new Address("Ukraine", "   "); () }

    val limited = new Teacher("Limited", 35, 12000, 1, kyiv)
    limited.addStudent(std1)
    checkError("Перевищення ліміту 1") { limited.addStudent(std2) }
    println(s"Після відмови студентів: ${limited.studentCount}")
    checkError("Ліміт 0") { emptyTeacher.addStudent(std1) }
    println(s"Після відмови студентів: ${emptyTeacher.studentCount}")

    // Додаткове завдання: повторне додавання того самого об'єкта.
    checkError("Повторне додавання") { teach1.addStudent(std1) }
    println(s"Ruslan після відмови: ${teach1.studentCount} студенти")
  }
}
