import scala.util.Random

// Адреса з перевіркою країни та міста.
class Address(val country: String, val city: String) {
  require(country.trim.nonEmpty, "Країна не може бути порожньою")
  require(city.trim.nonEmpty, "Місто не може бути порожнім")
  override def toString: String = s"$country, $city"
}

// Спільні дані для студента і викладача.
class Human(private val _name: String, private val _age: Int, val address: Address) {
  require(_name.trim.nonEmpty, "Ім'я не може бути порожнім")
  require(_age >= 0, "Вік не може бути від'ємним")
  def name: String = _name
  def age: Int = _age
  def show(): String = s"Ім'я: $name, вік: $age, адреса: $address"
}

class Student(name: String, age: Int, val group: String, address: Address)
    extends Human(name, age, address) {
  require(group.trim.nonEmpty, "Група не може бути порожньою")
  // Оцінка студента випадкова, від 1 до 5.
  val estimation: Int = Random.nextInt(5) + 1
  override def show(): String = s"${super.show()}, група: $group, оцінка: $estimation"
}

class Teacher(name: String, age: Int, val salary: Double, val limit: Int, address: Address)
    extends Human(name, age, address) {
  require(salary >= 0, "Зарплата не може бути від'ємною")
  require(limit >= 0, "Ліміт не може бути від'ємним")
  private var _students: List[Student] = List.empty

  def students: List[Student] = _students
  def studentCount: Int = _students.size

  // Додаю студента, якщо є місце і його ще немає у списку.
  def addStudent(student: Student): Unit = {
    require(studentCount < limit, "Ліміт студентів перевищено")
    require(!_students.exists(_ eq student), "Цього студента вже додано")
    _students = _students :+ student
  }

  def findStudentsByGroup(group: String): List[Student] = _students.filter(_.group == group)

  // Для викладача без студентів повертаю None.
  def averageEstimation: Option[Double] = {
    if (_students.isEmpty) None
    else Some(_students.map(_.estimation).sum.toDouble / _students.size)
  }

  override def show(): String = {
    s"${super.show()}, зарплата: $salary, ліміт: $limit, студентів: $studentCount"
  }
}

// Тут зберігаю викладачів і виконую пошук та сортування.
class DataList(private val _teachers: List[Teacher]) {
  def teachers: List[Teacher] = _teachers
  def printAllInformation(): Unit = {
    _teachers.foreach { teacher =>
      println(teacher.show())
      teacher.students.foreach(student => println("  " + student.show()))
    }
  }
  def findTeachers(count: Int): List[Teacher] = _teachers.filter(_.studentCount > count)
  def sortByStudentCount(): List[Teacher] = _teachers.sortBy(teacher => -teacher.studentCount)
  def findTeachersByCity(city: String): List[Teacher] = _teachers.filter(_.address.city == city)
  def totalStudentAssignments: Int = _teachers.map(_.studentCount).sum
}

object Lab2 {
  // Окремо перевіряю помилки, щоб програма не зупинялася.
  def checkError(title: String)(action: => Unit): Unit = {
    try {
      action
      println(s"$title: помилки не було")
    } catch {
      case e: IllegalArgumentException => println(s"$title: ${e.getMessage}")
    }
  }

  def main(args: Array[String]): Unit = {
    // Створюю початкові дані для перевірки методів.
    val kyiv = new Address("Ukraine", "Kyiv")
    val lviv = new Address("Ukraine", "Lviv")
    val std1 = new Student("Ivan", 19, "131 M", kyiv)
    val std2 = new Student("Valerii", 20, "131 M", kyiv)
    val std3 = new Student("Anna", 18, "132 M", kyiv)
    val std4 = new Student("Oleh", 21, "133 M", kyiv)
    val std5 = new Student("Maria", 19, "133 M", kyiv)
    val teach1 = new Teacher("Ruslan", 40, 20000, 10, kyiv)
    val teach2 = new Teacher("Andry", 35, 18000, 11, kyiv)
    teach1.addStudent(std1)
    teach1.addStudent(std2)
    teach1.addStudent(std3)
    teach2.addStudent(std4)
    teach2.addStudent(std5)
    val teach3 = new Teacher("Olena", 32, 15000, 2, lviv)
    val data = new DataList(List(teach1, teach2, teach3))

    println("=== Інформація про викладачів та студентів ===")
    data.printAllInformation()
    println("\n=== Пошук за кількістю студентів ===")
    List(0, 2, 3, 10).foreach { threshold =>
      println(s"Поріг $threshold: ${data.findTeachers(threshold).map(_.name)}")
    }
    println("\n=== Сортування ===")
    data.sortByStudentCount().foreach(t => println(s"${t.name}: ${t.studentCount}"))
    println(s"Початковий порядок: ${data.teachers.map(_.name)}")

    println("\n=== Поліморфізм ===")
    val people: List[Human] = List(std1, teach1)
    people.foreach(person => println(person.show()))

    println("\n=== Пошук за групою ===")
    println(s"131 M: ${teach1.findStudentsByGroup("131 M").map(_.name)}")
    println(s"Невідома група: ${teach1.findStudentsByGroup("немає").map(_.name)}")
    println("\n=== Середня оцінка ===")
    println(s"Ruslan: ${teach1.averageEstimation}")
    println(s"Olena (без студентів): ${teach3.averageEstimation}")
    println("\n=== Пошук за містом ===")
    println(s"Kyiv: ${data.findTeachersByCity("Kyiv").map(_.name)}")
    println(s"Lviv: ${data.findTeachersByCity("Lviv").map(_.name)}")
    println(s"Odesa: ${data.findTeachersByCity("Odesa").map(_.name)}")
    println(s"Усього записів про студентів: ${data.totalStudentAssignments}")
    println(s"У порожньому DataList: ${new DataList(Nil).totalStudentAssignments}")

    println("\n=== Перевірки помилкових даних ===")
    checkError("Від'ємний вік") { new Human("Test", -1, kyiv) }
    checkError("Від'ємна зарплата") { new Teacher("Test", 30, -1, 2, kyiv) }
    checkError("Від'ємний ліміт") { new Teacher("Test", 30, 1000, -1, kyiv) }
    checkError("Порожнє ім'я") { new Human("", 20, kyiv) }
    checkError("Ім'я з пробілів") { new Human("   ", 20, kyiv) }
    checkError("Порожня група") { new Student("Test", 20, "", kyiv) }
    checkError("Група з пробілів") { new Student("Test", 20, "   ", kyiv) }
    checkError("Порожня країна") { new Address("", "Kyiv") }
    checkError("Країна з пробілів") { new Address("   ", "Kyiv") }
    checkError("Порожнє місто") { new Address("Ukraine", "") }
    checkError("Місто з пробілів") { new Address("Ukraine", "   ") }

    val limited = new Teacher("Limited", 30, 1000, 1, kyiv)
    limited.addStudent(std1)
    checkError("Перевищення ліміту") { limited.addStudent(std2) }
    println(s"Кількість після відмови: ${limited.studentCount}")
    val zeroLimit = new Teacher("Zero", 30, 1000, 0, kyiv)
    checkError("Ліміт 0") { zeroLimit.addStudent(std1) }
    println(s"Кількість після відмови: ${zeroLimit.studentCount}")
    checkError("Повторне додавання студента") { teach1.addStudent(std1) }
    println(s"Кількість після відмови: ${teach1.studentCount}")
  }
}
