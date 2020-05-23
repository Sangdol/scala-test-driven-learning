import org.scalatest.funsuite.AnyFunSuite

// No public: A scala file can have multiple classes and all of them are public.
class Counter {
  private var count = 0

  def increment() { count += 1 }

  def current() = count

  def currentWithoutParens = count
}

class ClassTest extends AnyFunSuite {

  test("Counter") {
    val counter = new Counter
    counter.increment()  // mutator
    assert(counter.current == 1)  // accessor
    assert(counter.currentWithoutParens == 1) // accessor which doesn't allow parens.
  }

  test("Getter and setter") {
    // Scala generates getter and setter
    // You can see them if you run
    //   javap -private Person
    class Person {
      var age = 0

      // auxiliary constructor
      def this(age: Int) {
        // Each auxiliary constructor must start with
        // a call to a previously defined auxiliary constructor
        // or the primary constructor.
        this()
        this.age = age
      }
    }

    val person = new Person
    person.age = 1
    assert(person.age == 1)

    val oldPerson = new Person(100)
    assert(oldPerson.age == 100)

    class Immortal {
      val age = 10 // final
    }

    val immortal = new Immortal
    assert(immortal.age == 10)

    // This has a constructor with a parameter with a default value.
    // This can eliminate the auxiliary constructor.
    class Person2(var age: Int = 0) {
    }

    val person2 = new Person2
    person2.age = 10
    assert(person2.age == 10)

    val oldPerson2 = new Person2(100)
    assert(oldPerson2.age == 100)
  }

  test("Object") {
    // Companion object of Accounts class
    // Singleton
    object Account {
      private var lastNumber = 0

      def newUniqueNumber = { lastNumber += 1; lastNumber }

      // Can be used like Array(100)
      def apply(balance: Double) = {
        new Account(balance)
      }
    }

    class Account {
      val id = Account.newUniqueNumber

      var balance = 0.0

      def this(balance: Double) {
        this()
        this.balance = balance
      }
    }

    assert(Account.newUniqueNumber == 1)

    val account = new Account
    assert(account.id == 2)

    val richAccount = Account(100_000)
    assert(richAccount.balance == 100_000)
  }

  // https://docs.scala-lang.org/tour/case-classes.html
  test("Case Class") {
    case class Book(isbn: String)

    val b1 = Book("1")
    assert(b1.isbn == "1")

    val b2 = Book("1")
    assert(b1 == b2)

    val b3 = b2.copy(isbn = "2")
    assert(b3.isbn == "2")

    def bookGenerator: (String) => Book = (isbn) => Book(f"isbn: $isbn")

    assert(bookGenerator("123").isbn == "isbn: 123")

    def bookGenerator2(isbn: String): Book = {
      Book(f"ISBN: $isbn")
    }

    assert(bookGenerator2("111").isbn == "ISBN: 111")
  }
}
