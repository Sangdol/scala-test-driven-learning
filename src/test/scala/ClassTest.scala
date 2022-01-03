import org.scalatest.funsuite.AnyFunSuite

// No public: A scala file can have multiple classes and all of them are public.

class ClassTest extends AnyFunSuite {
  class Counter {
    private var count = 0

    def increment(): Unit = { count += 1 }

    def current(): Int = count

    def currentWithoutParens: Int = count
  }

  test("Counter") {
    val counter = new Counter
    counter.increment() // mutator
    assert(counter.current == 1) // accessor
    assert(
      counter.currentWithoutParens == 1
    ) // accessor which doesn't allow parens.
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
    class Person2(var age: Int = 0) {}

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

      def newUniqueNumber: Int = { lastNumber += 1; lastNumber }

      // Can be used like Array(100)
      def apply(balance: Double): Account = {
        new Account(balance)
      }
    }

    class Account {
      val id: Int = Account.newUniqueNumber

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

  test("Self-Type") {
    // https://docs.scala-lang.org/tour/self-types.html
    trait User {
      def username: String
    }

    trait Tweeter {
      this: User => // reassign this
      def tweet(tweetText: String) = s"$username: $tweetText"
    }

    // We mixin User because Tweeter required it
    class VerifiedTweeter(val username_ : String) extends Tweeter with User {
      def username = s"real $username_"
    }

    val sang = new VerifiedTweeter("sang")
    assert(sang.tweet("Hello") == "real sang: Hello")
  }

  test("constructor") {
    // https://docs.scala-lang.org/tour/classes.html

    // var: set and get
    class Point(var x: Int, var y: Int)

    val p = new Point(1, 2)
    p.x = 2

    // no identifier no access / visible only inside the class
    class Point2(x: Int, y: Int)
    val p2 = new Point2(1, 2)

    // val: only get
    class Point3(val x: Int, val y: Int)
    val p3 = new Point3(1, 2)
    p3.x
  }

  test("inheritance") {
    class Parent(n: Int) {
      def getN: Int = n
    }
    class Child(n: Int) extends Parent(n) {
      def getDoubleN: Int = n * 2
    }

    val p = new Parent(1)
    assert(p.getN == 1)

    val c = new Child(1)
    assert(c.getDoubleN == 2)
  }

  /**
   * https://stackoverflow.com/questions/9443004/what-does-the-operator-mean-in-scala
   */
  test("# nested class") {
    class A {
      class B

      def f(b: B) = 1
      def g(b: A#B) = 2
    }

    val a1 = new A
    val a2 = new A

    // This is not possible since nested classes are path dependent.
    //a2.f(new a1.B)

    assert(a1.g(new a1.B) == 1)
    assert(a2.g(new a1.B) == 2)
  }
}
