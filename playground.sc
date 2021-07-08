val test = Array(1, 2, 3, -4)

test.slice(0, 2)

test.slice(0, 3)

(0 until 3).toList

(3 to 3).toList

def intersperse[A](a : List[A], b : List[A]): List[A] = a match {
  case first :: rest => first :: intersperse(b, rest)
  case _             => b
}

// edge case:
//  unknowns is not enough
//  ranked_list is not enough
//
//def diversify(ranked_list: list, unknowns: list) -> list:
//
//    new_list = ranked_list.copy()
//    positions = [5 * i for i in range(len(unknowns))]
//    for el, pos in zip(unknowns, positions):
//        new_list.insert(pos, el)
//
//    return new_list
def diversify[A](rankedList : List[A], unknowns : List[A]): List[A] =
  unknowns.grouped(1).zipAll(rankedList.grouped(4), Nil, Nil).foldLeft(List[A]()) {
     (acc, groups) => acc ::: groups._1 ::: groups._2
  }

print(diversify(List.fill(20)(1), List.fill(5)(0)))
print(diversify(List.fill(0)(1), List.fill(5)(0)))
print(diversify(List.fill(10)(1), List.fill(1)(0)))
print(diversify(List.fill(3)(1), List.fill(3)(0)))
print(diversify(List(3, 8, 4, 6, 3, 2, 3, 4), List(-2, -1, -3, -5)))
