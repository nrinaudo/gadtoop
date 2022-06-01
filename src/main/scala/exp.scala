package gadtoop

enum Exp[A]:
  case Literal(value: Int) extends Exp[Int]
  case Plus(lhs: Exp[Int], rhs: Exp[Int]) extends Exp[Int]
  case Minus(lhs: Exp[Int], rhs: Exp[Int]) extends Exp[Int]
  case Equals(lhs: Exp[Int], rhs: Exp[Int]) extends Exp[Boolean]
  case And(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Exp[Boolean]
  case Or(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Exp[Boolean]
  case Cond(test: Exp[Boolean], ifTrue: Exp[A], ifFalse: Exp[A])
  case Tuple[Left, Right](left: Exp[Left], right: Exp[Right]) extends Exp[(Left, Right)]
  case Fst[Left, Right](tuple: Tuple[Left, Right]) extends Exp[Left]
  case Snd[Left, Right](tuple: Tuple[Left, Right]) extends Exp[Right]

object Exp:
  def eval[A](exp: Exp[A]): A = exp match
    case Literal(value)              => value
    case Plus(lhs, rhs)              => eval(lhs) + eval(rhs)
    case Minus(lhs, rhs)             => eval(lhs) - eval(rhs)
    case Equals(lhs, rhs)            => eval(lhs) == eval(rhs)
    case And(lhs, rhs)               => eval(lhs) && eval(rhs)
    case Or(lhs, rhs)                => eval(lhs) || eval(rhs)
    case Cond(test, ifTrue, ifFalse) => if eval(test) then eval(ifTrue) else eval(ifFalse)
    case Tuple(left, right)          => (eval(left), eval(right))
    case Fst(tuple)                  => eval(tuple)._1
    case Snd(tuple)                  => eval(tuple)._2

  def eq[A](lhs: Exp[A], rhs: Exp[A]): Boolean = lhs match
    case Literal(left) =>
      rhs match
        case Literal(right) => left == right
        case _              => false

    case Tuple(left1, right1) =>
      rhs match
        // This is the "magic". Through equational reasoning, the compiler is able to deduce that left1 and left2 are
        // of the same type, and that right1 and right2 are of the same type.
        // - lhs is Tuple[L1, R1], rhs is Tuple[L2, R2]
        // - they're also both Exp[A], so Tuple[L1, R1] = Tuple[L2, R2]
        // - Tuple is injective, which means that L1 = L2 and R1 = R2
        // - This allows us to conclude that Exp[L1] = Exp[L2] and Exp[R1] = Exp[R2]
        case Tuple(left2, right2) => eq(left1, left2) && eq(right1, right2)
        case _                    => false

    case _ => false
