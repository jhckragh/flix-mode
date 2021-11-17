def main(args: Array[String]): Int32 & Impure =
    let argList = Array.toList(args);
    match argList {
        case Nil =>
            println("Example usage: java -jar flix.jar Calc.flix --args \"1 + 2 * 3\"");
            1
        case _ =>
            let input = String.unwords(argList);
            match Parsing.parse(input) {
                case Ok(exp) =>
                    println(exp);
                    println(eval(exp));
                    0
                case Err(msg) =>
                    println("Error: ${msg}");
                    1
            }
    }

def eval(exp: Expression): Int32 =
    match exp {
        case Binary(left, op, right) =>
            let x = eval(left);
            let y = eval(right);
            match op {
                case Add => x + y
                case Sub => x - y
                case Mul => x * y
                case Div => x / y
            }
        case Number(n) => n
    }

/// --------

enum Operator {
    case Add, Sub, Mul, Div
}

enum Expression {
    case Binary(Expression, Operator, Expression),
    case Number(Int32)
}

instance ToString[Operator] {
    pub def toString(op: Operator): String = match op {
        case Add => "+"
        case Sub => "-"
        case Mul => "*"
        case Div => "/"
    }
}

instance ToString[Expression] {
    pub def toString(exp: Expression): String = match exp {
        case Binary(left, op, right) =>
            let leftStr = ToString.toString(left);
            let opStr = ToString.toString(op);
            let rightStr = ToString.toString(right);
            "(${opStr} ${leftStr} ${rightStr})"
        case Number(n) => ToString.toString(n)
    }
}

/// --------

namespace Parsing {
    type alias Progress = (Expression, List[String])

    pub def parse(s: String): Result[Expression, String] & Impure =
        let tokens = lex(s);
        match parseAdditive(tokens) {
            case Ok((exp, tokensRemaining)) =>
                match tokensRemaining {
                    case Nil => Ok(exp)
                    case t :: _ => Err("Unexpected token '${t}'")
                }
            case Err(msg) => Err(msg)
        }

    def lex(s: String): List[String] & Impure =
        let symbols = "+" :: "-" :: "*" :: "/" :: "(" :: ")" :: Nil;
        let expanded = List.foldLeft((str, sym) -> String.replace(sym, " ${sym} ", str), s, symbols);
        Array.toList(String.split(String.trim(expanded), "\\s+"))

    def parseAdditive(tokens: List[String]): Result[Progress, String] =
        parseMultiplicative(tokens) |>
        Result.flatMap(progress1 -> {
            let (exp1, tokensAfterExp1) = progress1;
            match tokensAfterExp1 {
                case head :: tail if head == "+" or head == "-" =>
                    parseMultiplicative(tail) |>
                    Result.flatMap(progress2 -> {
                        let (exp2, tokensAfterExp2) = progress2;
                        let op = operatorFromString(head);
                        parseAdditiveHelper(Binary(exp1, op, exp2), tokensAfterExp2)
                    })
                case _ => Ok(progress1)
            }
        })

    def parseAdditiveHelper(acc: Expression, tokens: List[String]): Result[Progress, String] =
        match tokens {
            case head :: tail if head == "+" or head == "-" =>
                parseMultiplicative(tail) |>
                Result.flatMap(progress -> {
                    let (exp, tokensAfterExp) = progress;
                    let op = operatorFromString(head);
                    parseAdditiveHelper(Binary(acc, op, exp), tokensAfterExp)
                })
            case _ => ??? //Ok((acc, tokens))
        }

    def parseMultiplicative(tokens: List[String]): Result[Progress, String] =
        parseUnary(tokens) |>
        Result.flatMap(progress1 -> {
            let (exp1, tokensAfterExp1) = progress1;
            match tokensAfterExp1 {
                case head :: tail if head == "*" or head == "/" =>
                    parseUnary(tail) |>
                    Result.flatMap(progress2 -> {
                        let (exp2, tokensAfterExp2) = progress2;
                        let op = operatorFromString(head);
                        parseMultiplicativeHelper(Binary(exp1, op, exp2), tokensAfterExp2)
                    })
                case _ => Ok(progress1)
            }
        })

    def parseMultiplicativeHelper(acc: Expression, tokens: List[String]): Result[Progress, String] =
        match tokens {
            case head :: tail if head == "*" or head == "/" =>
                parseUnary(tail) |>
                Result.flatMap(progress -> {
                    let (exp, tokensAfterExp) = progress;
                    let op = operatorFromString(head);
                    parseMultiplicativeHelper(Binary(acc, op, exp), tokensAfterExp)
                })
            case _ => Ok((acc, tokens))
        }

    def parseUnary(tokens: List[String]): Result[Progress, String] =
        // TODO: '(' expression ')'
        match tokens {
            case "+" :: tail =>
                parseUnary(tail) |>
                Result.map(progress -> {
                    let (exp, tokensAfterExp) = progress;
                    (Binary(Number(0), Add, exp), tokensAfterExp)
                })
            case "-" :: tail =>
                parseUnary(tail) |>
                Result.map(progress -> {
                    let (exp, tokensAfterExp) = progress;
                    (Binary(Number(0), Sub, exp), tokensAfterExp)
                })
            case head :: tail =>
                match Int32.fromString(head) {
                    case Ok(n) => Ok((Number(n), tail))
                    case _ => Err("The token '${head}' is not a valid decimal numeral")
                }
            case _ => Err("Expression ended unexpectedly")
        }

    def operatorFromString(s: String): Operator =
        match s {
            case "+" => Add
            case "-" => Sub
            case "*" => Mul
            case "/" => Div
            case _ => ?bug
        }
}
