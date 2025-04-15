(*1.Расстановка скобок в инфиксной арифметической записи в соответствии с общепринятыми приоритетами операций.*)
(*Работа с целыми и вещественными числами.
Отрицательные числа записываются в скобках(-1).Доступны переменны a,b,c,d,e,f,g,x,y.*)

fun CheckIfCharIsDigit(ch) = 
    let
        val numbers = [ #"0", #"1", #"2", #"3", #"4", #"5", #"6", #"7", #"8", #"9", #"a", #"b", #"c", #"d",#"e",#"f",#"g",#"x",#"y",#"."]
        fun check(ch, nil) = false
        |   check(ch, h::t) = if ch = h then true else check(ch, t)
    in
        check(ch, numbers)
    end;
    
fun CheckIfCharIsMathSign(ch) =
    let
        val operands = [ #"+", #"-", #"*", #"/"]
        fun check(ch, nil) = false
        |   check(ch, h::t) = if ch = h then true else check(ch, t)
    in
        check(ch,operands)
    end;
        
fun FromStringListToString(list) =
    let
        fun toString(nil, str) = str
        |   toString(h::t, str) = toString(t, str ^ h)
    in
        toString(list,"")
    end;

fun stringContainsBracket(s) = if substring(s,0,1) = "(" then true else false;
fun DeleteFirstAndLastBracket(expressionStr) = substring(expressionStr,1,size(expressionStr)-2);
fun CheckIfMulOrDiv(s) = if (s = "*") orelse (s = "/") then true else false;
fun CheckIfAddOrSub(s) = if (s = "-") orelse (s = "+") then true else false;
fun howManyOperandsInString(s) =
    let
        val charList = explode s
        fun check(nil, counter) = counter
        |   check(h::t, counter) = if CheckIfCharIsMathSign(h) then check(t, counter+1) else check(t, counter)
    in
        check(charList, 0)
    end;
    
fun CreateStringList(expressionStr) =
    let
        val charList = explode expressionStr
        fun create(nil, newStringList, buffer, bracketsCounter, brackets) = if buffer = "" then newStringList else newStringList @ [buffer]
          | create(h::t, newStringList, buffer, bracketsCounter, brackets) = 
                if h = #"("
                then 
                    create(t, newStringList, buffer ^ str h, bracketsCounter + 1, true) 
                else if h = #")" 
                    then if bracketsCounter = 1 
                        then create(t, newStringList @ [buffer ^ str h], "", bracketsCounter - 1, false)
                        else create(t, newStringList, buffer ^ str h, bracketsCounter - 1, true)
                else if CheckIfCharIsDigit h 
                    then create(t, newStringList, buffer ^ str h, bracketsCounter, brackets)
                else if CheckIfCharIsMathSign h
                        then if not brackets
                            then if buffer = ""
                                then create(t, newStringList @ [str h], "", bracketsCounter, false) 
                                else create(t, newStringList @ [buffer] @ [str h], "", bracketsCounter, false)
                        else create(t, newStringList, buffer ^ str h, bracketsCounter, true)
                else 
                    create(t, newStringList, buffer, bracketsCounter, brackets)  
    in
        create(charList, nil, "", 0, false) 
    end;

(* "1+3+4" = ["1","+","3","+","4"] *)
(* "(1+3)*4" = ["(1+3)","*","4"] *)
(* "11+14*566/(34+24)" = ["11","+","14","*","566","/","(34+24)"] *)
(* "(1+54*b+a)+c+b" = ["(1+54*b+a)","+","c","+","b"] *)


fun AddBracketsForMulAndDiv(expressionList) =
    let
        fun AddBrackets(nil, newExpressionList) = newExpressionList
          | AddBrackets([x], newExpressionList) = newExpressionList @ [x]  
          | AddBrackets(fExpression::oper::sExpression::t, newExpressionList) = 
                if CheckIfMulOrDiv(oper)
                then AddBrackets("(" ^ fExpression ^ oper ^ sExpression ^ ")" :: t, newExpressionList)  
                else AddBrackets(sExpression::t, newExpressionList @ [fExpression, oper]);  
    in
        AddBrackets(expressionList, nil)
    end;
    
(*["1","*","2","+","4"] -> ["(1*2)","+","4"] *)

fun AddBracketsForAddAndSub(expressionList) =
    let
        fun AddBrackets(nil, newExpressionList) = newExpressionList
          | AddBrackets([x], newExpressionList) = newExpressionList @ [x]  
          | AddBrackets(fExpression::oper::sExpression::t, newExpressionList) = 
                if CheckIfAddOrSub(oper)
                then AddBrackets("(" ^ fExpression ^ oper ^ sExpression ^ ")" :: t, newExpressionList)  
                else AddBrackets(sExpression::t, newExpressionList @ [fExpression, oper]);  
    in
        AddBrackets(expressionList, nil)
    end;
    
fun AddBrackets(expressionList) =
    let
        fun Add(nil, newExpressionList) = newExpressionList
            | Add(h::t, newExpressionList) =
        if stringContainsBracket(h) andalso howManyOperandsInString(h) > 1 
        then
                let
                    val depthExpression = DeleteFirstAndLastBracket(h)
                    val depthExprWithBrackets = AddBrackets(CreateStringList(depthExpression))
                in
                    Add(t, newExpressionList @ [FromStringListToString(depthExprWithBrackets)])
                end
        else
            Add(t, newExpressionList @ [h])
    in
        AddBracketsForAddAndSub(AddBracketsForMulAndDiv(Add(expressionList, nil)))
    end;

fun add_brackets s = 
    let
        val expressionList = CreateStringList(s)
        val expressionWithBrackets = AddBrackets(expressionList)
    in
        DeleteFirstAndLastBracket(FromStringListToString(expressionWithBrackets))
    end;
    
add_brackets "(1+4+6*8)+2/23-(-1)+(4*7*7)";
add_brackets "1+2+3";
add_brackets "1*9+90+a/b";
add_brackets "8+9*2-(5-2*2)-44";
add_brackets "1+2+3*2";
add_brackets "(1+8*9+a-2/c)-7*9+54-25";
add_brackets "(1-2)*54/22";
add_brackets "1*90/(24-22)";
add_brackets "0.1+0.9*1.789-(0.99873+33.353*90.12)";