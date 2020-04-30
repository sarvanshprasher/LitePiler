# SER 502 Project
<p align="center">
<img width="200" height="200" src="https://user-images.githubusercontent.com/11274840/80438904-27212680-88ba-11ea-90c8-14ae5c9d3c15.png">
    
    
# LitePiler
This is a group project for SER502. In this project we have developed a programming language and the framework which runs programs written in laungage. Name of language is LitePiler.

## Basic idea about language:

This language was written in four layers : 

- First part was to design a grammar relevant to language and which compiler could understand.We used Context Free Grammar to write programming rules for this language. (**Grammar**)

- Second part was to mould rules according to Parser. This part is used to create parse tree for programs written in  language. (**Parser**)

- Third part was to create Lexer.In this part we created a list of keywords as programming blocks in programming launguage . This layer is responsible for taking out the tokens from programs and map them to parser(parse tree). (**Lexer**)

- Fourth part was to write Interpreter. For the rules written in parser, we have written eval function for them which will help us in evaluating the end result of the program. (**Interpreter**)

## Flow diagram
![alt text](https://user-images.githubusercontent.com/11274840/80473399-ef869e80-88fa-11ea-8e3b-cfd632d43df8.png)

## Tools used for language : 

This language was developed and built on MacOs but it can be compiled and run on any platform. It is also a system indepedent language.

- [SWI-Prolog (Web Version)][5]
- [SWI-Prolog (Desktop Version)][5]
- Any Text Editor - Example(Visual Studio,Atom,SublimeText)

## Build and run the language

1. Write program according to programming syntax. For reference, [See sample codes][7] present in data folder.

2. For running program in environment, clone the [repository][8] to your local system.

3. Open SWI-Prolog runtime environment.(Desktop Version)

4. In the terminal, consult path to your program ---> consult('PATH-TO-YOUR-FILE/SER502-Spring2020-Team20/src/runtime/litepiler.pl'). 

**For Example :-**

    consult('/Users/sarvanshprasher/Desktop/Study material/SER 502/SER502-Spring2020-Team20/src/runtime/litepiler.pl'). 

5. Now for running your program, type this in terminal --->  litepiler('<PATH-TO-YOUR-FILE/Program.lp>').

**For Example :-** 

    litepiler('/Users/sarvanshprasher/Desktop/Study material/SER 502/SER502-Spring2020-Team20/data/fibonacci.lp').

6. A new file wil be created which will contain the parse tree of program with extension .lpy . That parse tree will be automatically executed and loaded into interpreter for giving you the output value on terminal.

## Sample programs for language :
**Consult Program**

<img width="943" alt="1" src="https://user-images.githubusercontent.com/11274840/80689660-7fa32000-8a82-11ea-9b3d-928c7c4bb7a2.png">


- **[Fibonacci Pattern:][9]**

    litepiler('/Users/sarvanshprasher/Desktop/Study material/SER 502/SER502-Spring2020-Team20/data/fibonacci.lp').
    
    <img width="941" alt="8" src="https://user-images.githubusercontent.com/11274840/80689671-83cf3d80-8a82-11ea-8fc3-4862d1c4dd8a.png">

- **[Factorial of a number:][10]** 

    litepiler('/Users/sarvanshprasher/Desktop/Study material/SER 502/SER502-Spring2020-Team20/data/factorialNumber.lp').
    <img width="939" alt="2" src="https://user-images.githubusercontent.com/11274840/80689664-816ce380-8a82-11ea-85ca-ebd90f11f47a.png">


- **[Power of a number:][11]** 

    litepiler('/Users/sarvanshprasher/Desktop/Study material/SER 502/SER502-Spring2020-Team20/data/powerOfNumber.lp').
    <img width="941" alt="6" src="https://user-images.githubusercontent.com/11274840/80689668-8336a700-8a82-11ea-86cd-18d429d95f82.png">

- **[Ternary Operation:][12]** 


    litepiler('/Users/sarvanshprasher/Desktop/Study material/SER 502/SER502-Spring2020-Team20/data/ternary.lp').
    <img width="940" alt="4" src="https://user-images.githubusercontent.com/11274840/80689665-829e1080-8a82-11ea-8793-aff337da0e67.png">

- **[Printing Natural Numbers:][13]** 

    litepiler('/Users/sarvanshprasher/Desktop/Study material/SER 502/SER502-Spring2020-Team20/data/printNumber.lp').
    <img width="940" alt="5" src="https://user-images.githubusercontent.com/11274840/80689666-8336a700-8a82-11ea-85c7-109f0c6fecf3.png">


- **[For Range Print Numbers:][14]** 

    litepiler('/Users/sarvanshprasher/Desktop/Study material/SER 502/SER502-Spring2020-Team20/data/rangeInForLoop.lp').
    <img width="943" alt="7" src="https://user-images.githubusercontent.com/11274840/80689670-83cf3d80-8a82-11ea-85bc-66f46c969937.png">
    
## Features Added :

- Taking input from user
- Printing out on terminal
- Control flow structures (traditional for loop, for range loop, while loop)
- Conditional statement (if else, ternary operation)
- Increment, decrement operation (i++, i--)
- Negative number
- Arithmetic operator (<,>,==,<=,>=)
- Conditional operator (and, or, not)
- Assignment operator (=)
- Datatypes (String, Boolean, Int)
- Comments
- Features Added but not tested :
   - String operation (String length, Concat strings)


## Contributors :

- [Abhishek Haksar][1] 
- [Rohit Kumar Singh][2] 
- [Sarvansh Prasher][3] 
- [Surya Chatterjee][4]


  [1]: https://github.com/Abhi241296
  [2]: https://github.com/rohitksingh
  [3]: https://github.com/sarvanshprasher
  [4]: https://github.com/surya-de
  [5]: http://www.swi-prolog.org
  [6]: http://www.swi-prolog.org/download/stable
  [7]: https://github.com/sarvanshprasher/SER502-Spring2020-Team20/tree/master/data
  [8]: https://github.com/sarvanshprasher/SER502-Spring2020-Team20
  [9]: https://github.com/sarvanshprasher/SER502-Spring2020-Team20/blob/master/data/fibonacci.lp
  [10]: https://github.com/sarvanshprasher/SER502-Spring2020-Team20/blob/master/data/factorialNumber.lp
  [11]: https://github.com/sarvanshprasher/SER502-Spring2020-Team20/blob/master/data/powerOfNumber.lp
  [12]: https://github.com/sarvanshprasher/SER502-Spring2020-Team20/blob/master/data/ternary.lp
  [13]: https://github.com/sarvanshprasher/SER502-Spring2020-Team20/blob/master/data/printNumber.lp
  [14]: https://github.com/sarvanshprasher/SER502-Spring2020-Team20/blob/master/data/rangeInForLoop.lp
  


