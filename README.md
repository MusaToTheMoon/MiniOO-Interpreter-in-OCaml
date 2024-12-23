# **MiniOO Interpreter**  
### **A Lightweight Object-Oriented Language Interpreter**  
**Author**: Muhammad Musa Khan  
**Course**: CSCI-GA 3110 Honors Programming Languages  
**SCHOOL**: New York University, Courant Institute of Mathematical Sciences

## **1. Project Overview**  

The **MiniOO Interpreter** is a functional implementation of a simple object-oriented programming language, MiniOO. It supports key programming language constructs such as procedures, dynamic memory allocation, and conditional statements, etc.

This project uses **OCaml** with components like **Menhir** for parsing and **OCamllex** for lexical analysis. The interpreter implements:  
- **Static semantics** for lexical scoping validation.  
- **Small-step operational semantics** for program execution.  


## **2. How to Run the Interpreter**  

### **Requirements**  
- **OCaml** (v4.12.0 or later)  
- **Menhir**: Install it via OPAM:  
   ```bash
   opam install menhir
   ```  


### **Steps to Run the Interpreter**  
1. **Unzip the Project**: Unzip all project files into a folder.  
2. **Build the Project**: Run the `make` command to compile the project:  
   ```bash
   make all
   ```  
3. **Run the Interpreter**:  
   - To execute a MiniOO program from a file:  
     ```bash
     ./interpreter < examples/input1.oo
     ```  
   - To enter MiniOO code manually (interactive mode):  
     ```bash
     ./interpreter
     ```  
     Exit interactive mode by pressing `Ctrl+D` twice.  
4. **Clean Up**: To remove all compiled files and executables:  
   ```bash
   make delete
   ```  


## **3. Project Structure**  

- **examples/**: Contains sample MiniOO programs showcasing various features.  
- **makefile**: Automates the build and clean-up processes.  
- **main.ml**: Entry point of the interpreter.  
- **lexer.mll**: Implements lexical analysis.  
- **parser.mly**: Implements syntax analysis using Menhir.  
- **ast.ml**: Defines the Abstract Syntax Tree (AST) for MiniOO.  
- **operationalSemantics.ml**: Implements runtime behavior of MiniOO programs.  
- **staticSemantics.ml**: Implements static checks to ensure program correctness.  
- **prettyPrint.ml**: Provides human-readable representations of AST and state.  
- **operationalTypes.ml**: Defines runtime components like stack, memory heap, and state.  


## **4. Features of MiniOO**  

### **Some of the Implemented Features**  
1. Variable Declaration and Assignment  
2. Procedures and Function Calls
3. Conditional Statements
4. While Loops
5. Field Access and Dynamic Memory Allocation (malloc)
6. Parallel Execution and Atomic Blocks
7. OCaml style nested multi-line comments 


## **5. Example Programs**  

Below are the MiniOO example programs included in the `examples/` directory.

### **1. Variable Declaration and Assignment** (`input1.oo`)  
```miniOO
(* Variable Declaration and Assignment *)
var x;
x = 10;
if (x > 5) then {print(x);} else {print(0);};
```
- Demonstrates variable assignment and conditionals.


### **2. Procedures and Function Calls** (`input2.oo`)  
```miniOO
(* Procedure and Function Call *)
var p;
p(5);
p = proc y: print(y + 10); print(0);;
```
- Demonstrates procedure declaration and calls.


### **3. Conditional Statements** (`input3.oo`)  
```miniOO
(* Conditional Statements *)
var x;
x = 10;
if (x > 5) then {print(x);} else {print(0);};
x = 2;
if x < 5 then print(x); else print(1);;
x = 10;
if x == 10 then print(2);;
```
- Supports optional parentheses and optional `else` branches.


### **4. While Loop and Arithmetic Operators** (`input4.oo`)  
```miniOO
(* While Loop and Arithmetic Operators *)
var x;
x = (10 * 2) / 4 + 1 - 2;
while (x > 0) {
    print(x);
    x = x - 1;
};
```
- Demonstrates loops, arithmetic operators, and semi-colon rules.


### **5. Field Access and Malloc** (`input5.oo`)  
```miniOO
(* Field Access and Malloc *)
var x;
x.F = 100;
print(x.F);
(* malloc(x); *) (* Uncomment this line to allocate memory and prevent errors *)
```
- Requires `malloc` for dynamic object allocation.


### **6. Parallel and Atomic Execution** (`input6.oo`)  
```miniOO
(* Parallel Execution *)
var x;
var y;
{
    x = 1; print(x); print(x);
|||
    y = 2; atom(print(y); print(y););
};
```
- Demonstrates parallel execution with the `|||` operator.  
- Atomic blocks ensure uninterrupted execution.


### **7. Factorial Calculation** (`input7.oo`)  
```miniOO
(* Factorial Calculation *)
var factorial;
var result;
result = 1;
factorial = proc n: {
    if (n == 0) then {
        print(result);
    } else {
        result = result * n;
        factorial(n - 1);
    };
};
factorial(5);
```
- Recursive procedure to calculate factorial.


## **6. Notes**  
- **Syntax Rules**:  
   - Variable names start with lowercase letters.  
   - Field names start with uppercase letters.  
   - Semi-colons are required after the last command.  

- **Limitations**:  
   - Fields must be integers, not variable objects.  
   - Parallel execution randomly selects one branch.  


## **7. Example Usage**  

To run the factorial example:  
```bash
./interpreter < examples/input7.oo
```

**Output**:  
```
120
```


## **8. Additional Information**  

This interpreter adheres to the formal syntax and operational semantics of MiniOO as described in the project specification. The project implements both **static semantics** (e.g., scoping rules) and **operational semantics** for a clear and robust execution of MiniOO programs.  

For more details, refer to the **Mini-SOS.pdf** document or explore the project files.


## **9. Acknowledgments**  

This project was implemented as part of the **CSCI-3110 Honors Programming Languages** course.
