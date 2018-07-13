package nanocc

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
	"reflect"
	"regexp"
	"strconv"
	"strings"
	"unsafe"
)

const (
	sizeofbyte = 1
	sizeofint  = int(strconv.IntSize / 8)
)

var (
	out  io.Writer = os.Stdout
	file []byte    // source code
	mem  []int     // VM memory: strings, data/bss, emitted instructions, stack
	// and byte representation of mem:
	memb = (*[]byte)(unsafe.Pointer((*reflect.SliceHeader)(unsafe.Pointer(&mem))))
)

var (
	p, lp int // current position in source code
	data  int // data/bss "pointer"
)

var (
	e, le int   // current position in emitted code
	id    []int // currently parsed identifier
	sym   []int // symbol table (simple list of identifiers)
	tk    int   // current token
	ival  int   // current token value
	ty    int   // current expression type
	loc   int   // local variable offset
	line  int   // current line number
	src   bool  // print source and assembly flag
	debug bool  // print executed instructions flag
)

// tokens and classes (operators last and in precedence order)
const ( // for example: `id[Class] = Num`
	Num = 128 + iota // constant value
	Fun              // function
	Sys              // system call
	Glo              // global variables
	Loc              // local variables
	ID               // user-defined identifier

	Char
	Else
	Enum
	If
	Int
	Return
	Sizeof
	While

	// operators
	Assign // =
	Cond   // ?
	Lor    // ||
	Lan    // &&
	Or     // |
	Xor    // ^
	And    // &
	Eq     // ==
	Ne     // !=
	Lt     // <
	Gt     // >
	Le     // <=
	Ge     // >=
	Shl    // <<
	Shr    // >>
	Add    // +
	Sub    // -
	Mul    // *
	Div    // /
	Mod    // %
	Inc    // ++
	Dec    // --
	Brak   // [
)

// opcodes
const (
	LEA = iota
	IMM
	JMP
	JSR
	BZ
	BNZ
	ENT
	ADJ
	LEV
	LI
	LC
	SI
	SC
	PSH
	OR
	XOR
	AND
	EQ
	NE
	LT
	GT
	LE
	GE
	SHL
	SHR
	ADD
	SUB
	MUL
	DIV
	MOD
	OPEN
	READ
	CLOS
	PRTF
	MALC
	FREE
	MSET
	MCMP
	EXIT
)

// types
const (
	CHAR = iota
	INT
	PTR
)

// identifier offsets (since we can't create an `ident` struct)
const (
	// Since the author did not implement the structure, the space pointed by `id[]` is divided into
	// `Idsz`-sized blocks (simulation structures). When `id[]` points to the beginning of the block,
	// `id[0] == id[Tk]` accesses the data of the Tk member.
	Tk = iota
	Hash
	Name  // is the name of this identifier
	Class // is a type, such as `Num` (constant value), `Fun` (function), `Sys` (system call), `Glo` (global var) etc.
	Type  // is a data type (such as return value type), such as `CHAR`, `INT`, `INT+PTR`
	Val
	HClass
	HType
	HVal
	Idsz
)

func next() { // lexical analysis
	var pp int
	// using loop to ignore whitespace characters, but characters that cannot be recognized by
	// the lexical analyzer are considered blank characters, such as '@', '$'
	for tk = int(file[p]); tk != 0; tk = int(file[p]) {
		p++
		if tk == '\n' {
			if src { // src is `-s` cli flag, which means output source code and corresponding bytecode
				fmt.Fprintf(out, "%d: %s", line, string(file[lp:p]))
				lp = p
				for le < e {
					le++
					fmt.Fprintf(out, "%8.4s", ("LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ," +
						"PSH ,OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ," +
						"OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,")[mem[le]*5:])
					if mem[le] <= ADJ { // instructions before `ADJ` have operands
						le++
						fmt.Fprintf(out, " %d\n", mem[le])
					} else {
						fmt.Fprint(out, "\n")
					}
				}
			}
			line++
		} else if tk == '#' { // skipping the remaining characters of the line after "#"
			for file[p] != 0 && file[p] != '\n' {
				p++
			}
		} else if (tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_' {
			// pp point to start of the current symbol
			// because it was `p++`, put `pp` on the previous char relative to `p`
			pp = p - 1
			for (file[p] >= 'a' && file[p] <= 'z') || (file[p] >= 'A' && file[p] <= 'Z') ||
				(file[p] >= '0' && file[p] <= '9') || (file[p] == '_') {
				tk = tk*147 + int(file[p]) // calculate the hash value, `tk` starts with `file[pp]`
				p++
			}
			tk = (tk << 6) + (p - pp) // hash plus symbol length
			id = sym
			for id[Tk] != 0 {
				if tk == id[Hash] && bytes.Equal(file[id[Name]:id[Name]+p-pp], file[pp:p]) { // find the same name, then
					tk = id[Tk] // `tk = id[Tk]`; think of id as a structure, Tk is the member of it (explained above)
					return
				}
				id = id[Idsz:] // go to next identifier in the table
			}
			id[Name] = pp
			id[Hash] = tk
			id[Tk] = ID // token type identifier
			tk = ID
			return
		} else if tk >= '0' && tk <= '9' { // first byte is a number and it is considered a numerical value
			if ival = tk - '0'; ival != 0 { // first digit is not 0, which is considered a decimal number
				for file[p] >= '0' && file[p] <= '9' {
					ival = ival*10 + int(file[p]) - '0'
					p++
				}
			} else if file[p] == 'x' || file[p] == 'X' { // first digit is 0 and it starts with 'x'
				p++ // and is considered to be a hexadecimal number
				tk = int(file[p])
				for (tk != 0) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')) {
					ival = ival*16 + (tk & 15)
					if tk >= 'A' {
						ival += 9
					}
				}
			} else { // considered octal
				for file[p] >= '0' && file[p] <= '7' {
					ival = ival*8 + int(file[p]) - '0'
					p++
				}
			}
			tk = Num // token is numeric, return
			return
		} else if tk == '/' {
			if file[p] == '/' { // two '/' at the beginning, single-line comments
				p++
				for file[p] != 0 && file[p] != '\n' {
					p++
				}
			} else {
				// division
				tk = Div
				return
			}
		} else if tk == '\'' || tk == '"' { // quotes start with character (string)
			pp = data
			for file[p] != 0 && file[p] != byte(tk) { // until the matching quotes are found
				ival = int(file[p])
				p++
				if ival == '\\' {
					ival = int(file[p])
					p++
					if ival == 'n' {
						ival = '\n' // '\n' is considered '\n', others directly ignore the '\' escape
					}
				}
				if tk == '"' { // if it is double quotes, it is considered as a string, copying characters to data
					(*memb)[data] = byte(ival)
					data++
				}
			}
			p++
			if tk == '"' { // double quotation marks `ival` point to the beginning of the string in data
				ival = pp
			} else { // single quotation marks are considered to be the number
				tk = Num
			}
			return
		} else if tk == '=' {
			if file[p] == '=' {
				p++
				tk = Eq
			} else {
				tk = Assign
			}
			return
		} else if tk == '+' {
			if file[p] == '+' {
				p++
				tk = Inc
			} else {
				tk = Add
			}
			return
		} else if tk == '-' {
			if file[p] == '-' {
				p++
				tk = Dec
			} else {
				tk = Sub
			}
			return
		} else if tk == '!' {
			if file[p] == '=' {
				p++
				tk = Ne
			}
			return
		} else if tk == '<' {
			if file[p] == '=' {
				p++
				tk = Le
			} else if file[p] == '<' {
				p++
				tk = Shl
			} else {
				tk = Lt
			}
			return
		} else if tk == '>' {
			if file[p] == '=' {
				p++
				tk = Ge
			} else if file[p] == '>' {
				p++
				tk = Shr
			} else {
				tk = Gt
			}
			return
		} else if tk == '|' {
			if file[p] == '|' {
				p++
				tk = Lor
			} else {
				tk = Or
			}
			return
		} else if tk == '&' {
			if file[p] == '&' {
				p++
				tk = Lan
			} else {
				tk = And
			}
			return
		} else if tk == '^' {
			tk = Xor
			return
		} else if tk == '%' {
			tk = Mod
			return
		} else if tk == '*' {
			tk = Mul
			return
		} else if tk == '[' {
			tk = Brak
			return
		} else if tk == '?' {
			tk = Cond
			return
		} else if tk == '~' || tk == ';' || tk == '{' || tk == '}' ||
			tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':' {
			return
		}
	}
}

func expr(lev int) { // expression parsing, `lev` represents an operator,
	// because each operator `token` is arranged in order of priority, so large `lev` indicates a high priority
	var (
		t int
		d []int
	)

	if tk == 0 {
		log.Panicf("%d: unexpected eof in expression\n", line)
	} else if tk == Num {
		e++
		mem[e] = IMM // directly take an immediate value as the expression value
		e++
		mem[e] = ival
		next()
		ty = INT
	} else if tk == '"' { // string
		e++
		mem[e] = IMM
		e++
		mem[e] = ival
		next()
		for tk == '"' { // continuous `"` handles C-style multiline text such as `"abc" "def"`
			next()
		}
		data = (data + sizeofint) & -sizeofint // byte alignment to int
		ty = PTR
	} else if tk == Sizeof {
		next()
		if tk == '(' {
			next()
		} else {
			log.Panicf("%d: open paren expected in sizeof\n", line)
		}
		ty = INT
		if tk == Int {
			next()
		} else if tk == Char {
			next()
			ty = CHAR
		}
		for tk == Mul { // multi-level pointers, plus `PTR` for each level
			next()
			ty += PTR
		}
		if tk == ')' {
			next()
		} else {
			log.Panicf("%d: close paren expected in sizeof\n", line)
		}
		e++
		mem[e] = IMM
		e++
		if ty == CHAR { // `char` is a byte, `int` and multi-level pointers are `int` size
			mem[e] = sizeofbyte
		} else {
			mem[e] = sizeofint
		}
		ty = INT
	} else if tk == ID { // identifier
		d = id
		next()
		if tk == '(' { // function
			next()
			t = 0           // number of parameters
			for tk != ')' { // calculate the value of the argument, push (parameter passing)
				expr(Assign)
				e++
				mem[e] = PSH
				t++
				if tk == ',' {
					next()
				}
			}
			next()
			if d[Class] == Sys { // system calls, such as `malloc`, `memset`; `d[Val]` are opcode
				e++
				mem[e] = d[Val]
			} else if d[Class] == Fun {
				e++
				mem[e] = JSR
				e++
				mem[e] = d[Val]
			} else {
				log.Panicf("%d: bad function call\n", line)
			}
			if t != 0 { // because the stack pass parameters, adjust the stack
				e++
				mem[e] = ADJ
				e++
				mem[e] = t
			}
			ty = d[Type] // type of function return value
		} else if d[Class] == Num { // enumeration, only enums have `[Class] == Num`
			e++
			mem[e] = IMM
			e++
			mem[e] = d[Val]
			ty = INT
		} else { // variable, takes the "address" first and then LC/LI
			if d[Class] == Loc {
				e++
				mem[e] = LEA // take the "address", `d[Val]` is the local variable offset
				e++
				mem[e] = loc - d[Val]
			} else if d[Class] == Glo { // `d[Val]` is a pointer to a global variable
				e++
				mem[e] = IMM
				e++
				mem[e] = d[Val]
			} else {
				log.Panicf("%d: undefined variable\n", line)
			}
			e++
			ty = d[Type]
			if ty == CHAR {
				mem[e] = LC
			} else {
				mem[e] = LI
			}
		}
	} else if tk == '(' {
		next()
		if tk == Int || tk == Char { // type cast
			if tk == Int {
				t = INT
			} else {
				t = CHAR
			}
			next()
			for tk == Mul { // pointer
				next()
				t += PTR
			}
			if tk == ')' {
				next()
			} else {
				log.Panicf("%d: bad cast\n", line)
			}
			expr(Inc) // high priority
			ty = t
		} else { // general syntax brackets
			expr(Assign)
			if tk == ')' {
				next()
			} else {
				log.Panicf("%d: close paren expected\n", line)
			}
		}
	} else if tk == Mul { // pointer dereference
		next()
		expr(Inc) // high priority
		if ty > INT {
			ty -= PTR
		} else {
			log.Panicf("%d: bad dereference\n", line)
		}
		e++
		if ty == CHAR {
			mem[e] = LC
		} else {
			mem[e] = LI
		}
	} else if tk == And { // "&", take the address operation
		next()    // according to the above code, when "token" is a variable, it takes the address first
		expr(Inc) // and then LI/LC, so `--e` becomes the address of "a"
		if mem[e] == LC || mem[e] == LI {
			e--
		} else {
			log.Panicf("%d: bad address-of\n", line)
		}
		ty += PTR
	} else if tk == '!' { // `!x` is equivalent to `x==0`
		next()
		expr(Inc)
		mem[e+1], mem[e+2], mem[e+3], mem[e+4] = PSH, IMM, 0, EQ
		e += 4
		ty = INT
	} else if tk == '~' { // `~x` is equivalent to `x ^ -1`
		next()
		expr(Inc)
		mem[e+1], mem[e+2], mem[e+3], mem[e+4] = PSH, IMM, -1, XOR
		e += 4
		ty = INT
	} else if tk == Add {
		next()
		expr(Inc)
		ty = INT
	} else if tk == Sub {
		next()
		e++
		mem[e] = IMM
		if tk == Num {
			e++
			mem[e] = -ival //value, taking negative
			next()
		} else {
			e++
			mem[e] = -1
			e++
			mem[e] = PSH
			expr(Inc)
			e++
			mem[e] = MUL // multiplication by -1
		}
		ty = INT
	} else if tk == Inc || tk == Dec { // processing `++x` and `--x`; `x--` and `x++` is handled later
		t = tk
		next()
		expr(Inc)
		if mem[e] == LC { // push address to stack (used by SC/SI below), then take the number
			mem[e] = PSH
			e++
			mem[e] = LC
		} else if mem[e] == LI {
			mem[e] = PSH
			e++
			mem[e] = LI
		} else {
			log.Panicf("%d: bad lvalue in pre-increment\n", line)
		}
		mem[e+1], mem[e+2] = PSH, IMM // pushing the value
		if ty > PTR {                 // the pointer is added or subtracted, otherwise it is added or subtracted by 1
			mem[e+3] = sizeofint
		} else {
			mem[e+3] = sizeofbyte
		}
		if t == Inc { // operation
			mem[e+4] = ADD
		} else {
			mem[e+4] = SUB
		}
		if ty == CHAR { // save the variable
			mem[e+5] = SC
		} else {
			mem[e+5] = SI
		}
		e += 5
	} else {
		log.Panicf("%d: bad expression\n", line)
	}

	for tk >= lev { // "precedence climbing" or "Top Down Operator Precedence" method
		t = ty            // `tk` is ASCII code will not exceed `Num=128`; `ty` may change during recursion,
		if tk == Assign { // so back up the currently processed expression type
			next()
			if mem[e] == LC || mem[e] == LI {
				mem[e] = PSH // the left part is processed by the variable part of `tk=ID` and pushes the address
			} else {
				log.Panicf("%d: bad lvalue in assignment\n", line)
			}
			expr(Assign) // getting the value of the right part `expr` as the result of `a=expr`
			ty = t
			e++
			if ty == CHAR {
				mem[e] = SC
			} else {
				mem[e] = SI
			}
		} else if tk == Cond { // `x?a:b` is similar to `if`, except that it cannot be without `else`
			next()
			e++
			mem[e] = BZ
			e++
			d = mem[e:]
			expr(Assign)
			if tk == ':' {
				next()
			} else {
				log.Panicf("%d: conditional missing colon\n", line)
			}
			d[0] = e + 3
			e++
			mem[e] = JMP
			e++
			d = mem[e:]
			expr(Cond)
			d[0] = e + 1
		} else if tk == Lor { // short circuit, the logical `or` operator on the left is `true`, expression is `true`,
			next() // do not evaluate the value of the right side of the operator
			e++
			mem[e] = BNZ
			e++
			d = mem[e:]
			expr(Lan)    // `lev` in `expr` indicates that the most associativity in
			d[0] = e + 1 // the recursive function must not be lower than which operator
			ty = INT
		} else if tk == Lan { // short circuit, logic `and`, same as above
			next()
			e++
			mem[e] = BZ
			e++
			d = mem[e:]
			expr(Or)
			d[0] = e + 1
			ty = INT
		} else if tk == Or { // push the current value, calculate the right value of the operator
			next() // and do the operation with the current value (in the stack)
			e++
			mem[e] = PSH
			expr(Xor)
			e++
			mem[e] = OR
			ty = INT
		} else if tk == Xor {
			next()
			e++
			mem[e] = PSH
			expr(And)
			e++
			mem[e] = XOR
			ty = INT
		} else if tk == And {
			next()
			e++
			mem[e] = PSH
			expr(Eq)
			e++
			mem[e] = AND
			ty = INT
		} else if tk == Eq {
			next()
			e++
			mem[e] = PSH
			expr(Lt)
			e++
			mem[e] = EQ
			ty = INT
		} else if tk == Ne {
			next()
			e++
			mem[e] = PSH
			expr(Lt)
			e++
			mem[e] = NE
			ty = INT
		} else if tk == Lt {
			next()
			e++
			mem[e] = PSH
			expr(Shl)
			e++
			mem[e] = LT
			ty = INT
		} else if tk == Gt {
			next()
			e++
			mem[e] = PSH
			expr(Shl)
			e++
			mem[e] = GT
			ty = INT
		} else if tk == Le {
			next()
			e++
			mem[e] = PSH
			expr(Shl)
			e++
			mem[e] = LE
			ty = INT
		} else if tk == Ge {
			next()
			e++
			mem[e] = PSH
			expr(Shl)
			e++
			mem[e] = GE
			ty = INT
		} else if tk == Shl {
			next()
			e++
			mem[e] = PSH
			expr(Add)
			e++
			mem[e] = SHL
			ty = INT
		} else if tk == Shr {
			next()
			e++
			mem[e] = PSH
			expr(Add)
			e++
			mem[e] = SHR
			ty = INT
		} else if tk == Add {
			next()
			e++
			mem[e] = PSH
			expr(Mul)
			ty = t
			if ty > PTR {
				mem[e+1], mem[e+2], mem[e+3], mem[e+4] = PSH, IMM, sizeofint, MUL // processing pointer
				e += 4
			}
			e++
			mem[e] = ADD
		} else if tk == Sub {
			next()
			e++
			mem[e] = PSH
			expr(Mul)
			if t > PTR && t == ty { // pointer subtraction
				mem[e+1], mem[e+2], mem[e+3], mem[e+4], mem[e+5] = SUB, PSH, IMM, sizeofint, DIV
				e += 5
				ty = INT
			} else if ty = t; ty > PTR { // pointer decrement value
				mem[e+1], mem[e+2], mem[e+3], mem[e+4], mem[e+5] = PSH, IMM, sizeofint, MUL, SUB
				e += 5
			} else {
				e++
				mem[e] = SUB
			}
		} else if tk == Mul {
			next()
			e++
			mem[e] = PSH
			expr(Inc)
			e++
			mem[e] = MUL
			ty = INT
		} else if tk == Div {
			next()
			e++
			mem[e] = PSH
			expr(Inc)
			e++
			mem[e] = DIV
			ty = INT
		} else if tk == Mod {
			next()
			e++
			mem[e] = PSH
			expr(Inc)
			e++
			mem[e] = MOD
			ty = INT
		} else if tk == Inc || tk == Dec { // processing `x++`, `x--`
			if mem[e] == LC {
				mem[e] = PSH
				e++
				mem[e] = LC
			} else if mem[e] == LI {
				mem[e] = PSH
				e++
				mem[e] = LI
			} else {
				log.Panicf("%d: bad lvalue in post-increment\n", line)
			}
			mem[e+1], mem[e+2] = PSH, IMM
			if ty > PTR {
				mem[e+3] = sizeofint
			} else {
				mem[e+3] = sizeofbyte
			}
			if tk == Inc { // first increment / decrement
				mem[e+4] = ADD
			} else {
				mem[e+4] = SUB
			}
			if ty == CHAR { // saving to memory
				mem[e+5] = SC
			} else {
				mem[e+5] = SI
			}
			mem[e+6], mem[e+7] = PSH, IMM
			if ty > PTR {
				mem[e+8] = sizeofint
			} else {
				mem[e+8] = sizeofbyte
			}
			if tk == Inc { // the opposite operation, to ensure that the post-increment/decrement
				mem[e+9] = SUB // does not affect the evaluation of this expression
			} else {
				mem[e+9] = ADD
			}
			e += 9
			next() // PS: I finally know which `a=1;b=a+++a++` is equal to 3
		} else if tk == Brak { // array index
			next()
			e++
			mem[e] = PSH // saving array pointer, calculating index
			expr(Assign)
			if tk == ']' {
				next()
			} else {
				log.Panicf("%d: close bracket expected\n", line)
			}
			if t > PTR {
				mem[e+1], mem[e+2], mem[e+3], mem[e+4] = PSH, IMM, sizeofint, MUL
				e += 4
			} else if t < PTR {
				log.Panicf("%d: pointer type expected\n", line)
			}
			e++
			mem[e] = ADD
			e++
			ty = t - PTR
			if ty == CHAR {
				mem[e] = LC
			} else {
				mem[e] = LI
			}
		} else {
			log.Panicf("%d: compiler error tk=%d\n", line, tk)
		}
	}
}

func stmt() { // statement parsing (syntax analysis, except for declarations)
	var (
		a, b int
	)

	if tk == If {
		next()
		if tk == '(' {
			next()
		} else {
			log.Panicf("%d: open paren expected\n", line)
		}
		expr(Assign)
		if tk == ')' {
			next()
		} else {
			log.Panicf("%d: close paren expected\n", line)
		}
		e++
		mem[e] = BZ // branch if zero
		e++
		b = e  // branch address pointer
		stmt() // parse body of `if`
		if tk == Else {
			mem[b] = e + 3 // `e + 3` points to `else` starting position
			e++
			mem[e] = JMP // `JMP` to target
			e++
			b = e
			next()
			stmt() // parse body of `else`
		}
		mem[b] = e + 1
	} else if tk == While {
		next()
		a = e + 1 // start address of `while` body
		if tk == '(' {
			next()
		} else {
			log.Panicf("%d: open paren expected\n", line)
		}
		expr(Assign)
		if tk == ')' {
			next()
		} else {
			log.Panicf("%d: close paren expected\n", line)
		}
		e++
		mem[e] = BZ
		e++
		b = e  // b = address after the end of `while` statement
		stmt() // parse body of `while`
		e++
		mem[e] = JMP // unconditional jump to the start of `while` statement
		e++          // (including the code for the loop condition), to implement the loop
		mem[e] = a
		mem[b] = e + 1 // `BZ` jump target (end of cycle)
	} else if tk == Return {
		next()
		if tk != ';' {
			expr(Assign) // calculate the return value
		}
		e++
		mem[e] = LEV
		if tk == ';' {
			next()
		} else {
			log.Panicf("%d: semicolon expected\n", line)
		}
	} else if tk == '{' {
		next()
		for tk != '}' {
			stmt()
		}
		next()
	} else if tk == ';' {
		next()
	} else {
		expr(Assign) // general statements are considered assignment statements/expressions
		if tk == ';' {
			next()
		} else {
			log.Panicf("%d: semicolon expected\n", line)
		}
	}
}

func main() {
	var (
		argc           = len(os.Args)
		argv           = copyargv() // copy `os.Args` to mem and get "pointer"
		fd             *os.File
		bt, ty, poolsz int
		idmain         []int
	)
	var (
		pc, sp, bp, a, cycle int // vm registers
		i, t                 int // temps
	)

	argc--
	argv++
	if argc > 0 && (*memb)[mem[argv]] == '-' && (*memb)[mem[argv]+1] == 's' {
		src = true
		argc--
		argv++
	}
	if argc > 0 && (*memb)[mem[argv]] == '-' && (*memb)[mem[argv]+1] == 'd' {
		debug = true
		argc--
		argv++
	}
	if argc < 1 {
		fmt.Fprint(out, "usage: nanocc [-s] [-d] file ...\n")
		return
	}

	if fd, _ = os.Open(os.Args[len(os.Args)-argc]); fd == nil {
		log.Panicf("could not open(%s)\n", os.Args[len(os.Args)-argc])
	}

	poolsz = 256 * 1024 // arbitrary size
	if sym = make([]int, poolsz/sizeofint); sym == nil {
		log.Panicf("could not malloc(%d) symbol area\n", poolsz)
	}
	if e, *memb = len(*memb)/sizeofint, append(*memb, make([]byte, poolsz)...); *memb == nil {
		log.Panicf("could not malloc(%d) text area\n", poolsz)
	}
	le = e
	if data, *memb = len(*memb), append(*memb, make([]byte, poolsz)...); *memb == nil {
		log.Panicf("could not malloc(%d) text area\n", poolsz)
	}
	if sp, *memb = len(*memb)/sizeofint, append(*memb, make([]byte, poolsz)...); *memb == nil {
		log.Panicf("could not malloc(%d) stack area\n", poolsz)
	}
	p, file = len(file), append(file, []byte("char else enum if int return sizeof while "+
		"open read close printf malloc free memset memcmp exit void main\x00")...)
	i = Char
	for i <= While { // add keywords to symbol table
		next()
		id[Tk] = i
		i++
	}
	i = OPEN
	for i <= EXIT { // add library to symbol table
		next()
		id[Class] = Sys
		id[Type] = INT
		id[Val] = i
		i++
	}
	next()
	id[Tk] = Char // handle void type
	next()
	idmain = id // keep track of main

	if p, file = len(file), append(file, make([]byte, poolsz)...); file == nil {
		log.Panicf("could not malloc(%d) source area\n", poolsz)
	}
	lp = p
	if n, err := fd.Read(file[p:]); err != nil {
		log.Panicf("read() returned %v\n", err)
	} else {
		i = p + n
	}
	file[i] = 0
	fd.Close()

	// parse declarations
	line = 1
	next()
	for tk != 0 {
		bt = INT // basetype
		if tk == Int {
			next()
		} else if tk == Char { // char variable
			next()
			bt = CHAR
		} else if tk == Enum {
			next()
			if tk != '{' { // seems to ignore the enumeration name, for example `enum xxx{}`
				next()
			}
			if tk == '{' {
				next()
				i = 0 // enum starts from 0 by default
				for tk != '}' {
					if tk != ID {
						log.Panicf("%d: bad enum identifier %d\n", line, tk)
					}
					next()
					if tk == Assign { // detected assignment operation such as `enum {Num = 128};`
						next()
						if tk != Num {
							log.Panicf("%d: bad enum initializer\n", line)
						}
						i = ival
						next()
					}
					id[Class] = Num // "id" has been processed by "next" function
					id[Type] = INT
					id[Val] = i
					i++
					if tk == ',' {
						next()
					}
				}
				next()
			}
		}
		for tk != ';' && tk != '}' { // `enum` finishes by `tk == ';'`, so the code below will be skipped
			ty = bt
			for tk == Mul { // `tk == Mul` means that the beginning of * is a pointer type,
				next() // and the type plus `PTR` indicates what kind of pointer
				ty += PTR
			}
			if tk != ID {
				log.Panicf("%d: bad global declaration\n", line)
			}
			if id[Class] != 0 {
				log.Panicf("%d: duplicate global definition\n", line)
			}
			next()
			id[Type] = ty  // type of assignment
			if tk == '(' { // function
				id[Class] = Fun // type is functional
				id[Val] = e + 1 // function Pointer? offset/address in bytecode
				next()
				i = 0
				for tk != ')' { // parameters
					ty = INT
					if tk == Int {
						next()
					} else if tk == Char {
						next()
						ty = CHAR
					}
					for tk == Mul {
						next()
						ty += PTR
					}
					if tk != ID {
						log.Panicf("%d: bad parameter declaration\n", line)
					}
					if id[Class] == Loc { // function arguments are local variables
						log.Panicf("%d: duplicate parameter definition\n", line)
					}
					id[HClass] = id[Class] // backup symbol information, to enter the function context
					id[Class] = Loc
					id[HType] = id[Type]
					id[Type] = ty
					id[HVal] = id[Val]
					id[Val] = i
					i++ // local variable numbering
					next()
					if tk == ',' {
						next()
					}
				}
				next()
				if tk != '{' {
					log.Panicf("%d: bad function definition\n", line)
				}
				i++
				loc = i // local variable offset
				next()
				for tk == Int || tk == Char { // parse in-function declaration
					if tk == Int {
						bt = INT
					} else {
						bt = CHAR
					}
					next()
					for tk != ';' {
						ty = bt
						for tk == Mul {
							next()
							ty += PTR
						}
						if tk != ID {
							log.Panicf("%d: bad local declaration\n", line)
						}
						if id[Class] == Loc {
							log.Panicf("%d: duplicate local definition\n", line)
						}
						id[HClass] = id[Class]
						id[Class] = Loc
						id[HType] = id[Type]
						id[Type] = ty
						id[HVal] = id[Val]
						i++
						id[Val] = i // storing variable offset
						next()
						if tk == ',' {
							next()
						}
					}
					next()
				}
				e++
				mem[e] = ENT
				e++
				mem[e] = i - loc // number of function local variables
				for tk != '}' {
					stmt() // function body parsing
				}
				e++
				mem[e] = LEV      // function return
				id = sym          // unwind symbol table locals
				for id[Tk] != 0 { // recovering symbol information
					if id[Class] == Loc {
						id[Class] = id[HClass]
						id[Type] = id[HType]
						id[Val] = id[HVal]
					}
					id = id[Idsz:]
				}
			} else {
				id[Class] = Glo // global variables
				id[Val] = data  // assign memory to global variables in the `data` section
				data += sizeofint
			}
			if tk == ',' {
				next()
			}
		}
		next()
	}

	if pc = idmain[Val]; pc == 0 {
		log.Panic("main() not defined\n")
	}
	if src {
		return
	}

	// setup stack
	sp = sp + poolsz/sizeofint
	bp = sp
	sp--
	mem[sp] = EXIT // call exit if main returns
	sp--
	mem[sp] = PSH
	t = sp
	sp--
	mem[sp] = argc
	sp--
	mem[sp] = argv * sizeofint
	sp--
	mem[sp] = t

	// run...
	cycle = 0
	for true {
		i = mem[pc]
		pc++
		cycle++
		if debug {
			fmt.Fprintf(out, "%d> %.4s", cycle, ("LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ," +
				"PSH ,OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ," +
				"OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,")[i*5:])
			if i <= ADJ {
				fmt.Fprintf(out, " %d\n", mem[pc])
			} else {
				fmt.Fprint(out, "\n")
			}
		}
		if i == LEA {
			a = (bp + mem[pc]) * sizeofint // we should always store "address" in bytes like global vars, strings, args
			pc++
		} else if i == IMM {
			a = mem[pc]
			pc++
		} else if i == JMP {
			pc = mem[pc]
		} else if i == JSR {
			sp--
			mem[sp] = pc + 1
			pc = mem[pc]
		} else if i == BZ {
			if a != 0 {
				pc = pc + 1
			} else {
				pc = mem[pc]
			}
		} else if i == BNZ {
			if a != 0 {
				pc = mem[pc]
			} else {
				pc = pc + 1
			}
		} else if i == ENT {
			sp--
			mem[sp] = bp
			bp = sp
			sp = sp - mem[pc]
			pc++
		} else if i == ADJ {
			sp = sp + mem[pc]
			pc++
		} else if i == LEV {
			sp = bp
			bp = mem[sp]
			sp++
			pc = mem[sp]
			sp++
		} else if i == LI {
			a = mem[a/sizeofint] // we should convert byte "pointer" to int "pointer"
		} else if i == LC {
			a = int((*memb)[a])
		} else if i == SI {
			mem[mem[sp]/sizeofint] = a
			sp++
		} else if i == SC {
			(*memb)[mem[sp]] = byte(a) // store `a` in a byte at `mem[sp]` "address"
			a = int((*memb)[mem[sp]])  // and load truncated int to acc
			sp++
		} else if i == PSH {
			sp--
			mem[sp] = a
		} else if i == OR {
			a = mem[sp] | a
			sp++
		} else if i == XOR {
			a = mem[sp] ^ a
			sp++
		} else if i == AND {
			a = mem[sp] & a
			sp++
		} else if i == EQ {
			if mem[sp] == a {
				a = 1
			} else {
				a = 0
			}
			sp++
		} else if i == NE {
			if mem[sp] != a {
				a = 1
			} else {
				a = 0
			}
			sp++
		} else if i == LT {
			if mem[sp] < a {
				a = 1
			} else {
				a = 0
			}
			sp++
		} else if i == GT {
			if mem[sp] > a {
				a = 1
			} else {
				a = 0
			}
			sp++
		} else if i == LE {
			if mem[sp] <= a {
				a = 1
			} else {
				a = 0
			}
			sp++
		} else if i == GE {
			if mem[sp] >= a {
				a = 1
			} else {
				a = 0
			}
			sp++
		} else if i == SHL {
			a = mem[sp] << uint(a)
			sp++
		} else if i == SHR {
			a = mem[sp] >> uint(a)
			sp++
		} else if i == ADD {
			a = mem[sp] + a
			sp++
		} else if i == SUB {
			a = mem[sp] - a
			sp++
		} else if i == MUL {
			a = mem[sp] * a
			sp++
		} else if i == DIV {
			a = mem[sp] / a
			sp++
		} else if i == MOD {
			a = mem[sp] % a
			sp++
		} else if i == OPEN { // instead returning the file descriptor we return "pointer" to path
			path := string((*memb)[mem[sp+1] : mem[sp+1]+bytes.IndexByte((*memb)[mem[sp+1]:], 0)])
			if _, err := os.Open(path); err != nil {
				a = -1
			} else {
				a = mem[sp+1]
			}
		} else if i == READ {
			path := string((*memb)[mem[sp+2] : mem[sp+2]+bytes.IndexByte((*memb)[mem[sp+2]:], 0)])
			f, _ := os.Open(path)
			a, _ = f.Read((*memb)[mem[sp+1] : mem[sp+1]+mem[sp]])
			f.Close()
		} else if i == CLOS {
			// we don't use global `os.File` or file descriptors
		} else if i == PRTF {
			t = sp + mem[pc+1]
			a = printf(memb, mem[t-1], mem[t-2], mem[t-3], mem[t-4], mem[t-5], mem[t-6])
		} else if i == MALC {
			a, *memb = len(*memb), append(*memb, make([]byte, mem[sp])...)
		} else if i == FREE {
			// garbage collector will do the work for us
		} else if i == MSET {
			a = mem[sp+2]
		} else if i == MCMP {
			a = bytes.Compare((*memb)[mem[sp+2]:mem[sp+2]+mem[sp]], (*memb)[mem[sp+1]:mem[sp+1]+mem[sp]])
		} else if i == EXIT {
			fmt.Fprintf(out, "exit(%d) cycle = %d\n", mem[sp], cycle)
			return
		} else {
			log.Panicf("unknown instruction = %d! cycle = %d\n", i, cycle)
		}
	}
}

// C utils

// copyargv copies `os.Args` and their pointers to the start of `mem` block
func copyargv() (index int) {
	index = len(*memb) / sizeofint                                        // set `index` to next position
	*memb = append(*memb, make([]byte, (len(os.Args)+1)*sizeofint)...)    // add bytes for storing arg pointers + int(0)
	argptr := len(*memb)                                                  // set arg pointer to first arg position
	argsize := (len(strings.Join(os.Args, " ")) + sizeofint) & -sizeofint // calculate aligned size for args
	*memb = append(*memb, make([]byte, argsize)...)                       // add bytes for storing args, with alignment
	for i, arg := range os.Args {
		mem[index+i] = argptr                        // store arg pointer
		argptr += copy((*memb)[argptr:], arg+"\x00") // store arg + zero byte
	}
	return
}

func printf(mem *[]byte, f int, a ...int) int {
	var (
		reg    = regexp.MustCompile(`(?:[^%]%|^%)(\d+|\*)?(?:\.(\d+|\*)?)?([sd])`)
		fstr   = string((*mem)[f : f+bytes.IndexByte((*mem)[f:], 0)])
		ai     []interface{}
		widths int
	)
	for i, subm := range reg.FindAllStringSubmatch(fstr, -1) {
		if subm[1] == "*" {
			ai = append(ai, a[i+widths])
			widths++
		}
		if subm[2] == "*" {
			ai = append(ai, a[i+widths])
			widths++
		}
		if subm[3] == "d" {
			ai = append(ai, a[i+widths])
		} else { // == "s"
			ptr := a[i+widths]
			ai = append(ai, string((*mem)[ptr:ptr+bytes.IndexByte((*mem)[ptr:], 0)]))
		}
	}
	result := fmt.Sprintf(fstr, ai...)
	fmt.Fprint(out, result)
	return len(result)
}
