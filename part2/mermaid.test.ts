import { expect } from 'chai';
import { L4toMermaid, mapL4toMermaid, unparseMermaid } from "./mermaid"
import {makeProgram} from "./L4-ast"
import { makeOk, isOk, Result, bind } from "../shared/result";

describe('Mermaid', () => {
    it('convert simple expressions', () => {
        expect(L4toMermaid("(L4 \"hello\")")).to.deep.equal(makeOk(`graph TD\n\tProgram_1["Program"] -->|exps| Exps_1[":"]\n\tExps_1 --> StrExp_1["StrExp(hello)"]`));
    });
    it('convert simple compound expressions', () => {
        expect(L4toMermaid("(define my-list '(1 2))")).to.deep.equal(makeOk(`graph TD\n\tDefineExp_1["DefineExp"] -->|var| VarDecl_1["VarDecl(my-list)"]\n\tDefineExp_1 -->|val| LitExp_1["LitExp"]\n\tLitExp_1 -->|val| CompoundSexp_1["CompoundSexp"]\n\tCompoundSexp_1 -->|val1| Number_1["number(1)"]\n\tCompoundSexp_1 -->|val2| CompoundSexp_2["CompoundSexp"]\n\tCompoundSexp_2 -->|val1| Number_2["number(2)"]\n\tCompoundSexp_2 -->|val2| EmptySExp_1["EmptySExp"]`));
    });
    it('convert simple compound expressions 2', () => {
        expect(L4toMermaid("(+ 1 2)")).to.deep.equal(makeOk(`graph TD\n\tAppExp_1["AppExp"] -->|rator| PrimOp_1["PrimOp(+)"]\n\tAppExp_1 -->|rands| Rands_1[":"]\n\tRands_1 --> NumExp_1["NumExp(1)"]\n\tRands_1 --> NumExp_2["NumExp(2)"]`));
    });
    //it('convert complex compound expressions 2', () => {
    //    expect(L4toMermaid("(lambda (x y) ((lambda (x) (+ x y)) (+ x x)) 1)")).to.deep.equal(makeOk(`graph TD\n\tProcExp_1["ProcExp"] -->|args| Args_1[":"]\n\tProcExp_1 -->|body| Body_1[":"]\n\tArgs_1 --> VarDecl_1["VarDecl(x)"]\n\tArgs_1 --> VarDecl_2["VarDecl(y)"]\n\tBody_1 --> AppExp_1["AppExp"]\n\tBody_1 --> NumExp_1["NumExp(1)"]\n\tAppExp_1 -->|rator| ProcExp_2["ProcExp"]\n\tAppExp_1 -->|rands| Rands_1[":"]\n\tProcExp_2 -->|args| Args_2[":"]\n\tArgs_2 --> VarDecl_3["VarDecl(x)"]\n\tProcExp_2 -->|body| Body_2[":"]\n\tBody_2 --> AppExp_2["AppExp"]\n\tAppExp_2 -->|rator| PrimOp_1["PrimOp(+)"]\n\tAppExp_2 -->|rands| Rands_2[":"]\n\tRands_2 --> VarRef_1["VarRef(x)"]\n\tRands_2 --> VarRef_2["VarRef(y)"]\n\tRands_1 --> AppExp_3["AppExp"]\n\tAppExp_3 -->|rator| PrimOp_2["PrimOp(+)"]\n\tAppExp_3 -->|rands| Rands_3[":"]\n\tRands_3 --> VarRef_3["VarRef(x)"]\n\tRands_3 --> VarRef_4["VarRef(x)"]`));
    //});
    it('convert empty program body', () => {
        expect(bind(mapL4toMermaid(makeProgram([])), unparseMermaid)).to.deep.equal(makeOk(`graph TD\n\tProgram_1["Program"] -->|exps| Exps_1[":"]`));
    });
    it('convert empty program body', () => {
        expect(bind(mapL4toMermaid(makeProgram([])), unparseMermaid)).to.deep.equal(makeOk(`graph TD\n\tProgram_1["Program"] -->|exps| Exps_1[":"]`));
    });
});

////////////////////////////////////////////////////
//  TODO: DELETE!
//////////////////////////////////////////////////// 

//let x = L4toMermaid("(lambda (x y)((lambda (x) (+ x y))(+ x x))1)")
//let x = L4toMermaid("(if #t (+ 1 2) (+ 4 7))")
//let x = L4toMermaid("(let ((a 1) (b 2)) (+ a b))")
//let x = L4toMermaid("(set! a 4)")
//let x = L4toMermaid("(letrec ((a 1) (b 2)) (+ a b))")
//let x = L4toMermaid("(L4 (define my-list '(1 2)))")
//let x = L4toMermaid("(L4 (+ 2 5))")
//let x = L4toMermaid("(L4 (+ (/ 18 2) (* 7 17)))")
//let x = L4toMermaid("(define my-list '(1 2))")
//let x = L4toMermaid("(L4 1 #t “hello”)")
//let x = L4toMermaid("(L4 \"hello\")")
//let x = L4toMermaid(`(L4 
//(define makeAdder (lambda (n) (lambda (y) (+ y n))))
//(define a6 (makeAdder 6))
//(define a7 (makeAdder 7))
//(+ (a6 1) (a7 1)))`)
//let x = L4toMermaid(`(L4 
//(define f
//    (lambda (x)
//      (if (= x 0)
//          1
//          (* x (f (- x 1))))))
//  (f 5))`);
//let x = L4toMermaid("(set! x (+ 1 2))");
//let x = L4toMermaid(`(letrec ((a 1) (b #t)) (if b a (+ a 1)))`);
//let x = L4toMermaid(`(L4 (define my-list '(#f #t "hello" 2 + (quote x))))`);
//let x = parseL4(`(L4 (define my-list (quote x)))`);
//let x = L4toMermaid(`(L4 (define my-list '(1 77)) (boolean? (cdr my-list)))`);
//isOk(x) ? console.log(x.value) : console.log(x.message)