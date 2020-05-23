import { expect } from 'chai';
import { L4toMermaid } from "./mermaid"
import { makeOk, isOk, Result, bind } from "../shared/result";

describe('Mermaid', () => {
    it('convert simple expressions', () => {
        expect(L4toMermaid("(L4 \"hello\")")).to.deep.equal(makeOk(`graph TD\n\tProgram_1["Program"] -->|exps| Exps_1[":"]\n\tExps_1 --> StrExp_1["StrExp(hello)"]`));
    });
    it('convert simple compound expressions', () => {
        expect(L4toMermaid("(define my-list '(1 2))")).to.deep.equal(makeOk(`graph TD\n\tDefineExp_1["DefineExp"] -->|var| Var_1["VarDecl(my-list)"]\n\tDefineExp_1 -->|val| LitExp_1["LitExp"]\n\tLitExp_1 -->|val| CompoundSexp_1["CompoundSexp"]\n\tCompoundSexp_1 -->|val1| Number_1["number(1)"]\n\tCompoundSexp_1 -->|val2| CompoundSexp_2["CompoundSexp"]\n\tCompoundSexp_2 -->|val1| Number_2["number(2)"]\n\tCompoundSexp_2 -->|val2| EmptySExp_1["EmptySExp"]`));
    });
    it('convert simple compound expressions 2', () => {
        expect(L4toMermaid("(+ 1 2)")).to.deep.equal(makeOk(`graph TD\n\tAppExp_1["AppExp"] -->|rator| PrimOp_1["PrimOp(+)"]\n\tAppExp_1 -->|rands| Rands_1[":"]\n\tRands_1 --> NumExp_1["NumExp(1)"]\n\tRands_1 --> NumExp_2["NumExp(2)"]`));
    });
    //it('convert complex compound expressions 2', () => {
    //    expect(L4toMermaid("(lambda (x y) ((lambda (x) (+ x y)) (+ x x)) 1)")).to.deep.equal(makeOk(`graph TD\n\tProcExp_1["ProcExp"] -->|args| Args_1[":"]\n\tProcExp_1 -->|body| Body_1[":"]\n\tArgs_1 --> VarDecl_1["VarDecl(x)"]\n\tArgs_1 --> VarDecl_2["VarDecl(y)"]\n\tBody_1 --> AppExp_1["AppExp"]\n\tBody_1 --> NumExp_1["NumExp(1)"]\n\tAppExp_1 -->|rator| ProcExp_2["ProcExp"]\n\tAppExp_1 -->|rands| Rands_1[:]\n\tProcExp_2 -->|args| Args_2[:]\n\tArgs_2 --> VarDecl_3["VarDecl(x)"]\n\tProcExp_2 -->|body| Body_2[":"]\n\tBody_2 --> AppExp_2["AppExp"]\n\tAppExp_2 -->|rator| PrimOp_1["PrimOp(+)"]`));
    //});
});
