// ========================================================
// L4 normal eval
import { Sexp } from "s-expression";
import {contains, filter, includes, map, range, reduce, zip, zipWith} from "ramda";
import {
    CExp,
    Exp,
    IfExp,
    Program,
    parseL4Exp,
    isLetExp,
    LetExp,
    Binding,
    PrimOp,
    VarDecl,
    makeProcExp,
    makeAppExp, makeLetExp, makeBinding
} from "./L4-ast";
import { isAppExp, isBoolExp, isCExp, isDefineExp, isIfExp, isLitExp, isNumExp,
          isPrimOp, isProcExp, isStrExp, isVarRef } from "./L4-ast";
import {
    applyNormalEnv,
    makeEmptyNormalEnv,
    NormalEnv,
    makeExtNormalEnv,
    isEmptyNormalEnv,
    ExtNormalEnv, isExtNormalEnv
} from './L4-env-normal';
import { applyPrimitive } from "./evalPrimitive";
import {Value, makeNormalClosure, isNormalClosure, NormalClosure} from "./L4-value";
import { first, rest, isEmpty } from '../shared/list';
import {Result, makeOk, makeFailure, bind, mapResult, safe2} from "../shared/result";
import { parse as p } from "../shared/parser";

export const normalEval = (exp: CExp, env: NormalEnv): Result<Value> =>
    isBoolExp(exp) ? makeOk(exp.val) :
    isNumExp(exp) ? makeOk(exp.val) :
    isStrExp(exp) ? makeOk(exp.val) :
    isPrimOp(exp) ? makeOk(exp) :
    isLitExp(exp) ? makeOk(exp.val) :
    isVarRef(exp) ? bind(applyNormalEnv(env, exp.var), (e: CExp) => normalEval(e, env)) :
    isIfExp(exp) ? evalIf(exp, env) :
    isProcExp(exp) ? makeOk(makeNormalClosure(exp.args, exp.body, env)) :
    isLetExp(exp) ? evalLet(exp, env) :
//    isAppExp(exp) ? bind(normalEval(exp.rator, env), proc => normalApplyProc(proc, exp.rands, env)) :
        isAppExp(exp) ? safe2((args: CExp[], proc: Value) => normalApplyProc(proc, args, env))
            (mapResult((rand: CExp) => makeOk(makeLetExp(envToBindings(env), [rand])), exp.rands),
                normalEval(exp.rator, env)) : // TODO Refactor: makeOk and compute envToBindings only once

    makeFailure(`Bad ast: ${exp}`);

export const isTrueValue = (x: Value): boolean =>
    ! (x === false);

const evalIf = (exp: IfExp, env: NormalEnv): Result<Value> =>
    bind(normalEval(exp.test, env),
         (test: Value) => isTrueValue(test) ? normalEval(exp.then, env) : normalEval(exp.alt, env));


const envToBindings = (env: NormalEnv) : Binding[] => {
    const addNewBindings = (currentBindings: Binding[], newEnv: ExtNormalEnv): Binding[] => {
        const currentVars = map((b:Binding) => b.var.var, currentBindings);
        const pairs = zip(newEnv.vars, newEnv.vals);
        const newEntries = filter((pair) => !currentVars.includes(pair[0]) , pairs);
        return currentBindings.concat(map((pair) => makeBinding(pair[0], pair[1]), newEntries));
    };

    const envToBindingsRecursion = (env: NormalEnv, currentBindings: Binding[]): Binding[] =>
        isEmptyNormalEnv(env) ? currentBindings :
            envToBindingsRecursion(env.nextEnv, addNewBindings(currentBindings, env));

    return envToBindingsRecursion(env, []);
};

const normalApplyProc = (proc: Value, args: CExp[], env: NormalEnv): Result<Value> =>
    isPrimOp(proc) ? normalApplyPrimOp(proc, args, env) :
    isNormalClosure(proc) ? applyNormalClosure(proc, args) :
    makeFailure(`Bad proc applied ${proc}`);

const applyNormalClosure = (proc: NormalClosure, args: CExp[]): Result<Value> => {
    const vars = map((v: VarDecl) => v.var, proc.params);
    return evalExps(proc.body, makeExtNormalEnv(vars, args, proc.env));
};

const normalApplyPrimOp = (proc: PrimOp, args: CExp[], env: NormalEnv): Result<Value> => {
    const argVals: Result<Value[]> = mapResult((arg) => normalEval(arg, env), args);
    return bind(argVals, (args: Value[]) => applyPrimitive(proc, args));
};

// Evaluate a sequence of expressions (in a program)
export const evalExps = (exps: Exp[], env: NormalEnv): Result<Value> =>
    isEmpty(exps) ? makeFailure("Empty sequence") :
    isDefineExp(first(exps)) ? evalDefineExps(first(exps), rest(exps), env) :
    evalCExps(first(exps), rest(exps), env);

const evalCExps = (first: Exp, rest: Exp[], env: NormalEnv): Result<Value> =>
    isCExp(first) && isEmpty(rest) ? normalEval(first, env) :
    isCExp(first) ? bind(normalEval(first, env), _ => evalExps(rest, env)) :
    makeFailure("Never");

const evalDefineExps = (def: Exp, exps: Exp[], env: NormalEnv): Result<Value> =>
    isDefineExp(def) ? evalExps(exps, makeExtNormalEnv([def.var.var], [def.val], env)) :
    makeFailure("Unexpected " + def);

export const evalNormalProgram = (program: Program): Result<Value> =>
    evalExps(program.exps, makeEmptyNormalEnv());

export const evalNormalParse = (s: string): Result<Value> =>
    bind(p(s),
        (parsed: Sexp) => bind(parseL4Exp(parsed),
            (exp: Exp) => evalExps([exp], makeEmptyNormalEnv())));

const evalLet = (exp: LetExp, env: NormalEnv): Result<Value> => {
    const vals = map((b: Binding) => b.val, exp.bindings);
    const vars = map((b: Binding) => b.var.var, exp.bindings);
    return evalExps(exp.body, makeExtNormalEnv(vars, vals, env));
};