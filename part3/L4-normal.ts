// ========================================================
// L4 normal eval
import { Sexp } from "s-expression";
import {filter, map, zip} from "ramda";
import {
    CExp, Exp, IfExp, Program, parseL4Exp, isLetExp, LetExp, Binding, PrimOp,
    VarDecl, makeLetExp, makeBinding, isAppExp, isBoolExp, isCExp, isDefineExp,
    isIfExp, isLitExp, isNumExp, isPrimOp, isProcExp, isStrExp, isVarRef, AppExp } from "./L4-ast";
import { applyEnv, makeEmptyEnv, Env, makeExtEnv, isEmptyEnv, ExtEnv } from './L4-env-normal';
import { applyPrimitive } from "./evalPrimitive";
import {Value, makeClosure, isClosure, Closure} from "./L4-value";
import { first, rest, isEmpty } from '../shared/list';
import {Result, makeOk, makeFailure, bind, mapResult, safe2} from "../shared/result";
import { parse as p } from "../shared/parser";

export const L4normalEval = (exp: CExp, env: Env): Result<Value> =>
    isBoolExp(exp) ? makeOk(exp.val) :
    isNumExp(exp) ? makeOk(exp.val) :
    isStrExp(exp) ? makeOk(exp.val) :
    isPrimOp(exp) ? makeOk(exp) :
    isLitExp(exp) ? makeOk(exp.val) :
    isVarRef(exp) ? bind(applyEnv(env, exp.var), (e: CExp) => L4normalEval(e, env)) :
    isIfExp(exp) ? evalIf(exp, env) :
    isProcExp(exp) ? makeOk(makeClosure(exp.args, exp.body, env)) :
    isLetExp(exp) ? evalLet(exp, env) :
    isAppExp(exp) ? evalApp(exp, env) :
    makeFailure(`Bad ast: ${exp}`);

export const isTrueValue = (x: Value): boolean =>
    ! (x === false);

const evalIf = (exp: IfExp, env: Env): Result<Value> =>
    bind(L4normalEval(exp.test, env),
         (test: Value) => isTrueValue(test) ? L4normalEval(exp.then, env) : L4normalEval(exp.alt, env));

const evalApp = (app: AppExp, env: Env) : Result<Value> => {
    const bindings = envToBindings(env);
    const args = map((rand: CExp) => makeLetExp(bindings, [rand]), app.rands);
    return bind(L4normalEval(app.rator, env), (proc) => L4normalApplyProc(proc, args, env));
};

const envToBindings = (env: Env) : Binding[] => {
    const addNewBindings = (currentBindings: Binding[], newEnv: ExtEnv): Binding[] => {
        const currentVars = map((b:Binding) => b.var.var, currentBindings);
        const pairs = zip(newEnv.vars, newEnv.vals);
        const newEntries = filter((pair) => !currentVars.includes(pair[0]) , pairs);
        return currentBindings.concat(map((pair) => makeBinding(pair[0], pair[1]), newEntries));
    };

    const envToBindingsRecursion = (env: Env, currentBindings: Binding[]): Binding[] =>
        isEmptyEnv(env) ? currentBindings :
            envToBindingsRecursion(env.nextEnv, addNewBindings(currentBindings, env));

    return envToBindingsRecursion(env, []);
};

const L4normalApplyProc = (proc: Value, args: CExp[], env: Env): Result<Value> =>
    isPrimOp(proc) ? applyPrimOp(proc, args, env) :
    isClosure(proc) ? applyClosure(proc, args) :
    makeFailure(`Bad proc applied ${proc}`);

const applyClosure = (proc: Closure, args: CExp[]): Result<Value> => {
    const vars = map((v: VarDecl) => v.var, proc.params);
    return evalExps(proc.body, makeExtEnv(vars, args, proc.env));
};

const applyPrimOp = (proc: PrimOp, args: CExp[], env: Env): Result<Value> => {
    const argVals: Result<Value[]> = mapResult((arg) => L4normalEval(arg, env), args);
    return bind(argVals, (args: Value[]) => applyPrimitive(proc, args));
};

// Evaluate a sequence of expressions (in a program)
export const evalExps = (exps: Exp[], env: Env): Result<Value> =>
    isEmpty(exps) ? makeFailure("Empty sequence") :
    isDefineExp(first(exps)) ? evalDefineExps(first(exps), rest(exps), env) :
    evalCExps(first(exps), rest(exps), env);

const evalCExps = (first: Exp, rest: Exp[], env: Env): Result<Value> =>
    isCExp(first) && isEmpty(rest) ? L4normalEval(first, env) :
    isCExp(first) ? bind(L4normalEval(first, env), _ => evalExps(rest, env)) :
    makeFailure("Never");

const evalDefineExps = (def: Exp, exps: Exp[], env: Env): Result<Value> =>
    isDefineExp(def) ? evalExps(exps, makeExtEnv([def.var.var], [def.val], env)) :
    makeFailure("Unexpected " + def);

export const evalNormalProgram = (program: Program): Result<Value> =>
    evalExps(program.exps, makeEmptyEnv());

export const evalNormalParse = (s: string): Result<Value> =>
    bind(p(s),
        (parsed: Sexp) => bind(parseL4Exp(parsed),
            (exp: Exp) => evalExps([exp], makeEmptyEnv())));

const evalLet = (exp: LetExp, env: Env): Result<Value> => {
    const vals = map((b: Binding) => b.val, exp.bindings);
    const vars = map((b: Binding) => b.var.var, exp.bindings);
    return evalExps(exp.body, makeExtEnv(vars, vals, env));
};