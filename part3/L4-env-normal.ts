// Environment for L4 (support for Letrec)
// =======================================
// An environment represents a partial function from symbols (variable names) to values.
// It supports the operation: apply-env(env,var)
// which either returns the value of var in the environment, or else throws an error.
//
// Env is defined inductively by the following cases:
// * <env> ::= <empty-env> | <extended-env> | <rec-env>
// * <empty-env> ::= (empty-env) // empty-env()
// * <extended-env> ::= (env (symbol+) (value+) next-env) // env(vars:List(Symbol), vals:List(Value), next-env: Env)
// * <rec-ext-env> ::= (rec-env (symbol+) (params+) (bodies+) next-env)
//       // rec-env(vars:List(Symbol), paramss:List(List(var-decl)), bodiess:List(List(cexp)), next-env: Env)
//
// The key operation on env is apply-env(var) which returns the value associated to var in env
// or throw an error if var is not defined in env.

import { VarDecl, CExp } from './L4-ast';
import { makeClosure, Value } from './L4-value';
import { Result, makeOk, makeFailure } from '../shared/result';

// ========================================================
// Environment data type
export type NormalEnv = EmptyNormalEnv | ExtNormalEnv;
export interface EmptyNormalEnv {tag: "EmptyNormalEnv" }
export interface ExtNormalEnv {
    tag: "ExtNormalEnv";
    vars: string[];
    vals: CExp[];
    nextEnv: NormalEnv;
}

export const makeEmptyNormalEnv = (): EmptyNormalEnv => ({tag: "EmptyNormalEnv"});
export const makeExtNormalEnv = (vs: string[], vals: CExp[], env: NormalEnv): ExtNormalEnv =>
    ({tag: "ExtNormalEnv", vars: vs, vals: vals, nextEnv: env});

export const isEmptyNormalEnv = (x: any): x is EmptyNormalEnv => x.tag === "EmptyNormalEnv";
export const isExtNormalEnv = (x: any): x is ExtNormalEnv => x.tag === "ExtNormalEnv";

export const isEnv = (x: any): x is NormalEnv => isEmptyNormalEnv(x) || isExtNormalEnv(x);

// Apply-env
export const applyNormalEnv = (env: NormalEnv, v: string): Result<CExp> =>
    isEmptyNormalEnv(env) ? makeFailure(`var not found ${v}`) :
    applyExtNormalEnv(env, v);

export const applyExtNormalEnv = (env: ExtNormalEnv, v: string): Result<CExp> =>
    env.vars.includes(v) ? makeOk(env.vals[env.vars.indexOf(v)]) :
    applyNormalEnv(env.nextEnv, v);
