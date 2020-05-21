import { CExp } from './L4-ast';
import { Result, makeOk, makeFailure } from '../shared/result';

// ========================================================
// Environment data type
export type Env = EmptyEnv | ExtEnv;
export interface EmptyEnv {tag: "EmptyEnv" }
export interface ExtEnv {
    tag: "ExtEnv";
    vars: string[];
    vals: CExp[];
    nextEnv: Env;
}

export const makeEmptyEnv = (): EmptyEnv => ({tag: "EmptyEnv"});
export const makeExtEnv = (vs: string[], vals: CExp[], env: Env): ExtEnv =>
    ({tag: "ExtEnv", vars: vs, vals: vals, nextEnv: env});

export const isEmptyEnv = (x: any): x is EmptyEnv => x.tag === "EmptyEnv";
export const isExtEnv = (x: any): x is ExtEnv => x.tag === "ExtEnv";

export const isEnv = (x: any): x is Env => isEmptyEnv(x) || isExtEnv(x);

// Apply-env
export const applyEnv = (env: Env, v: string): Result<CExp> =>
    isEmptyEnv(env) ? makeFailure(`var not found ${v}`) :
    applyExtEnv(env, v);

export const applyExtEnv = (env: ExtEnv, v: string): Result<CExp> =>
    env.vars.includes(v) ? makeOk(env.vals[env.vars.indexOf(v)]) :
    applyEnv(env.nextEnv, v);
