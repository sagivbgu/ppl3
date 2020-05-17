import { isAtomicGraph, GraphContent, NodeRef, Graph, makeGraph, makeDir, makeCompoundGraph, Edge, makeEdge, CompoundGraph, Node, makeNodeDecl, AtomicGraph, isNodeDecl, makeNodeRef, isCompoundGraph } from "./mermaid-ast"
import { isStrExp, isPrimOp, isAtomicExp, isAppExp, Parsed, Exp, isProgram, Program, isNumExp, isExp, isVarRef, makeAppExp, isDefineExp, DefineExp, CExp, isCExp, isBoolExp, VarDecl, parseL4, parseL4Exp, AppExp } from "./L4-ast"
import { Result, makeOk, makeFailure, bind, mapResult, safe2 } from "../shared/result";
import { allT } from "../shared/list"
import { chain } from "ramda";
//import {  } from "../L3/L3-ast";

export const mapL4toMermaid = (exp: Parsed): Result<Graph> => 
    isProgram(exp) ? mapProgramtoMermaid(exp) :
    isExp(exp) ? bind(mapExptoContent(exp),
                      (g: GraphContent): Result<Graph> => 
                        makeOk(makeGraph(makeDir("TD"), g))):
    makeFailure("mapL4toMermaid Not an option");

export const mapProgramtoMermaid = (program: Program): Result<Graph> =>
    bind(mapResult(mapExptoContent, program.exps), 
        (graphs: GraphContent[]): Result<Graph> => 
            allT(isCompoundGraph, graphs) ? bind(joinGraphsEdges(graphs), 
                                                (g: CompoundGraph): Result<Graph> => 
                                                    makeOk(makeGraph(makeDir("TD"), g))) :
            isAtomicGraph(graphs[0]) ? makeOk(makeGraph(makeDir("TD"), graphs[0])) :
            makeFailure("mapProgramtoMermaid impossible"))

export const mapExptoContent = (exp: Exp): Result<GraphContent> =>  
    isDefineExp(exp) ? mapDefinetoMermaid(exp) :
    isCExp(exp) ? mapCExptoMermaid(exp) :
    makeFailure("mapExptoContent Not an option");

export const mapDefinetoMermaid = (exp: DefineExp): Result<CompoundGraph> =>
    makeFailure("mapDefinetoMermaid Not implemented");

/*
export type AtomicExp = NumExp | BoolExp | StrExp | PrimOp | VarRef;
export type CompoundExp = AppExp | IfExp | ProcExp | LetExp | LitExp | LetrecExp | SetExp;
export type CExp =  AtomicExp | CompoundExp;
*/
export const mapCExptoMermaid = (exp: CExp): Result<GraphContent> =>
    isAtomicExp(exp) ? mapAtomicFirstTimetoMermaid(exp) :
    isAppExp(exp) ? mapAppExptoMermaid(exp) :
    makeFailure("mapCExptoMermaid Not implemented");

export const mapAtomicFirstTimetoMermaid = (exp: CExp): Result<AtomicGraph> =>
    isNumExp(exp) ? makeOk(makeNodeDecl(exp.tag, `${exp.tag}(${exp.val})`)) :
    isBoolExp(exp) ? makeOk(makeNodeDecl(exp.tag, `${exp.tag}(${exp.val})`)) :
    isStrExp(exp) ? makeOk(makeNodeDecl(exp.tag, `${exp.tag}(${exp.val})`)) :
    isPrimOp(exp) ? makeOk(makeNodeDecl(exp.tag, `${exp.tag}(${exp.op})`)) :
    isVarRef(exp) ? makeOk(makeNodeDecl(exp.tag, `${exp.tag}(${exp.var})`)) :
    makeFailure("mapAtomicFirstTimetoMermaid: Not an AtomicExp");

export const mapVarDecltoMermaid = (exp: VarDecl, id: string): Result<Node> =>
    makeFailure("mapVarDecltoMermaid Not implemented");

export const mapArraytoMermaid = (arr: CExp[], id: string) : Result<CompoundGraph> => 
    bind(mapResult(mapCExptoMermaid, arr),      // First convert each element
        (elements: GraphContent[]): Result<CompoundGraph> =>
            bind(mapResult((g: GraphContent): Result<CompoundGraph> => 
                 isAtomicGraph(g) ? makeOk(makeCompoundGraph([makeEdge(makeNodeRef(id), g)])):
                 isCompoundGraph(g) ? connectEdgeToGraphRoot(makeEdge(makeNodeRef(id), makeNodeRef(id)), g) :
                 makeFailure("mapArraytoMermaid: Not an option")
            , elements),
            (graphs: CompoundGraph[]): Result<CompoundGraph> => joinGraphsEdges(graphs))
    )

export const mapAppExptoMermaid = (exp: AppExp): Result<CompoundGraph> => 

    safe2((rator: CompoundGraph, rands: CompoundGraph) => joinGraphsEdges([rator, rands]))

    // Create the rator Graph
    (bind(mapCExptoMermaid(exp.rator),
         (ratorGraph: GraphContent): Result<CompoundGraph> =>
            connectEdgeToGraphRoot(makeEdge(makeNodeDecl(exp.tag, exp.tag), 
                                            makeNodeDecl("temp", "temp"), 
                                            "rator"), ratorGraph)),
    
    // Create the rands Graph
    bind(mapArraytoMermaid(exp.rands, "rands"),
        (randsGraph: CompoundGraph): Result<CompoundGraph> =>
            joinEdgeToGraph(makeEdge(makeNodeRef(exp.tag), makeNodeDecl("rands", ":"), "rands"), randsGraph)
    ))

/*
    Take an array of graphs and join their Edges into one compound graph by order given
*/
export const joinGraphsEdges = (graphs: CompoundGraph[]) : Result<CompoundGraph> =>
    makeOk(makeCompoundGraph(chain((g: CompoundGraph) : Edge[] => g.edges ,graphs)))

/*
    Take a single Edge and a graph and join all the edges into a new compound graph
*/
export const joinEdgeToGraph = (edge: Edge, graph: CompoundGraph) : Result<CompoundGraph> =>
    makeOk(makeCompoundGraph(chain((e: Edge[]) : Edge[] => e ,[[edge], graph.edges])))

/*
    Returns a NodeRef to the first node in the graph
*/
export const getGraphRoot = (graph: GraphContent) : Result<Node> => 
    isCompoundGraph(graph) ? makeOk(graph.edges[0].from) :
    isAtomicGraph(graph) ? makeOk(graph) :
    makeFailure("getGraphRoot: Not an option")

/*

*/
export const connectEdgeToGraphRoot = (top: Edge, graph: GraphContent): Result<CompoundGraph> =>
    bind(getGraphRoot(graph),
        (root: Node): Result<CompoundGraph> => 
            isCompoundGraph(graph) ? joinEdgeToGraph(makeEdge(top.from, root, top.label), graph) :
            isAtomicGraph(graph) ? makeOk(makeCompoundGraph([makeEdge(top.from, root, top.label)])) :    
            makeFailure("connectEdgeToGraphRoot: Not an option"))


export const makeVarGen = (): (v: string) => string => {
    let count: number = 0;
    return (v: string) => {
        count++;
        return `${v}_${count}`;
    };
};

console.log(
    "*",
    JSON.stringify(bind(parseL4("(L4 (+ 2 5))"), 
        (x: Parsed): Result<Graph> => mapL4toMermaid(x))),
    "**"
);