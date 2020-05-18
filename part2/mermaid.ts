import { isAtomicGraph, GraphContent, NodeRef, Graph, makeGraph, makeDir, makeCompoundGraph, Edge, makeEdge, CompoundGraph, Node, makeNodeDecl, AtomicGraph, isNodeDecl, makeNodeRef, isCompoundGraph, isNodeRef } from "./mermaid-ast"
import { isCompoundExp, CompoundExp, isIfExp, isProcExp, isLetExp, isLitExp, IfExp, LetExp, LitExp, LetrecExp, SetExp, isStrExp, isPrimOp, isAtomicExp, isAppExp, Parsed, Exp, isProgram, Program, isNumExp, isExp, isVarRef, makeAppExp, isDefineExp, DefineExp, CExp, isCExp, isBoolExp, VarDecl, parseL4, parseL4Exp, AppExp, isLetrecExp, isSetExp, ProcExp } from "./L4-ast"
import { Result, makeOk, makeFailure, bind, mapResult, safe2 } from "../shared/result";
import { allT, first, second } from "../shared/list"
import { union, chain, filter, map, zip, repeat, replace, contains, slice } from "ramda";
import { isArray } from "util";
import {  } from "../L3/L3-ast";

export const mapL4toMermaid = (exp: Parsed): Result<Graph> => 
    isProgram(exp) ? mapProgramtoMermaid(exp) :
    isExp(exp) ? mapExpToMermaid(exp) :
    makeFailure("mapL4toMermaid Not an option");

export const mapProgramtoMermaid2 = (program: Program): Result<Graph> =>
    bind(mapCompoundExptoContent(program), 
            (proGraph: CompoundGraph): Result<Graph> =>
                bind(renameNodes(proGraph.edges),
                (edges: Edge[]): Result<Graph> =>
                    makeOk(makeGraph(makeDir("TD"), makeCompoundGraph(edges)))))

export const mapProgramtoMermaid = (program: Program): Result<Graph> =>
    bind(mapResult(mapExptoContent, program.exps), 
    
        (graphs: GraphContent[]): Result<Graph> => 
            allT(isCompoundGraph, graphs) ? 
                bind(joinGraphsEdges(graphs), 
                    (g: CompoundGraph): Result<Graph> =>
                        bind(renameNodes(g.edges),
                            (edges: Edge[]): Result<Graph> =>
                                makeOk(makeGraph(makeDir("TD"), makeCompoundGraph(edges))))) :

            isAtomicGraph(graphs[0]) ? bind(renameAtomicGraph(graphs[0]), 
                                            (g: AtomicGraph): Result<Graph> => 
                                            makeOk(makeGraph(makeDir("TD"), graphs[0]))) :
            
            makeFailure("mapProgramtoMermaid impossible"))

export const mapExpToMermaid = (exp: Exp): Result<Graph> =>
/*
bind(mapExptoContent(exp),
                    (g: GraphContent): Result<Graph> => 
                        makeOk(makeGraph(makeDir("TD"), g))):
                        */
    makeFailure("not implemenetd");

export const mapExptoContent = (exp: Exp): Result<GraphContent> =>   
    isDefineExp(exp) ? mapCompoundExptoContent(exp) :
    isCExp(exp) ? mapCExptoContent(exp) :
    makeFailure("mapExptoContent Not an option");

/*

*/
export const mapCExptoContent = (exp: CExp): Result<GraphContent> =>
    isAtomicExp(exp) ? mapAtomictoContent(exp) :
    isCompoundExp(exp) ? mapCompoundExptoContent(exp):
    makeFailure("mapCExptoMermaid Not implemented");

/*
    transform an atomic expression to an atomic graph
*/
export const mapAtomictoContent = (exp: CExp): Result<AtomicGraph> => 
    // Known atomic expression has only two values {tag: X, val/var/op: Y }
    Object.values(exp).length === 2 ? 
    makeOk(makeNodeDecl(exp.tag,`${exp.tag}(${Object.values(exp)[1]})`)) :
    makeFailure("mapAtomicFirstTimetoMermaid: more than 2 keys")

export const mapArraytoContent = (arr: Exp[], id: string) : Result<CompoundGraph> => 
    // First, convert each exp in arr to a graph content
    bind(mapResult(mapExptoContent, arr),      
        (expressions: GraphContent[]): Result<CompoundGraph> => 
            // Second, add to each content an Edge from [:] --> to the content
            bind(mapResult((g: GraphContent): Result<CompoundGraph> => 
                 isAtomicGraph(g) ? makeOk(makeCompoundGraph([makeEdge(makeNodeRef(id), g)])):
                 isCompoundGraph(g) ? connectEdgeToGraphRoot(makeEdge(makeNodeRef(id), makeNodeRef(id)), g) :
                 makeFailure("mapArraytoMermaid: Not an option")
            , expressions),
            // Third, join all the edges together
            (graphs: CompoundGraph[]): Result<CompoundGraph> => joinGraphsEdges(graphs))
    )


export const mapCompoundExptoContent = (expParent: CompoundExp | Program | DefineExp): Result<CompoundGraph> => {
    const keys = slice(1, Infinity, Object.keys(expParent));
    const values = slice(1, Infinity, Object.values(expParent));
    
    // For each value in exp (which can be an array or a single exp) do the following
    return bind(mapResult((e: Exp | Exp[]): Result<CompoundGraph> =>
            // if the value is an array
            isArray(e) ? 
                // Happy flow: create a new edge 
                safe2((idAndLabel: string, arrayValueGraph: CompoundGraph): Result<CompoundGraph> => 
                    joinEdgeToGraph(makeEdge(makeNodeRef(expParent.tag), 
                                             makeNodeDecl(idAndLabel, ":"), 
                                                          idAndLabel), 
                                                          arrayValueGraph))
                (makeOk((keys[values.indexOf(e)])),
                (mapArraytoContent(e, keys[values.indexOf(e)]))) 

            // Otherwise, if the value is a single Exp
            : bind(mapExptoContent(e), 
                (singleValueGraph: GraphContent): Result<CompoundGraph> =>
                connectEdgeToGraphRoot(makeEdge(makeNodeDecl(expParent.tag, expParent.tag), 
                                                makeNodeDecl("temp", "temp"),
                                                keys[values.indexOf(e)]), 
                                                singleValueGraph))
            , values), 

            (graphs: CompoundGraph[]): Result<CompoundGraph> =>
                joinGraphsEdges(graphs)
            )
}

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
    Returns a copy of the first node in the graph
*/
export const getGraphRoot = (graph: GraphContent) : Result<Node> => 
    isCompoundGraph(graph) ? makeOk(graph.edges[0].from) :
    isAtomicGraph(graph) ? makeOk(graph) :
    makeFailure("getGraphRoot: Not an option")

/*
    takes an Edge and a Graph, and makes the edge point to the first node of the graph
*/
export const connectEdgeToGraphRoot = (top: Edge, graph: GraphContent): Result<CompoundGraph> =>
    bind(getGraphRoot(graph),
        (root: Node): Result<CompoundGraph> => 
            isCompoundGraph(graph) ? joinEdgeToGraph(makeEdge(top.from, root, top.label), graph) :
            isAtomicGraph(graph) ? makeOk(makeCompoundGraph([makeEdge(top.from, root, top.label)])) :    
            makeFailure("connectEdgeToGraphRoot: Not an option"))

/*
    Takes an array of edges an change each node id to a unique name
*/
export const renameNodes = (edges: Edge[]): Result<Edge[]> => {
    const upperCaseFirstLetter = (s: string) : string => 
        s.charAt(0).toUpperCase() + s.substring(1)

    const generateTypes = (edges: Edge[]): string[] => 
        union([], chain((e: Edge): string[] => [e.from.id, e.to.id], edges))

    const renameEdge = (e: Edge): Result<Edge> => 
        safe2((from: Node, to: Node) => makeOk(makeEdge(from, to, e.label)))
            (renameNode(e.from), renameNode(e.to))

    const renameNode = (n: Node): Result<Node> => {
        const pos = types.indexOf(n.id);
        const varGen = varGens[pos]

        return  isNodeDecl(n) ? makeOk(makeNodeDecl(upperCaseFirstLetter(varGen(n.id, true)), n.label)) :
                isNodeRef(n) ? makeOk(makeNodeRef(upperCaseFirstLetter(varGen(n.id, false)))) :
                makeFailure("renameNode: Not an option")
    };

    const types = generateTypes(edges);

    const varGens = map((x: string): (v: string, inc: boolean) => string => 
                            makeVarGen(), types)

    return mapResult(renameEdge ,edges)
};

/*
    like renameEdges but for the single Node in the AtomicGraph
*/
export const renameAtomicGraph = (g : AtomicGraph): Result<AtomicGraph> => {
    const varGen = makeVarGen();
    return  isNodeDecl(g) ? makeOk(makeNodeDecl(varGen(g.id, true), g.label)) :
            makeFailure("renameAtomicGraph: not an option")
};

export const makeVarGen = (): (v: string, inc: boolean) => string => {
    let count: number = 0;
    return (v: string, inc: boolean) => {
        inc ? count++ : count = count;
        return `${v}_${count}`;
    };
};

console.log(
    //"*",
    JSON.stringify(bind(parseL4("(L4 (+ 2 5))"),
        (x: Parsed): Result<Graph> => mapL4toMermaid(x))),
    //"**",
);