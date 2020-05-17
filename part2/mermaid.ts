import { isAtomicGraph, GraphContent, NodeRef, Graph, makeGraph, makeDir, makeCompoundGraph, Edge, makeEdge, CompoundGraph, Node, makeNodeDecl, AtomicGraph, isNodeDecl, makeNodeRef, isCompoundGraph, isNodeRef } from "./mermaid-ast"
import { IfExp, LetExp, LitExp, LetrecExp, SetExp, isStrExp, isPrimOp, isAtomicExp, isAppExp, Parsed, Exp, isProgram, Program, isNumExp, isExp, isVarRef, makeAppExp, isDefineExp, DefineExp, CExp, isCExp, isBoolExp, VarDecl, parseL4, parseL4Exp, AppExp, isLetrecExp, isSetExp, ProcExp } from "./L4-ast"
import { Result, makeOk, makeFailure, bind, mapResult, safe2 } from "../shared/result";
import { allT, first, second } from "../shared/list"
import { chain, filter, map, zip, repeat, replace, contains } from "ramda";
import { isArray } from "util";
import { isIfExp, isProcExp, isLetExp, isLitExp } from "../L3/L3-ast";
//import {  } from "../L3/L3-ast";

export const mapL4toMermaid = (exp: Parsed): Result<Graph> => 
    isProgram(exp) ? mapProgramtoMermaid(exp) :
    isExp(exp) ? mapExpToMermaid(exp) :
    makeFailure("mapL4toMermaid Not an option");

export const mapProgramtoMermaid = (program: Program): Result<Graph> =>
// TODO: like this: Program_1[Program] -->|exps| Exps_1[:] 
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
    isDefineExp(exp) ? mapDefinetoMermaid(exp) :
    isCExp(exp) ? mapCExptoContent(exp) :
    makeFailure("mapExptoContent Not an option");

export const mapDefinetoMermaid = (exp: DefineExp): Result<CompoundGraph> =>
    makeFailure("mapDefinetoMermaid Not implemented");

/*


*/
export const mapCExptoContent = (exp: CExp): Result<GraphContent> =>
    isAtomicExp(exp) ? mapAtomictoContent(exp) :
    isAppExp(exp) ? mapAppExptoContent(exp) :
    /*isIfExp(exp) ? mapIfExptoContent(exp) :
    isProcExp(exp) ? mapProcExptoContent(exp) :
    isLetExp(exp) ? mapLetExptoContent(exp) :
    isLitExp(exp) ? mapLitExptoContent(exp) : 
    isLetrecExp(exp) ? mapLetrecExptoContent(exp) : 
    isSetExp(exp) ? mapSetExptoContent(exp) :*/
    makeFailure("mapCExptoMermaid Not implemented");

export const mapAtomictoContent = (exp: CExp): Result<AtomicGraph> => 
    /*Object.keys(exp).length !== 2 ? makeFailure("mapAtomicFirstTimetoMermaid: more than 2 keys") :
    makeOk(makeNodeDecl(exp.tag, `${exp.tag}(${Object.keys(exp)[1]})`))*/
    isNumExp(exp) ? makeOk(makeNodeDecl(exp.tag, `${exp.tag}(${exp.val})`)) :
    isBoolExp(exp) ? makeOk(makeNodeDecl(exp.tag, `${exp.tag}(${exp.val})`)) :
    isStrExp(exp) ? makeOk(makeNodeDecl(exp.tag, `${exp.tag}(${exp.val})`)) :
    isPrimOp(exp) ? makeOk(makeNodeDecl(exp.tag, `${exp.tag}(${exp.op})`)) :
    isVarRef(exp) ? makeOk(makeNodeDecl(exp.tag, `${exp.tag}(${exp.var})`)) :
    makeFailure("mapAtomictoContent: Not atomic expression")
    

export const mapArraytoContent = (arr: Exp[], id: string) : Result<CompoundGraph> => 
    bind(mapResult(mapExptoContent, arr),      // First convert each element
        (elements: GraphContent[]): Result<CompoundGraph> =>
            bind(mapResult((g: GraphContent): Result<CompoundGraph> => 
                 isAtomicGraph(g) ? makeOk(makeCompoundGraph([makeEdge(makeNodeRef(id), g)])):
                 isCompoundGraph(g) ? connectEdgeToGraphRoot(makeEdge(makeNodeRef(id), makeNodeRef(id)), g) :
                 makeFailure("mapArraytoMermaid: Not an option")
            , elements),
            (graphs: CompoundGraph[]): Result<CompoundGraph> => joinGraphsEdges(graphs))
    )

export const mapAppExptoContent = (exp: AppExp): Result<CompoundGraph> => 
    safe2((rator: CompoundGraph, rands: CompoundGraph) => joinGraphsEdges([rator, rands]))
    
    // Create the rator Graph
    (bind(mapCExptoContent(exp.rator),
         (ratorGraph: GraphContent): Result<CompoundGraph> =>
            connectEdgeToGraphRoot(makeEdge(makeNodeDecl(exp.tag, exp.tag), 
                                            makeNodeDecl("temp", "temp"), 
                                            "rator"), ratorGraph)),
    // Create the rands Graph
    bind(mapArraytoContent(exp.rands, "Rands"),
        (randsGraph: CompoundGraph): Result<CompoundGraph> =>
            joinEdgeToGraph(makeEdge(makeNodeRef(exp.tag), makeNodeDecl("Rands", ":"), "rands"), randsGraph)
    ))

export const mapVarDecltoContent = (exp: VarDecl, id: string): Result<Node> =>
    makeFailure("mapVarDecltoMermaid:  Not implemented");

export const mapIfExptoContent = (exp: IfExp): Result<CompoundGraph> =>
    makeFailure("mapIfExpltoContent:  Not implemented");

export const mapProcExptoContent = (exp: ProcExp): Result<CompoundGraph> =>
    makeFailure("mapProcExpltoContent:  Not implemented");

export const mapLetExptoContent = (exp: LetExp): Result<CompoundGraph> =>
    makeFailure("mapLetExpltoContent:  Not implemented");

export const mapLitExptoContent = (exp: LitExp): Result<CompoundGraph> =>
    makeFailure("mapLitExpltoContent:  Not implemented");

export const mapLetrecExptoContent = (exp: LetrecExp): Result<CompoundGraph> =>
    makeFailure("mapLetrecExpltoContent:  Not implemented");

export const mapSetExptoContent = (exp: SetExp): Result<CompoundGraph> =>
    makeFailure("maSetExpltoContent:  Not implemented");

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
    const types = ["AppExp", "NumExp", "PrimOp", "Rands"];
    const varGens = map((x: string): (v: string, inc: boolean) => string => 
                            makeVarGen(), types)

    const renameEdge = (e: Edge): Result<Edge> => 
        safe2((from: Node, to: Node) => makeOk(makeEdge(from, to, e.label)))
            (renameNode(e.from), renameNode(e.to))

    const renameNode = (n: Node): Result<Node> => {
        const pos = types.indexOf(n.id);
        const varGen = varGens[pos]

        return  isNodeDecl(n) ? makeOk(makeNodeDecl(varGen(n.id, true), n.label)) :
                isNodeRef(n) ? makeOk(makeNodeRef(varGen(n.id, false))) :
                makeFailure("renameNode: Not an option")
    };

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
    "*",
    JSON.stringify(bind(parseL4("(L4 (+ 2 5) (/ 18 2))"),
        (x: Parsed): Result<Graph> => mapL4toMermaid(x))),
    "**",
);