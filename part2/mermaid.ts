import {AtomicGraph, CompoundGraph, Edge, Graph, GraphContent, isAtomicGraph, isCompoundGraph, isNodeDecl,
        isNodeRef, makeCompoundGraph, makeDir, makeEdge, makeGraph, makeHeader, makeNodeDecl, makeNodeRef,
        Node} from "./mermaid-ast"
import {AtomicExp, Exp, isAtomicExp, isBinding, isCompoundExp, isDefineExp, isExp, isProgram, isVarDecl, Parsed,
        parseL4Exp, parseL4Program, Program, VarDecl, parseL4} from "./L4-ast"
import {bind, isOk, makeFailure, makeOk, mapResult, Result, safe2} from "../shared/result";
import {first, isEmpty, rest} from "../shared/list"
import {chain, map, reduce, union} from "ramda";
import {isToken, parse as p} from "../shared/parser";
import {isArray, isBoolean, isNumber, isString} from "../shared/type-predicates"
import {CompoundSExp, EmptySExp, isCompoundSExp, isEmptySExp, isSymbolSExp, SExpValue, SymbolSExp} from "./L4-value"
import {Sexp} from "s-expression";

const defaultGraphDirection = "TD";

/*  === Question 2.3 ===
    Signature: unparseMermaid(g)
    Type: [Graph -> Result<string>]
    Purpose: Convert a Mermaid Graph AST to a concrete syntax string
    Pre-conditions: true
*/
export const unparseMermaid = (g: Graph): Result<string> =>
    bind(unparseGraphContent(g.content), 
        (contentStr: string): Result<string> =>
            makeOk(`graph ${g.header.dir.val}${contentStr}`));

export const unparseGraphContent = (gc: GraphContent): Result<string> =>
    isCompoundGraph(gc) ? unparseCompoundGraph(gc) :
    isAtomicGraph(gc) ? unparseNode(gc) :
    makeFailure("unparseGraphContent: Not an option");

export const unparseCompoundGraph = (g: CompoundGraph): Result<string> =>
    bind(mapResult(unparseEdge, g.edges), 
        (edgeStrs: string[]): Result<string> =>
            makeOk(reduce(concatLines,"" , edgeStrs)));

export const concatLines = (strA: string, strB: string): string =>
    strA + '\n\t' + strB;

export const unparseEdge = (edge: Edge): Result<string> =>
    safe2((fromNode: string, toNode: string): Result<string> => 
        
        edge.label !== undefined ? 
            makeOk(`${fromNode} -->|${edge.label}| ${toNode}`) :
        makeOk(`${fromNode} --> ${toNode}`))
        
        (unparseNode(edge.from), (unparseNode(edge.to)));

export const unparseNode = (node: Node): Result<string> =>
    isNodeRef(node) ? makeOk(`${node.id}`) :
    isNodeDecl(node) ? makeOk(`${node.id}["${node.label}"]`) :
    makeFailure("unparseNode: not an option");

/*  === Question 2.3 ===
    Signature: L4toMermaid(concrete)
    Type: [string -> Result<string>]
    Purpose: Convert a code written in L4 concrete syntanx and convert it to
             a code written in Mermaid concrete syntax
    Pre-conditions: true
*/
export const L4toMermaid = (concrete: string): Result<string> =>
bind(p(concrete), 
    (sexp: Sexp): Result<string> =>
        sexp === "" || isEmpty(sexp) ? makeFailure("Unexpected empty program") :
        isToken(sexp) ? makeFailure("Program cannot be a single token") :
        isArray(sexp) ?
            first(sexp) === "L4" ? L4ProgramToMermaid(sexp) :
            L4ExpToMermaid(sexp) :
        makeFailure("Unexpected type " + sexp));

export const L4ProgramToMermaid = (sexp: Sexp) : Result<string> =>
    bind(parseL4Program(sexp), 
        (p: Program): Result<string> => 
            bind(mapL4toMermaid(p), unparseMermaid));

export const L4ExpToMermaid = (sexp: Sexp): Result<string> =>
    bind(parseL4Exp(sexp), 
        (p: Exp): Result<string> => 
            bind(mapL4toMermaid(p), unparseMermaid));
        
/*  === Question 2.2 ===
    Signature: mapL4toMermaid(exp)
    Type: [Parsed -> Result<Graph>]
    Purpose: Convert a L4 AST to a Mermaid graph AST
    Pre-conditions: true
*/
export const mapL4toMermaid = (exp: Parsed): Result<Graph> => 
    isProgram(exp) ? mapProgramtoMermaid(exp) : 
    isExp(exp) ? mapExpToMermaid(exp) :
    makeFailure("mapL4toMermaid: Not an option");

/*
    Signature: mapProgramtoMermaid(program)
    Type: [Program -> Result<Graph>]
    Purpose: Convert a L4 Program expression AST to a Mermaid graph AST
    Pre-conditions: true
*/
export const mapProgramtoMermaid = (program: Program): Result<Graph> => {
    const newIds = renameVars([program.tag, "exps"], []); 
    return bind(mapCompoundExptoContent(program.exps, newIds[1], newIds), 
        (expsGraph: CompoundGraph): Result<Graph> =>
            bind(makeOk(makeEdge(makeNodeDecl(newIds[0], program.tag), 
                                 makeNodeDecl(newIds[1], ":"), 
                                 "exps")), 
                (firstEdge: Edge): Result<Graph> => 
                    bind(joinGraphsEdges([makeCompoundGraph([firstEdge]), expsGraph]), 
                        (unitedGraph: CompoundGraph): Result<Graph> =>
                            makeOk(makeGraph(makeHeader(makeDir(defaultGraphDirection)), unitedGraph)))))
};
    
/*
    Signature: mapExpToMermaid(exp)
    Type: [Exp -> Result<Graph>]
    Purpose: Convert a L4 Exp expression AST to a Mermaid graph AST
    Pre-conditions: true
*/
export const mapExpToMermaid = (exp: Exp): Result<Graph> => {
    const newName = renameVars([exp.tag], []);
    return bind(mapExptoContent(exp, newName[0], newName), 
            (g: GraphContent): Result<Graph> =>
                isCompoundGraph(g) ? bind(changeRootToNodeDecl(g, exp.tag), 
                                        (c: CompoundGraph): Result<Graph> => 
                                        makeOk(makeGraph(makeHeader(makeDir(defaultGraphDirection)), c))) :
                isAtomicGraph(g) ?
                    makeOk(makeGraph(makeHeader(makeDir(defaultGraphDirection)), g)) :
                makeFailure("mapExpToMermaid: Not an option"))
};

/*
    Signature: mapExptoContent(exp, expId, forbbidenIds)
    Type: [^ * string * string[] -> Result<GraphContent>]
    Purpose: The Router function - directs each exp to its correct mapping function
             exp - the exp to convert
             expId - the id of all the Nodes of exp in the graph
             forbbidenIds - all the graph's current Node ids
    Pre-conditions: 1) expId is unique in the entire graph
                    2) forbbidenIds are ALL the current graph's Ids
*/
export const mapExptoContent = 
    (exp: Exp | CompoundSExp | VarDecl | SymbolSExp | EmptySExp | 
          number  | boolean | string | Exp[],
     expId: string,
     forbbidenIds: string[]): Result<GraphContent> =>

    isDefineExp(exp) ? mapCompoundExptoContent(exp, expId, forbbidenIds) :
    isCompoundExp(exp) ? mapCompoundExptoContent(exp, expId, forbbidenIds) :
    isCompoundSExp(exp) ? mapCompoundExptoContent(exp, expId, forbbidenIds) :
    isAtomicExp(exp) ? mapAtomictoContent(exp, expId) :
    isVarDecl(exp) ? mapAtomictoContent(exp, expId) :
    isSymbolSExp(exp) ?  mapAtomictoContent(exp, expId) :
    isEmptySExp(exp) ? mapEmptyExpressionsToContent(exp, expId) :
    isNumber(exp) ? mapAtomicValuesToContent(exp, expId) :
    isString(exp) ? mapAtomicValuesToContent(exp, expId) :
    isBoolean(exp) ? mapAtomicValuesToContent(exp, expId) :
    isArray(exp) ? mapCompoundExptoContent(exp, expId, forbbidenIds) :
    isBinding(exp) ? mapCompoundExptoContent(exp, expId, forbbidenIds) :
    makeFailure(`mapExptoContent: Unknown Expression: ${JSON.stringify(exp)}`);

/*
    Signature: mapAtomictoContent(exp, expId)
    Type: [AtomicExp | VarDecl | SymbolSExp * string -> Result<AtomicGraph>]
    Purpose: Convert a L4 atomic expression (or str-exp) AST to a Mermaid AtomicGraph
    Pre-conditions: exp is atomic and has only 2 values {tag: _, X: _}
*/
export const mapAtomictoContent = (exp: AtomicExp | VarDecl | SymbolSExp, 
                                  expId: string): Result<AtomicGraph> => 
    Object.values(exp).length === 2 ? 
        Object.values(exp)[1] === true ? makeOk(makeNodeDecl(expId,`${exp.tag}(#t)`)) :
        Object.values(exp)[1] === false ? makeOk(makeNodeDecl(expId,`${exp.tag}(#f)`)) :
        makeOk(makeNodeDecl(expId,`${exp.tag}(${Object.values(exp)[1]})`)) :
    makeFailure("mapAtomictoContent: Atomic Expression with more than 2 keys");

/*
    Signature: mapAtomicValuesToContent(exp, expId)
    Type: [number | string | boolean * string -> Result<AtomicGraph>]
    Purpose: Convert an atomic value to an AtomicGraph
    Pre-conditions: true
*/
export const mapAtomicValuesToContent = (exp: number | string | boolean, expId: string): Result<AtomicGraph> =>
    exp === true ? makeOk(makeNodeDecl(expId,`${typeof(exp)}(#t)`)) :
    exp === false ? makeOk(makeNodeDecl(expId,`${typeof(exp)}(#f)`)) :
    makeOk(makeNodeDecl(expId,`${typeof(exp)}(${exp})`));

/*
    Signature: mapEmptyExpressionsToContent(exp, expId)
    Type: [EmptySExp * string -> Result<AtomicGraph>]
    Purpose: Convert an empty atomic expression value to an AtomicGraph
    Pre-conditions: true
*/
export const mapEmptyExpressionsToContent = (exp: EmptySExp, expId: string): Result<AtomicGraph> =>
    makeOk(makeNodeDecl(expId,`${exp.tag}`));

/*
    Signature: mapCompoundExptoContent(exp, expId, forbbidenIds)
    Type: [Exp | CompoundSExp | Exp[] * string * string[] -> Result<CompoundGraph>]
    Purpose: Convert recursively a Non-Atomic L4 Expression to a CompoundGraph.
             This method is generic, it doesn't care about the specific type of exp.
    Pre-conditions: same as mapExptoContent
    REMARK: THE FUNCTION CAN BE A PURE EXPRESSION! 
            HOWEVER we use consts to make the code more understandable
*/
export const mapCompoundExptoContent = (exp: Exp | CompoundSExp | Exp[], 
                                        expId: string, 
                                        forbbidenIds: string[]): Result<CompoundGraph> => {

    // Here we take all the expression's parameters and values generically
    // if exp is not an array, it means that it's a Compound(S)Exp. So take all the
    //      the keys and the values from the object (except "tag", it is not needed)
    // if exp is an array, it was a parameter of a previous Compound(S)Exp,
    //      so it has no keys. and the values are its elements
    const keys = !isArray(exp) ? rest(Object.keys(exp)) : [];

    const values = !isArray(exp) ? rest(Object.values(exp)) : exp;

    // Extract all the values names
    const valuesTags = map((v):string => "" === extractTag(v)
                                            ? keys[values.indexOf(v)] // in case of array
                                            : extractTag(v), values);

    // Rename all values names according to restrictions given
    const childrenIds = renameVars(valuesTags, forbbidenIds); 

    // convertValues - Convert each child from left to right
    // after converting each one, take all the NodeIds from its graph
    // and pass them as forbbiden to the next child
    // each value is now a graph content, all of them joined in one array
    return bind(convertValues(values, expId, childrenIds, union(childrenIds, forbbidenIds)), 
            // Now we create for each child an Edge from expId --> NodeDecl(child)
            // and then unite them all into one beautiful graph
            (childGraphs: GraphContent[]): Result<CompoundGraph> => 
                // So, for each GraphContent in childGraphs
                bind(mapResult((gc: GraphContent): Result<Edge>=>
                        // if it was originally an array, 
                        // make ExpId_X -->|key[child]|ChildId_X[":"]
                        isArray(values[childGraphs.indexOf(gc)]) ? 
                            makeOk(makeEdge(
                                    makeNodeRef(expId),
                                    makeNodeDecl(childrenIds[childGraphs.indexOf(gc)], ":"),
                                    !isArray(exp) ? keys[childGraphs.indexOf(gc)] : undefined)) :
                        // if the child is an atomic graph, 
                        // make ExpId_X --> |key[child]|ChildId_X[Child(Value)]
                        isAtomicGraph(gc) ? 
                            makeOk(makeEdge(
                                    makeNodeRef(expId),
                                    gc,
                                    keys[childGraphs.indexOf(gc)])) : 
                        // if the child is a CompoundGraph,
                        // make ExpId_X --> |key[child]|ChildId_X[Child]
                        isCompoundGraph(gc) ? 
                            makeOk(makeEdge(
                                    makeNodeRef(expId),
                                    makeNodeDecl(childrenIds[childGraphs.indexOf(gc)], 
                                                    valuesTags[childGraphs.indexOf(gc)]),
                                                    keys[childGraphs.indexOf(gc)])) :
                        makeFailure("mapCompoundExptoContent: (Creating Edges) Not an option")
                    , childGraphs),
                    // Finally, Join all the graphs of the children
                    // and then put on top of them the edges
                    (childrenEdges: Edge[]): Result<CompoundGraph> => 
                        bind(joinGraphsEdges(childGraphs), 
                            (unitedChildren: CompoundGraph): Result<CompoundGraph> =>
                                joinGraphsEdges([makeCompoundGraph(childrenEdges), unitedChildren]))))

    };

/*
    Signature: convertValues(exp, expId, childrenIds, forbbidenIds)
    Type: [Exp[] * string * string[] * string[] -> Result<CompoundGraph>]
    Purpose: Convert each child from left to right
             after converting each one, take all the NodeIds from its graph
             and pass them as forbbiden to the next child
             each value is now a graph content, all of them joined in one array.
             * childrenIds - are the Ids for the exps' Nodes given by their parent
    Pre-conditions: same as mapExptoContent
*/
export const convertValues = (exps: Exp[], expId: string, childrenIds: string[], forbbidenIds: string[]): Result<GraphContent[]> =>
    // Reduce is going through the exps array, convert each child to a GraphContent
    // and join them all into one array
    reduce((convertedExps: Result<GraphContent[]>, e: Exp): Result<GraphContent[]> =>
        !isOk(convertedExps) ? convertedExps :
        // First, extract all the node ids from the previous edges
        // join them to the given childrenIds and pass them forward as forbbidenNames
        bind(makeOk(union(childrenIds, extractNodesIdsFromContents(convertedExps.value))),
                (forbbidenNames: string[]): Result<GraphContent[]> =>
                    // Now, convert the single exp using its given id
                    bind(mapExptoContent(e, childrenIds[exps.indexOf(e)], union(forbbidenNames, forbbidenIds)), 
                        (g: GraphContent): Result<GraphContent[]> => 
                            // return to reduce the united array of all the prevs and this one
                            makeOk(union(convertedExps.value, [g]))))
            
        , makeOk([]), exps);
    

////////////////////////////////////////////////////
//  Utility Methods regarding Graph Manipulations
////////////////////////////////////////////////////

/*
    Signature: joinGraphsEdges(graphs)
    Type: [GraphContent[] -> Result<CompoundGraph>]
    Purpose: Given a list of GraphContent, combine all the Edges arrays into
             a single array of Edges 
    Pre-conditions: true
*/
export const joinGraphsEdges = (graphs: GraphContent[]) : Result<CompoundGraph> => 
    // If the content is compound, take its edges
    // otherwise, it is not neccessary, because the parent created an edge for it
    makeOk(makeCompoundGraph(chain((g: GraphContent) : Edge[] => 
                                isCompoundGraph(g) ? g.edges : []
                            ,graphs)));

/*
    Signature: getGraphRoot(graph)
    Type: [GraphContent -> Result<Node>]
    Purpose: return a copy of the first Node in the Edges list
    Pre-conditions: true
*/
export const getGraphRoot = (graph: GraphContent) : Result<Node> => 
    isCompoundGraph(graph) ? makeOk(graph.edges[0].from) :
    isAtomicGraph(graph) ? makeOk(graph) :
    makeFailure("getGraphRoot: Not an option");

/*
    Signature: changeRootToNodeDecl(graph)
    Type: [CompoundGraph -> Result<CompoundGraph>]
    Purpose: change the first node in the first edge to NodeDecl
    Pre-conditions: true
*/
export const changeRootToNodeDecl = (graph: CompoundGraph, label: string): Result<CompoundGraph> => 
safe2((root: Node, edges: Edge[]) => 
    joinGraphsEdges(
        [makeCompoundGraph([makeEdge(makeNodeDecl(root.id, label),
                                     first(edges).to,
                                     first(edges).label)]),
         makeCompoundGraph(rest(edges))]))

(getGraphRoot(graph), makeOk(graph.edges));

////////////////////////////////////////////////////
//  Utility Methods regarding Renaming Nodes
////////////////////////////////////////////////////

/*
    Signature: renameVars(vars, forbbidenNames)
    Type: [string[] * string[] ->string[]>]
    Purpose: Change the name of the vars according to the Mermaid rules:
                - var "type" has its own counter (and not a global counter).
                  Meaning, AppExp_i and ProcExp_j are incremented independently
                - each var generated is unique (doesn't appear in forbbidenNames)
    Pre-conditions: true
*/                         
export const renameVars = (vars: string[], forbbidenNames: string[]): string[] => {
    // make sure vars is unique
    const setOfVars = union([], vars);

    // For each type create its OWN var generator (so it will get its own counter)
    const varGens = map((x: string): (v: string) => string => 
                            makeVarGen(), setOfVars);

    // Helper function to match UpperCase Mermaid convention
    const upperCaseFirstLetter = (s: string) : string => 
        s.charAt(0).toUpperCase() + s.substring(1);

    const renameVar = (s: string): string => {
        // get the matching var generator
        const pos = setOfVars.indexOf(s);
        const varGen = varGens[pos];
        // try to rename
        const tempName = ["number", "string", "boolean"].includes(s) ? varGen(s) :
                          upperCaseFirstLetter(varGen(s));
        // if the name is in the forbbidenNams, try again
        return forbbidenNames.indexOf(tempName) !== -1 ? renameVar(s) : tempName;
    };

    return map(renameVar, vars);
};

export const makeVarGen = (): (v: string) => string => {
    let count: number = 0;
    return (v: string) => {
        count++;
        return `${v}_${count}`;
    };
};

/*
    Signature: extractTag(x)
    Type: [Exp | SExpValue ->string>]
    Purpose: Self-Explanatory
    Pre-conditions: true
*/
export const extractTag = (x: Exp | SExpValue) : string =>
        isExp(x) ? x.tag :
        isSymbolSExp(x) ? x.tag :
        isEmptySExp(x) ? x.tag :
        isCompoundSExp(x) ? x.tag : 
        isVarDecl(x) ? x.tag :
        isBinding(x) ? x.tag :
        isNumber(x) ? "number" :
        isString(x) ? "string" :
        isBoolean(x) ? "boolean" : "";

/*
    Signature: extractNodesIdsFromEdges(edges)
    Type: [Edge[] -> string[] ]
    Purpose: Self-Explanatory
    Pre-conditions: true
*/
export const extractNodesIdsFromEdges = (edges: Edge[]) : string[] =>
    union(map((e: Edge): string => e.from.id, edges), 
          map((e: Edge): string => e.to.id, edges));

/*
    Signature: extractNodesIdsFromContents(contents)
    Type: [GraphContent[] -> string[] ]
    Purpose: Self-Explanatory
    Pre-conditions: true
*/
export const extractNodesIdsFromContents = (contents: GraphContent[]) : string[] =>
    chain((x: string[]): string[] => x, 
        map((g: GraphContent): string[] => 
            isCompoundGraph(g) ? extractNodesIdsFromEdges(g.edges) : [g.id] ,contents));