import { isAtomicGraph, GraphContent, Graph, makeGraph, makeDir, makeCompoundGraph, Edge, makeEdge, CompoundGraph, Node, makeNodeDecl, AtomicGraph, isNodeDecl, makeNodeRef, isCompoundGraph, isNodeRef, unparseMermaid } from "./mermaid-ast"
import { VarDecl, isVarDecl, isCompoundExp, CompoundExp, isAtomicExp, Parsed, Exp, isProgram, Program,  isExp, isDefineExp, DefineExp, CExp, isCExp, parseL4, parseL4Exp } from "./L4-ast"
import { isOk, Result, makeOk, makeFailure, bind, mapResult, safe2 } from "../shared/result";
import { first, rest } from "../shared/list"
import { union, chain, map } from "ramda";
import { isArray, isNumber, isString, isBoolean } from "util";
import { isSymbolSExp, isEmptySExp, EmptySExp, SymbolSExp, CompoundSExp, isCompoundSExp } from "./L4-value"

/*
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
export const mapProgramtoMermaid = (program: Program): Result<Graph> =>
    bind(mapCompoundExptoContent(program), 
            (proGraph: CompoundGraph): Result<Graph> =>
                bind(renameNodes(proGraph.edges),
                (edges: Edge[]): Result<Graph> =>
                    makeOk(makeGraph(makeDir("TD"), makeCompoundGraph(edges)))))

/*
    Signature: mapExpToMermaid(exp)
    Type: [Exp -> Result<Graph>]
    Purpose: Convert a L4 Exp expression AST to a Mermaid graph AST
    Pre-conditions: true
*/
export const mapExpToMermaid = (exp: Exp): Result<Graph> => 
    bind(mapExptoContent(exp), 
        (expGraph: GraphContent): Result<Graph> =>
            isCompoundGraph(expGraph) ?
                bind(renameNodes(expGraph.edges),
                (edges: Edge[]): Result<Graph> =>
                    makeOk(makeGraph(makeDir("TD"), makeCompoundGraph(edges)))) :
            isAtomicGraph(expGraph) ? 
                bind(renameAtomicGraph(expGraph), 
                        (g: AtomicGraph): Result<Graph> => 
                            makeOk(makeGraph(makeDir("TD"), g))) :
            makeFailure("mapExpToMermaid: not an option"))

/*
    Signature: mapExptoContent(exp)
    Type: [* -> Result<GraphContent>]
    Purpose: Convert a L4 Exp expression AST to a Mermaid GraphContent,
             Atomic (single Node) or Compund (list of Edges) 
    Pre-conditions: true
*/
export const mapExptoContent = (exp: Exp    | CompoundSExp  | 
                                    VarDecl | SymbolSExp    | EmptySExp | 
                                    number  | boolean       | string): 
                                        Result<GraphContent> => 
    // Compound Expressions (and special forms)
    isCExp(exp) ? mapCExptoContent(exp) : 
    isDefineExp(exp) ? mapCompoundExptoContent(exp) :
    isCompoundSExp(exp) ? mapCompoundExptoContent(exp) :

    // Atomic Expressions
    isAtomicExp(exp) ? mapAtomictoContent(exp) :
    isVarDecl(exp) ? mapAtomictoContent(exp) :
    isSymbolSExp(exp) ? mapAtomictoContent(exp) :

    // Atomic Values
    isNumber(exp) ? mapAtomicValuesToContent(exp) :
    isString(exp) ? mapAtomicValuesToContent(exp) :
    isBoolean(exp) ? mapAtomicValuesToContent(exp) :

    // Empty Values
    isEmptySExp(exp) ? mapEmptyExpressionsToContent(exp) :
    makeFailure(`mapExptoContent: Not supporting ${exp}`)

/*
    Signature: mapCExptoContent(exp)
    Type: [CExp -> Result<GraphContent>]
    Purpose: Convert a L4 CExp expression AST to a Mermaid GraphContent,
             Atomic (single Node) or Compund (list of Edges) 
    Pre-conditions: true
*/
export const mapCExptoContent = (exp: CExp): Result<GraphContent> =>
    isAtomicExp(exp) ? mapAtomictoContent(exp) :
    isCompoundExp(exp) ? mapCompoundExptoContent(exp):
    makeFailure("mapCExptoMermaid Not implemented");

/*
    Signature: mapAtomictoContent(exp)
    Type: [CExp -> Result<AtomicGraph>]
    Purpose: Convert a L4 atomic expression AST to a Mermaid GraphContent,
             Atomic (single Node) or Compund (list of Edges) 
    Pre-conditions: exp is atomic and has only 2 values!
*/
export const mapAtomictoContent = (exp: CExp | VarDecl | SymbolSExp): Result<AtomicGraph> =>
    // Known atomic expression has only two values {tag: X, val/var/op: Y }
    Object.values(exp).length === 2 ? 
    makeOk(makeNodeDecl(exp.tag,`${exp.tag}(${Object.values(exp)[1]})`)) :
    makeFailure("mapAtomicFirstTimetoMermaid: more than 2 keys")

/*
    Signature: mapAtomicValuesToContent(exp)
    Type: [number | string | boolean -> Result<AtomicGraph>]
    Purpose: Convert an atomic value to an AtomicGraph
    Pre-conditions: exp must be an atomic value
*/
export const mapAtomicValuesToContent = (exp: number | string | boolean): Result<AtomicGraph> =>
    makeOk(makeNodeDecl(typeof(exp),`${typeof(exp)}(${exp})`))

/*
    Signature: mapEmptyExpressionsToContent(exp)
    Type: [EmptySExp -> Result<AtomicGraph>]
    Purpose: Convert an empty atomic expression value to an AtomicGraph
    Pre-conditions: exp must be an atomic value
*/
export const mapEmptyExpressionsToContent = (exp: EmptySExp): Result<AtomicGraph> =>
    makeOk(makeNodeDecl(exp.tag,`${exp.tag}`))
/*
    Signature: mapCompoundExptoContent(expParent)
    Type: [Exp | Program -> Result<CompoundGraph>]
    Purpose: Convert recursively a Non-Atomic L4 Expression to a CompoundGraph.
             This method is generic, it doesn't care about the specific type of expParent.
    Pre-conditions: expParent is Non Atomic L4 Expression
*/
export const mapCompoundExptoContent = (expParent: Exp | Program | CompoundSExp): Result<CompoundGraph> => {
    const keys = rest(Object.keys(expParent));
    const values = rest(Object.values(expParent));
    
    // For each value in exp (which can be an array or a single exp) do the following
    return bind(mapResult((e: Exp | Exp[]): Result<CompoundGraph> =>
            // if the value is an array
            isArray(e) ? 
                // Happy flow: create a new edge "expParent.tag -->|name|[:] "
                // with the declartion "[:]" and connect it 
                // to the graph given by mapArraytoContent
                safe2((idAndLabel: string, arrayValueGraph: CompoundGraph): Result<CompoundGraph> => 
                    joinEdgeToGraph(makeEdge(makeNodeRef(expParent.tag),    
                                             makeNodeDecl(idAndLabel, ":"), 
                                                          idAndLabel), 
                                                          arrayValueGraph))
                (makeOk((keys[values.indexOf(e)])),
                (mapArraytoContent(e, keys[values.indexOf(e)]))) 

            // Otherwise, if the value is a single Exp
            // Convert it to Graph Content
            : bind(mapExptoContent(e), 
                (singleValueGraph: GraphContent): Result<CompoundGraph> =>
                // Then create a new edge "expParent.tag --> "
                // and connect it to the conent given by mapExptoContent
                // the "temp" will be replaced in this method
                connectEdgeToGraphRoot(makeEdge(makeNodeRef(expParent.tag), 
                                                makeNodeDecl("temp", "temp"),
                                                keys[values.indexOf(e)]), 
                                                singleValueGraph))
            , values), 
            // Finally, join all the Edges together
            // and change the first appearence of the Expression's Node
            // to NodeDecl
            (graphs: CompoundGraph[]): Result<CompoundGraph> =>
                bind(joinGraphsEdges(graphs), changeRootToNodeDecl)
            )
}

/*
    Signature: mapArraytoContent(arr, id)
    Type: [Exp[] * string -> Result<CompoundGraph>]
    Purpose: If an L4 Expression has an array parameter (like rands or body), we call this 
            method with that parameter. 
             we convert to Mermaid each of the elements in the array,
             then we create a psuedo-node with the label *id* (usually ":"),
             and connect it to each of the converted expressions
    Pre-conditions: true
*/
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

////////////////////////////////////////////////////
//  Utility Methods regarding Graph Manipulations
////////////////////////////////////////////////////

/*
    Signature: joinGraphsEdges(graphs)
    Type: [CompoundGraph[] -> Result<CompoundGraph>]
    Purpose: Given a list of CompoundGraphs, combine all the Edges arrays into
             a single array of Edges 
    Pre-conditions: true
*/
export const joinGraphsEdges = (graphs: CompoundGraph[]) : Result<CompoundGraph> =>
    makeOk(makeCompoundGraph(chain((g: CompoundGraph) : Edge[] => g.edges ,graphs)))

/*
    Signature: joinEdgeToGraph(edge, graph)
    Type: [Edge * CompoundGraph -> Result<CompoundGraph>]
    Purpose: Add the given Edge to the top of the graph's edges list
    Pre-conditions: true
*/
export const joinEdgeToGraph = (edge: Edge, graph: CompoundGraph) : Result<CompoundGraph> =>
    makeOk(makeCompoundGraph(chain((e: Edge[]) : Edge[] => e ,[[edge], graph.edges])))

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
    Signature: connectEdgeToGraphRoot(top, graph)
    Type: [Edge * GraphContent -> Result<CompoundGraphde>]
    Purpose: Takes an Edge and a Graph, and makes the edge point to the first 
             node of the graph
    Pre-conditions: true
*/
export const connectEdgeToGraphRoot = (top: Edge, graph: GraphContent): Result<CompoundGraph> =>
    // First, get a copy of the root
    bind(getGraphRoot(graph),
        (root: Node): Result<CompoundGraph> => 
            // if the root is a Node Ref, just add to the graph an Edge from top ==> root
            isNodeRef(root) ? 
                isCompoundGraph(graph) ? joinEdgeToGraph(makeEdge(top.from, root, top.label), graph) :
                isAtomicGraph(graph) ? makeOk(makeCompoundGraph([makeEdge(top.from, root, top.label)])) :    
                makeFailure("connectEdgeToGraphRoot: Not an option (1)"):
            
            // otherwise, if the root is NodeDecl
            isNodeDecl(root) ?
                // and the graph is Compound
                isCompoundGraph(graph) ? 
                    // We need to create a new Edge from top ==> root
                    // BUT the NodeDecl from root must be shifted to 
                    // the previous edge (which we are creating here)
                    joinEdgeToGraph(makeEdge(top.from, root, top.label), 
                                    makeCompoundGraph(chain((x)=>x, 
                                        [[makeEdge(makeNodeRef(root.id),
                                                   first(graph.edges).to,
                                                   first(graph.edges).label)],
                                          rest(graph.edges)]))) :
                // if the graph is a single node, just create an Edge
                isAtomicGraph(graph) ? makeOk(makeCompoundGraph([makeEdge(top.from, root, top.label)])) : 
                makeFailure("connectEdgeToGraphRoot: Not an option (2)"):
            makeFailure("connectEdgeToGraphRoot: Not an option (3)")
        )

/*
    Signature: changeRootToNodeDecl(graph)
    Type: [CompoundGraph -> Result<CompoundGraphde>]
    Purpose: Change the top Node to a NodeDecl
    Pre-conditions: true
*/
export const changeRootToNodeDecl = (graph: CompoundGraph): Result<CompoundGraph> => 
    safe2((root: Node, edges: Edge[]) => makeOk(makeCompoundGraph(chain((x)=>x, 
                                        [[makeEdge(makeNodeDecl(root.id, root.id),
                                                first(edges).to,
                                                first(edges).label)],
                                        rest(edges)]))))
    (getGraphRoot(graph), makeOk(graph.edges))

////////////////////////////////////////////////////
//  Utility Methods regarding Renaming Nodes
////////////////////////////////////////////////////

/*
    Signature: changeRootToNodeDecl(edges)
    Type: [Edge[] -> Result<Edge[]>]
    Purpose: Change the name of the nodes according to the Mermaid rules:
                - Each L4 Expression's Node has its own counter (and not a global counter).
                  Meaning, AppExp_i and ProcExp_j are incremented independently
                - Each NodeDecl increments the counter 
                - Each NodeRef gets the counter of the previous NodeDecl
    Pre-conditions: All NodeRefs refer to the previos NodeDecl in the graph!
*/
export const renameNodes = (edges: Edge[]): Result<Edge[]> => {
    const upperCaseFirstLetter = (s: string) : string => 
        s.charAt(0).toUpperCase() + s.substring(1)

    // Generically go over all the edges and get the Id's of the nodes
    const generateTypes = (edges: Edge[]): string[] => 
        union([], chain((e: Edge): string[] => [e.from.id, e.to.id], edges))
    
    // Rename the From node and To node
    const renameEdge = (e: Edge): Result<Edge> => 
        safe2((from: Node, to: Node) => makeOk(makeEdge(from, to, e.label)))
            (renameNode(e.from), renameNode(e.to))

    // Rename a single node according to the defined rules 
    const renameNode = (n: Node): Result<Node> => {
        const pos = types.indexOf(n.id);
        const varGen = varGens[pos]

        return  isNodeDecl(n) ? makeOk(makeNodeDecl(upperCaseFirstLetter(varGen(n.id, true)), n.label)) :
                isNodeRef(n) ? makeOk(makeNodeRef(upperCaseFirstLetter(varGen(n.id, false)))) :
                makeFailure("renameNode: Not an option")
    };

    const types = generateTypes(edges);

    // For each type create its OWN var generator (so it will get its own counter)
    const varGens = map((x: string): (v: string, inc: boolean) => string => 
                            makeVarGen(), types)

    return mapResult(renameEdge ,edges)
};

/*
    Signature: renameAtomicGraph(g)
    Type: [AtomicGraph -> Result<AtomicGraph>]
    Purpose: Just rename the single node inside the Atomic Graph
    Pre-conditions: true
*/
export const renameAtomicGraph = (g : AtomicGraph): Result<AtomicGraph> => 
    isNodeDecl(g) ? makeOk(makeNodeDecl(makeVarGen()(g.id, true), g.label)) :
    makeFailure("renameAtomicGraph: not an option")

/*
    Signature: makeVarGen(v, inc)
    Type: [string * boolean -> [string * boolean -> string]]
    Purpose: Create a variable changeing function
             if inc is true, it increments the counter
             if inc is false, it doesn't
    Pre-conditions: true
*/
export const makeVarGen = (): (v: string, inc: boolean) => string => {
    let count: number = 0;
    return (v: string, inc: boolean) => {
        inc ? count++ : count = count;
        return `${v}_${count}`;
    };
};

////////////////////////////////////////////////////
//  TODO: DELETE!
//////////////////////////////////////////////////// 
let x = (bind(parseL4("(L4 (define my-list '(1 2)))"),
(x: Parsed): Result<string> => bind(mapL4toMermaid(x), unparseMermaid)))
isOk(x) ? console.log(x.value) : console.log(x.message)
console.log("###########################################")

let y = parseL4("(L4 (lambda (x y)((lambda (x) (+ x y))(+ x x))1))");
isOk(y) ? console.log(JSON.stringify(y.value)) : console.log(y.message);
//(x: Parsed): Result<string> => bind(mapL4toMermaid(x), unparseMermaid)))
//isOk(x) ? console.log(x.value) : console.log(x.message)

console.log(
    //"*",
    //JSON.stringify(bind(parseL4("(L4 (+ 2 5) (/ 18 2))"),
    //    (x: Parsed): Result<Graph> => mapL4toMermaid(x))),
    //"*********************",
    //JSON.stringify(bind(parseL4Exp(["/", "18", "2"]),
    //    (x: Parsed): Result<Graph> => mapL4toMermaid(x))),
    //"*********************",
    //JSON.stringify(bind(parseL4("(L4 (lambda (x y)((lambda (x) (+ x y))(+ x x))1))"),
    //    (x: Parsed): Result<Graph> => mapL4toMermaid(x)))
    //JSON.stringify(bind(parseL4("(L4 (lambda (x) ((+ x x)) 1))"),
    //    (x: Parsed): Result<Graph> => mapL4toMermaid(x)))
    //JSON.stringify(bind(parseL4("(L4 (define my-list '(1 2)))"),
    //    (x: Parsed): Result<string> => bind(mapL4toMermaid(x), unparseMermaid))),
);