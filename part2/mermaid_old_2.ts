import { isGraphContent, isEdge, NodeDecl, isAtomicGraph, GraphContent, Graph, makeGraph, makeDir, makeCompoundGraph, Edge, makeEdge, CompoundGraph, Node, makeNodeDecl, AtomicGraph, isNodeDecl, makeNodeRef, isCompoundGraph, isNodeRef, unparseMermaid } from "./mermaid-ast"
import { VarDecl, isVarDecl, isCompoundExp, CompoundExp, isAtomicExp, Parsed, Exp, isProgram, Program,  isExp, isDefineExp, DefineExp, CExp, isCExp, parseL4, parseL4Exp } from "./L4-ast"
import { isOk, Result, makeOk, makeFailure, bind, mapResult, safe2, isFailure } from "../shared/result";
import { first, rest, allT } from "../shared/list"
import { concat, union, chain, map, zip, KeyValuePair, filter, is, reduce } from "ramda";
import { isArray, isNumber, isString, isBoolean } from "util";
import { SExpValue, isSymbolSExp, isEmptySExp, EmptySExp, SymbolSExp, CompoundSExp, isCompoundSExp } from "./L4-value"

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
export const mapProgramtoMermaid = (program: Program): Result<Graph> => {
    console.log("=== mapProgramtoMermaid ===")
    const keys = Object.keys(program);
    const values = Object.values(program);
    
    // Collect the tags from the all the children
    const valuesTags = union([program.tag], 
                        map((v):string => "" === extractTag(v) ? 
                                            keys[values.indexOf(v)] 
                                            : extractTag(v), rest(values)));
    

    // rename all the ids
    const nodesIds = renameVars(valuesTags, [])
    console.log("KEYS: ", JSON.stringify(keys));
    console.log("VALUES: ", JSON.stringify(values));
    console.log("VALUESTAGS: ", JSON.stringify(valuesTags));
    console.log("RENAMEDVARS: ", JSON.stringify(nodesIds));
    console.log("FORBBIDEN: ",  JSON.stringify(nodesIds))

    return bind(mapCompoundExptoContent(program, nodesIds[0], nodesIds), 
            (proGraph: CompoundGraph): Result<Graph> =>
                bind(changeRootToNodeDecl(proGraph),
                    (finalProGraph: CompoundGraph): Result<Graph> =>
                            makeOk(makeGraph(makeDir("TD"), finalProGraph))))
}

/*
    Signature: mapExpToMermaid(exp)
    Type: [Exp -> Result<Graph>]
    Purpose: Convert a L4 Exp expression AST to a Mermaid graph AST
    Pre-conditions: true
*/
export const mapExpToMermaid = (exp: Exp): Result<Graph> => {
    const keys = Object.keys(exp);
    const values = Object.values(exp);
    
    // Collect the tags from the all the children
    const valuesTags = map((v):string => "" === extractTag(v) ? keys[values.indexOf(v)] 
                                              : extractTag(v)
        , values);
    
    // rename all the ids
    const nodesIds = renameVars(valuesTags, [])

    return bind(mapExptoContent(exp, nodesIds[0], nodesIds), 
            (expGraph: GraphContent): Result<Graph> =>
                    makeOk(makeGraph(makeDir("TD"), expGraph)))
}

/*
    Signature: mapExptoContent(exp)
    Type: [* -> Result<GraphContent>]
    Purpose: Convert a L4 Exp expression AST to a Mermaid GraphContent,
             Atomic (single Node) or Compund (list of Edges) 
    Pre-conditions: true
*/
export const mapExptoContent = (exp: Exp    | CompoundSExp  | 
                                    VarDecl | SymbolSExp    | EmptySExp | 
                                    number  | boolean       | string, 
                                expId: string,
                                forbbidenIds: string[]): 
                                        Result<GraphContent> => {
    console.log("=== mapExptoContent ===");
    console.log("expId: ", JSON.stringify(expId));
    console.log("FORBBIDEN: ",  JSON.stringify(forbbidenIds))
    // Compound Expressions (and special forms)
    return isCExp(exp) ? mapCExptoContent(exp, expId, forbbidenIds) : 
    isDefineExp(exp) ? mapCompoundExptoContent(exp, expId, forbbidenIds) :
    isCompoundSExp(exp) ? mapCompoundExptoContent(exp, expId, forbbidenIds) :

    // Atomic Expressions
    isAtomicExp(exp) ? mapAtomictoContent(exp, expId) :
    isVarDecl(exp) ? mapAtomictoContent(exp, expId) :
    isSymbolSExp(exp) ? mapAtomictoContent(exp, expId) :

    // Atomic Values
    isNumber(exp) ? mapAtomicValuesToContent(exp, expId) :
    isString(exp) ? mapAtomicValuesToContent(exp, expId) :
    isBoolean(exp) ? mapAtomicValuesToContent(exp, expId) :

    // Empty Values
    isEmptySExp(exp) ? mapEmptyExpressionsToContent(exp, expId) :
    makeFailure(`mapExptoContent: Not supporting ${exp}`)
}

/*
    Signature: mapCExptoContent(exp)
    Type: [CExp -> Result<GraphContent>]
    Purpose: Convert a L4 CExp expression AST to a Mermaid GraphContent,
             Atomic (single Node) or Compund (list of Edges) 
    Pre-conditions: true
*/
export const mapCExptoContent = (exp: CExp, expId: string, forbbidenIds: string[]): Result<GraphContent> => {
    console.log("=== mapCExptoContent ===");
    console.log("expId: ", JSON.stringify(expId));
    console.log("FORBBIDEN: ",  JSON.stringify(forbbidenIds))
    return isAtomicExp(exp) ? mapAtomictoContent(exp, expId) :
    isCompoundExp(exp) ? mapCompoundExptoContent(exp, expId, forbbidenIds):
    makeFailure("mapCExptoMermaid Not an option");
}

/*
    Signature: mapAtomictoContent(exp)
    Type: [CExp -> Result<AtomicGraph>]
    Purpose: Convert a L4 atomic expression AST to a Mermaid GraphContent,
             Atomic (single Node) or Compund (list of Edges) 
    Pre-conditions: exp is atomic and has only 2 values!
*/
export const mapAtomictoContent = (exp: CExp | VarDecl | SymbolSExp, 
                                  expId: string): Result<AtomicGraph> => {
    console.log("=== mapAtomictoContent ===");
    console.log("expId: ", JSON.stringify(expId));
    // Known atomic expression has only two values {tag: X, val/var/op: Y }
    return Object.values(exp).length === 2 ? 
    makeOk(makeNodeDecl(expId,`${exp.tag}(${Object.values(exp)[1]})`)) :
    makeFailure("mapAtomicFirstTimetoMermaid: more than 2 keys")
                                  }

/*
    Signature: mapAtomicValuesToContent(exp)
    Type: [number | string | boolean -> Result<AtomicGraph>]
    Purpose: Convert an atomic value to an AtomicGraph
    Pre-conditions: exp must be an atomic value
*/
export const mapAtomicValuesToContent = (exp: number | string | boolean, expId: string): Result<AtomicGraph> => {
    console.log("=== mapAtomicValuesToContent ===");
    console.log("expId: ", JSON.stringify(expId));
    return makeOk(makeNodeDecl(expId,`${typeof(exp)}(${exp})`))
}

/*
    Signature: mapEmptyExpressionsToContent(exp)
    Type: [EmptySExp -> Result<AtomicGraph>]
    Purpose: Convert an empty atomic expression value to an AtomicGraph
    Pre-conditions: exp must be an atomic value
*/
export const mapEmptyExpressionsToContent = (exp: EmptySExp, expId: string): Result<AtomicGraph> => {
    console.log("=== mapEmptyExpressionsToContent ===");
    console.log("expId: ", JSON.stringify(expId));
    return makeOk(makeNodeDecl(expId,`${exp.tag}`))
}
/*
    Signature: mapCompoundExptoContent(expParent)
    Type: [Exp | Program -> Result<CompoundGraph>]
    Purpose: Convert recursively a Non-Atomic L4 Expression to a CompoundGraph.
             This method is generic, it doesn't care about the specific type of expParent.
    Pre-conditions: expParent is Non Atomic L4 Expression
*/

export const mapCompoundExptoContent = (exp: Exp | Program | CompoundSExp, 
                                        expId: string, 
                                        forbbidenIds: string[]): Result<CompoundGraph> => {
    console.log("=== mapCompoundExptoContent ===");
    console.log("expId: ", JSON.stringify(expId));
    console.log("FORBBIDEN_GIVEN: ",  JSON.stringify(forbbidenIds))                          
    const keys = rest(Object.keys(exp));
    const values = rest(Object.values(exp));
    
    // Collect the tags from the all the children
    const valuesTags = map((v):string => "" === extractTag(v) ? keys[values.indexOf(v)] 
                                              : extractTag(v), values);

    // rename all the children ids
    
    const childrenNodesIds = renameVars(valuesTags, forbbidenIds)
    console.log("KEYS: ", JSON.stringify(keys));
    console.log("VALUES: ", JSON.stringify(values));
    console.log("VALUESTAGS: ", JSON.stringify(valuesTags));
    console.log("RENAMEDVARS: ", JSON.stringify(childrenNodesIds));
    console.log("FORBBIDEN_NEW: ",  JSON.stringify(union(forbbidenIds, childrenNodesIds)))

    // Happy flow: After getting the parsed children and the children Edges
    //      * fix the edge for each children (basically, deal with the possibility of an 
    //        AtomicGraph Child)
    //      * combine all the edges of the children
    //      * put on top of these edges the children edges
    return safe2((childrenParsed: GraphContent[], childrenEdges: Edge[]): Result<CompoundGraph> =>
        bind(mapResult(fixChildEdge, zip(childrenEdges, childrenParsed)), 
                (fixedChildrenEdges: Edge[]): Result<CompoundGraph> =>
                    bind(joinGraphsEdges(childrenParsed), 
                            (unitedChildren: CompoundGraph): Result<CompoundGraph> => 
                                joinGraphsEdges([makeCompoundGraph(fixedChildrenEdges), 
                                                 unitedChildren]))))

    // First we convert each child (stored in values)
    (mapResult((e: Exp | Exp[]): Result<GraphContent> =>
        // if the value is an array, call mapArraytoContent
        isArray(e) ? mapArraytoContent(e, 
                     childrenNodesIds[values.indexOf(e)], 
                     union(forbbidenIds, childrenNodesIds)) :
        // otherwise
        mapExptoContent(e, 
                        childrenNodesIds[values.indexOf(e)], 
                        union(forbbidenIds, childrenNodesIds)), values), 

    // Second we create Edge connecting exp to each child
    bind(mapResult((key: string): Result<Edge> => {
        //console.log(">>>>>>>", `key:${key} `, `v[k]:${JSON.stringify(values[keys.indexOf(key)])}`)
        return makeOk(makeEdge(makeNodeRef(expId), 
                               makeNodeDecl(childrenNodesIds[keys.indexOf(key)], 
                                            values[keys.indexOf(key)].tag), key))
    }, keys), 
        // Third, for each edge we created, if the value is an array
        // change the label to ":"
        (edges: Edge[]): Result<Edge[]> =>
            mapResult((e: Edge): Result<Edge> => {
                //console.log("===", JSON.stringify(e), "===");
                return !isString(e.label) ? makeOk(e) :
                isArray(values[keys.indexOf(e.label)]) ? 
                    makeOk(makeEdge(e.from, makeNodeDecl(e.to.id, ":"), e.label)) : 
                makeOk(e)
            } ,edges)))
}


export const mapArraytoContentNew = (arr: Exp[], arrId: string, forbbidenIds: string[]) : Result<CompoundGraph> => {
    console.log("=== mapArraytoContent ===");
    console.log("arrId: ", JSON.stringify(arrId));
    console.log("FORBBIDEN_GIVEN: ",  JSON.stringify(forbbidenIds))                          
    const valuesTags = map(extractTag, arr);
    const childrenNodesIds = renameVars(valuesTags, forbbidenIds);

    console.log("VALUESTAGS: ", JSON.stringify(valuesTags));
    console.log("RENAMEDVARS: ", JSON.stringify(childrenNodesIds));
    console.log("FORBBIDEN_NEW: ",  JSON.stringify(union(forbbidenIds, childrenNodesIds)))

    // Happy flow: After getting the parsed children and the children Edges
    //      * fix the edge for each children (basically, deal with the possibility of an 
    //        AtomicGraph Child)
    //      * combine all the edges of the children
    //      * put on top of these edges the children edges
    return safe2((childrenParsed: GraphContent[], childrenEdges: Edge[]): Result<CompoundGraph> =>
        bind(mapResult(fixChildEdge, zip(childrenEdges, childrenParsed)), 
                (fixedChildrenEdges: Edge[]): Result<CompoundGraph> =>
                    bind(joinGraphsEdges(childrenParsed), 
                            (unitedChildren: CompoundGraph): Result<CompoundGraph> => 
                                joinGraphsEdges([makeCompoundGraph(fixedChildrenEdges), 
                                                unitedChildren]))))

        // First we convert each child in arr
        (mapResult((e: Exp): Result<GraphContent> => 
            mapExptoContent(e, 
                            childrenNodesIds[arr.indexOf(e)], 
                            union(forbbidenIds, childrenNodesIds)), arr), 

        // Second we take all the tags of the Exps in arr
        // and then create a NodeDecl for them
        bind(mapResult((nodeId: string): Result<NodeDecl> => 
            makeOk(makeNodeDecl(nodeId, valuesTags[childrenNodesIds.indexOf(nodeId)])) , childrenNodesIds), 
            // then we create an Edge from arrNode[":"] --> child
            (nodes: NodeDecl[]): Result<Edge[]> => 
                mapResult((n: NodeDecl): Result<Edge> => 
                    makeOk(makeEdge(makeNodeRef(arrId), n)) , nodes)))
}

export const mapArraytoContent = (arr: Exp[], arrId: string, forbbidenIds: string[]) : Result<CompoundGraph> => {
    console.log("=== mapArraytoContent ===");
    console.log("arrId: ", JSON.stringify(arrId));
    console.log("FORBBIDEN_GIVEN: ",  JSON.stringify(forbbidenIds))                          
    const valuesTags = map(extractTag, arr);
    const childrenNodesIds = renameVars(valuesTags, forbbidenIds);

    console.log("VALUESTAGS: ", JSON.stringify(valuesTags));
    console.log("RENAMEDVARS: ", JSON.stringify(childrenNodesIds));
    console.log("FORBBIDEN_NEW: ",  JSON.stringify(union(forbbidenIds, childrenNodesIds)))

    const childrenNodeDecls = mapResult((nodeId: string): Result<NodeDecl> => 
        makeOk(makeNodeDecl(nodeId, valuesTags[childrenNodesIds.indexOf(nodeId)])) , childrenNodesIds);

    const childrenEdges = isFailure(childrenNodeDecls) ? makeFailure("") :
        mapResult((n: NodeDecl): Result<Edge> => 
            makeOk(makeEdge(makeNodeRef(arrId), n)) , childrenNodeDecls.value)
    
    const childrenEdgesFakeGraphContent = isFailure(childrenEdges) ? makeFailure("") :
            !isArray(childrenEdges.value) ? makeFailure("") :
            !allT(isEdge, childrenEdges.value) ? makeFailure("") :
            makeOk(makeCompoundGraph(childrenEdges.value))
    

    const convertedChildren = !isOk(childrenEdgesFakeGraphContent) ? makeFailure("") :
    !isGraphContent(childrenEdgesFakeGraphContent.value) ? makeFailure("") :
    convertValues(arr, arrId, childrenNodesIds, [childrenEdgesFakeGraphContent.value])

    return !isOk(convertedChildren) ? makeFailure("") :
    !isArray(convertedChildren.value) ? makeFailure("") :
    !allT(isGraphContent, convertedChildren.value) ? makeFailure("") :
    isFailure(childrenEdges) ? makeFailure("") :
    !isArray(childrenEdges.value) ? makeFailure("") :
    !isArray(convertedChildren.value) ? makeFailure("") :
    !allT(isEdge, convertedChildren.value) ? makeFailure("") :
    bind(mapResult(fixChildEdge, zip(childrenEdges.value, convertedChildren.value)), 
            (edges: Edge[]): Result<CompoundGraph> => joinGraphsEdges(makeCompoundGraph(convertedChildren.value)))
    )
}

////////////////////////////////////////////////////
//  Utility Methods regarding Graph Manipulations
////////////////////////////////////////////////////
export const changeRootToNodeDecl = (graph: CompoundGraph): Result<CompoundGraph> => 
    safe2((root: Node, edges: Edge[]) => makeOk(makeCompoundGraph(chain((x)=>x, 
                                        [[makeEdge(makeNodeDecl(root.id, root.id),
                                                first(edges).to,
                                                first(edges).label)],
                                        rest(edges)]))))
    (getGraphRoot(graph), makeOk(graph.edges))

/*
    Signature: joinGraphsEdges(graphs)
    Type: [CompoundGraph[] -> Result<CompoundGraph>]
    Purpose: Given a list of CompoundGraphs, combine all the Edges arrays into
             a single array of Edges 
    Pre-conditions: true
*/
export const joinGraphsEdges = (graphs: GraphContent[]) : Result<CompoundGraph> => 
    makeOk(makeCompoundGraph(chain((g: GraphContent) : Edge[] => 
                                        isCompoundGraph(g) ? g.edges : []
                                    ,graphs)))


export const convertValues = (exps: Exp[], expId: string, childrenIds: string[], init: GraphContent[]): Result<GraphContent[]> => {
    console.log("=== convertValues === ");
    console.log("expId: ", expId);
    console.log("EXPS: ", JSON.stringify(exps));
    console.log("GRAPHCONTENTS: ", JSON.stringify(init))
    return reduce((convertedExps: Result<GraphContent[]>, e: Exp): Result<GraphContent[]> =>
            isFailure(convertedExps) ? convertedExps :
            bind(makeOk(extractNodesIdsFromContents(convertedExps.value)),
                    (forbbidenNames: string[]): Result<GraphContent[]> => 
                        bind(mapExptoContent(e, childrenIds[exps.indexOf(e)], forbbidenNames), 
                            (g: GraphContent): Result<GraphContent[]> => 
                                makeOk(union(convertedExps.value, [g]))))
                
            , makeOk(init), exps)
}


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

export const fixChildEdge = (pair: KeyValuePair<Edge, GraphContent>): Result<Edge> =>
    // First, get a copy of the root
    bind(getGraphRoot(pair[1]),
        (root: Node): Result<Edge> => 
            // if the root is a Node Ref, the prev Edge was fine because it declares the child
            isNodeRef(root) ? makeOk(pair[0]) :
            // otherwise, if the root is NodeDecl
            isNodeDecl(root) ? makeOk(makeEdge(pair[0].from, root, pair[0].label)) :
            makeFailure("fixChildEdge: Not an option"))

////////////////////////////////////////////////////
//  Utility Methods regarding Renaming Nodes
////////////////////////////////////////////////////

/*
    Signature: renameNodes(edges)
    Type: [Edge[] -> Result<Edge[]>]
    Purpose: Change the name of the nodes according to the Mermaid rules:
                - Each L4 Expression's Node has its own counter (and not a global counter).
                  Meaning, AppExp_i and ProcExp_j are incremented independently
                - Each NodeDecl increments the counter 
                - Each NodeRef gets the counter of the previous NodeDecl
    Pre-conditions: All NodeRefs refer to the previos NodeDecl in the graph!
*/
export const renameVars = (vars: string[], forbbidenNames: string[]): string[] => {
    const setOfVars = union([], vars)

    // For each type create its OWN var generator (so it will get its own counter)
    const varGens = map((x: string): (v: string, inc: boolean) => string => 
                            makeVarGen(), setOfVars)

    const upperCaseFirstLetter = (s: string) : string => 
        s.charAt(0).toUpperCase() + s.substring(1)
    
    const renameVar = (s: string): string => {
        const pos = setOfVars.indexOf(s);
        const varGen = varGens[pos];
        const tempName = upperCaseFirstLetter(varGen(s, true));
        console.log(`\trenameVars: tempName=${tempName} forbbiden: ${forbbidenNames.indexOf(tempName)}`)
        const newName = forbbidenNames.indexOf(tempName) !== -1 ? renameVar(s) : tempName
        console.log(`\trenameVars: newName=${tempName}`)
        return newName;
    }

    return map(renameVar, vars);
};

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

export const extractTag = (x: Exp | SExpValue) : string =>
        isExp(x) ? x.tag :
        isSymbolSExp(x) ? x.tag :
        isEmptySExp(x) ? x.tag :
        isCompoundSExp(x) ? x.tag : 
        isNumber(x) ? "number" :
        isString(x) ? "string" :
        isBoolean(x) ? "boolean" : ""

export const extractNodesIdsFromEdges = (edges: Edge[]) : string[] =>
    union(map((e: Edge): string => e.from.id, edges), 
          map((e: Edge): string => e.to.id, edges))

export const extractNodesIdsFromContents = (contents: GraphContent[]) : string[] =>
    chain((x: string[]): string[] => x, 
        map((g: GraphContent): string[] => 
            isCompoundGraph(g) ? extractNodesIdsFromEdges(g.edges) : [g.id] ,contents))

////////////////////////////////////////////////////
//  TODO: DELETE!
//////////////////////////////////////////////////// 

//let v = makeVarGen2();
//console.log(v("AppExp_2", true));

//let x = (bind(parseL4("(L4 (define my-list '(1 2)))"),
//let x = (bind(parseL4("(L4 (+ 2 5))"),
//let x = (bind(parseL4("(L4 (+ (/ 18 2) 5))"),
let x = (bind(parseL4("(L4 (+ (/ 18 2) (* 7 17)))"),
//let x = (bind(parseL4("(L4 (lambda (x y)((lambda (x) (+ x y))(+ x x))1))"),
(x: Parsed): Result<string> => bind(mapL4toMermaid(x), unparseMermaid)))
isOk(x) ? console.log(x.value) : console.log(x.message)