import { isGraphContent, isEdge, NodeDecl, isAtomicGraph, GraphContent, Graph, makeGraph, makeDir, makeCompoundGraph, Edge, makeEdge, CompoundGraph, Node, makeNodeDecl, AtomicGraph, isNodeDecl, makeNodeRef, isCompoundGraph, isNodeRef, unparseMermaid } from "./mermaid-ast"
import { AtomicExp, VarDecl, isVarDecl, isCompoundExp, CompoundExp, isAtomicExp, Parsed, Exp, isProgram, Program,  isExp, isDefineExp, DefineExp, CExp, isCExp, parseL4, parseL4Exp } from "./L4-ast"
import { isOk, Result, makeOk, makeFailure, bind, mapResult, safe2, isFailure } from "../shared/result";
import { first, rest, allT } from "../shared/list"
import { concat, union, chain, map, zip, KeyValuePair, filter, is, reduce } from "ramda";
import { isArray, isNumber, isString, isBoolean } from "util";
import { SExpValue, isSymbolSExp, isEmptySExp, EmptySExp, SymbolSExp, CompoundSExp, isCompoundSExp } from "./L4-value"

export const L4toMermaid = (concrete: string): Result<string> =>
    (bind(parseL4(concrete),
        (p: Program): Result<string> =>
            bind(mapL4toMermaid(p), unparseMermaid)))

export const mapL4toMermaid = (exp: Parsed): Result<Graph> => 
    isProgram(exp) ? mapProgramtoMermaid(exp) : 
    isExp(exp) ? mapExpToMermaid(exp) :
    makeFailure("mapL4toMermaid: Not an option")


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
                            makeOk(makeGraph(makeDir("TD"), unitedGraph)))))
}
    


export const mapExpToMermaid = (exp: Exp): Result<Graph> => 
    makeFailure("mapExpToMermaid: Not implemented")


export const mapExptoContent = 
    (exp: Exp | CompoundSExp | VarDecl | SymbolSExp | EmptySExp | 
          number  | boolean | string | Exp[],
     expId: string,
     forbbidenIds: string[]): Result<GraphContent> => {
        console.log("=== mapExptoContent ===")
        console.log("expId: ", expId)
        console.log("forbbidenIds: ", forbbidenIds)
         return isDefineExp(exp) ? mapCompoundExptoContent(exp, expId, forbbidenIds) :
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
         makeFailure(`mapExptoContent: Unknown Expression: ${exp}`) 
     }
    


export const mapAtomictoContent = (exp: AtomicExp | VarDecl | SymbolSExp, 
                                  expId: string): Result<AtomicGraph> => 
    Object.values(exp).length === 2 ? 
    makeOk(makeNodeDecl(expId,`${exp.tag}(${Object.values(exp)[1]})`)) :
    makeFailure("mapAtomictoContent: Atomic Expression with more than 2 keys")


export const mapAtomicValuesToContent = (exp: number | string | boolean, expId: string): Result<AtomicGraph> =>
    makeOk(makeNodeDecl(expId,`${typeof(exp)}(${exp})`))


export const mapEmptyExpressionsToContent = (exp: EmptySExp, expId: string): Result<AtomicGraph> =>
    makeOk(makeNodeDecl(expId,`${exp.tag}`))

export const mapCompoundExptoContent = (exp: Exp | CompoundSExp | Exp[], 
                                        expId: string, 
                                        forbbidenIds: string[]): Result<CompoundGraph> => {
    console.log("=== mapCompoundExptoContent === ")
    console.log("exp", JSON.stringify(exp))
    console.log("expId: ", expId)
    console.log("forbbidenIds: ", forbbidenIds)
    // Here we take all the expression parameters and values generically
    // if exp is not an array, means it is an Compound(S)Exp
    // if exp is an array, it was a parameter of a previous Compound(S)Exp
    const keys = !isArray(exp) ? rest(Object.keys(exp)) : [];        
    console.log("keys: ", keys)
    const values = !isArray(exp) ? rest(Object.values(exp)) : exp;    
    console.log("values: ", JSON.stringify(values))
    // Extract all the values names
    const valuesTags = map((v):string => "" === extractTag(v)
                                            ? keys[values.indexOf(v)] // in case of array
                                            : extractTag(v), values);
    console.log("valuesTags: ", valuesTags)
    // Rename all values names according to restrictions given
    const childrenIds = renameVars(valuesTags, forbbidenIds); 
    console.log("childrenIds: ", childrenIds)
    // Convert each child from left to right
    // after converting each one, take all the NodeIds from its graph
    // and pass them as forbbiden to the next child
    // each value is now a graph content, all of them in one array
    return bind(convertValues(values, expId, childrenIds, union(childrenIds, forbbidenIds)), 
            // Now we create each child an Edge from expId --> NodeDecl(child)
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
                        // if we got atomic graph, 
                        //make ExpId_X --> |key[child]|ChildId_X[Child(Value)]
                        isAtomicGraph(gc) ? 
                            makeOk(makeEdge(
                                    makeNodeRef(expId),
                                    gc,
                                    keys[childGraphs.indexOf(gc)])) : 
                        // if we got a CompoundGraph,
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

    }

export const convertValues = (exps: Exp[], expId: string, childrenIds: string[], forbbidenIds: string[]): Result<GraphContent[]> => {
    console.log("=== convertValues ===")
    console.log("expId: ", expId)
    console.log("childrenIds: ", childrenIds)
    // Reduce is going through the exps array, convert each child to a GraphContent
    // and join them all into one array
    return reduce((convertedExps: Result<GraphContent[]>, e: Exp): Result<GraphContent[]> =>
        !isOk(convertedExps) ? convertedExps :
        // First, extract all the node ids from the previous edges
        // join them to the given childrenIds and pass them forward as forbbidenNames
        bind(makeOk(union(childrenIds, extractNodesIdsFromContents(convertedExps.value))),
                (forbbidenNames: string[]): Result<GraphContent[]> => {
                    console.log(">>> converting: ", JSON.stringify(e))
                    console.log(">>> will give him the id: ", childrenIds[exps.indexOf(e)])
                    console.log(">>> forbbidenNames: ", union(forbbidenNames, forbbidenIds))
                    // Now, convert the single exp using its given id
                    return bind(mapExptoContent(e, childrenIds[exps.indexOf(e)], union(forbbidenNames, forbbidenIds)), 
                        (g: GraphContent): Result<GraphContent[]> => 
                            // return to reduce the united array of all the prevs and this one
                            makeOk(union(convertedExps.value, [g])))
                        })
            
        , makeOk([]), exps)
}
    

////////////////////////////////////////////////////
//  Utility Methods regarding Graph Manipulations
////////////////////////////////////////////////////
export const joinGraphsEdges = (graphs: GraphContent[]) : Result<CompoundGraph> => 
    // If the content is compound, take its edges
    // otherwise, it is not neccessary, because the parent created an edge for it
    makeOk(makeCompoundGraph(chain((g: GraphContent) : Edge[] => 
                                isCompoundGraph(g) ? g.edges : []
                            ,graphs)))


////////////////////////////////////////////////////
//  Utility Methods regarding Renaming Nodes
////////////////////////////////////////////////////

export const renameVars = (vars: string[], forbbidenNames: string[]): string[] => {
    // make sure vars is unique
    const setOfVars = union([], vars)

    // For each type create its OWN var generator (so it will get its own counter)
    const varGens = map((x: string): (v: string, inc: boolean) => string => 
                            makeVarGen(), setOfVars)

    // Helper function to match UpperCase Mermaid convention
    const upperCaseFirstLetter = (s: string) : string => 
        s.charAt(0).toUpperCase() + s.substring(1)
    
    const renameVar = (s: string): string => {
        const pos = setOfVars.indexOf(s);
        const varGen = varGens[pos];
        // try to rename
        const tempName = upperCaseFirstLetter(varGen(s, true));
        // if the name is in the forbbidenNams, try again
        const newName = forbbidenNames.indexOf(tempName) !== -1 ? renameVar(s) : tempName
        return newName;
    }

    return map(renameVar, vars);
};

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

//let x = (bind(parseL4("(L4 (+ 2 5))"),
//let x = (bind(parseL4("(L4 (define my-list '(1 2)))"),
//let x = (bind(parseL4("(L4 (+ (/ 18 2) 5))"),
//let x = (bind(parseL4("(L4 (+ (/ 18 2) (* 7 17)))"),
//let x = (bind(parseL4("(L4 (lambda (x y)((lambda (x) (+ x y))(+ x x))1))"),
//(x: Parsed): Result<string> => bind(mapL4toMermaid(x), unparseMermaid)))
//isOk(x) ? console.log(x.value) : console.log(x.message)

let x = L4toMermaid("(L4 (lambda (x y)((lambda (x) (+ x y))(+ x x))1))")
isOk(x) ? console.log(x.value) : undefined