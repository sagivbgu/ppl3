import { Result, makeOk, makeFailure, bind, mapResult, safe2, safe3, isOk } from "../shared/result";
import { isEmpty, map, chain, reduce } from "ramda";

/*
    <graph> ::= <header> <graphContent>     // Graph(dir: Dir, content: GraphContent)
    <header> ::= graph (TD|LR)<newline>     // Direction can be TD or LR
    <graphContent> ::= <atomicGraph> | <compoundGraph>
    <atomicGraph> ::= <nodeDecl>
    <compoundGraph> ::= <edge>+

    <edge> ::= <node> --><edgeLabel>? <node><newline> // <edgeLabel> is optional
                                                      // Edge(from: Node, to: Node, label?: string)

    <node> ::= <nodeDecl> | <nodeRef>
    <nodeDecl> ::= <identifier>["<string>"] // NodeDecl(id: string, label: string)
    <nodeRef> ::= <identifier>              // NodeRef(id: string)
    <edgeLabel> ::= |<identifier>|          // string
*/

export type GraphContent = AtomicGraph | CompoundGraph;
export type Node = NodeDecl | NodeRef;
export type AtomicGraph = NodeDecl;

export interface Graph { tag: "Graph"; dir: Dir; content: GraphContent; }
export interface Header { tag: "Header"; dir: Dir; }
export interface Dir { tag: "Dir"; val: string; }
export interface CompoundGraph { tag: "CompoundGraph"; edges: Edge[]; }

export interface Edge { tag: "Edge"; from: Node; to: Node; label?: string; }

export interface NodeDecl { tag: "NodeDecl"; id: string; label: string; }
export interface NodeRef { tag: "NodeRef"; id: string; }
export interface EdgeLabel {tag: "EdgeLabel"; label: string; }

// Type value constructors for disjoint types
export const makeGraph = (dir: Dir, content: GraphContent) : Graph => 
                                    ({tag: "Graph", dir: dir, content: content});
export const makeHeader = (dir: Dir) : Header => 
                                    ({tag: "Header", dir: dir});
export const makeDir = (val: string) : Dir => 
                                    ({tag: "Dir", val: val});
export const makeCompoundGraph = (edges: Edge[]) : CompoundGraph => 
                                    ({tag: "CompoundGraph", edges: edges});
export const makeEdge = (from: Node, to: Node, label?: string) : Edge =>
                                    ({tag: "Edge", from: from, to: to, label: label});
export const makeNodeDecl = (id: string, label: string) : NodeDecl => 
                                    ({tag: "NodeDecl", id: id, label: label});
export const makeNodeRef = (id: string) : NodeRef =>
                                    ({tag: "NodeRef", id: id});
export const makeEdgeLabel = (label: string) : EdgeLabel =>
                                    ({tag: "EdgeLabel", label: label});

// Type predicates for disjoint types
export const isGraph = (x: any): x is Graph => x.tag === "Graph";
export const isHeader = (x: any): x is Header => x.tag === "Header";
export const isDir = (x: any): x is Dir => x.tag === "Dir";
export const isCompoundGraph = (x: any): x is CompoundGraph => x.tag === "CompoundGraph";
export const isEdge = (x: any): x is Edge => x.tag === "Edge";
export const isNodeDecl = (x: any): x is NodeDecl => x.tag === "NodeDecl";
export const isNodeRef = (x: any): x is NodeRef => x.tag === "NodeRef";
export const isEdgeLabel = (x: any): x is EdgeLabel => x.tag === "EdgeLabel";

export const isAtomicGraph = (x: any): x is AtomicGraph => isNodeDecl(x);

// ==========================================================================
// Unparse: Map an AST to a concrete syntax string.


export const unparseMermaid = (g: Graph): Result<string> =>
    bind(unparseGraphContent(g.content), 
        (contentStr: string): Result<string> =>
            makeOk(`graph ${g.dir.val}${contentStr}`))

export const unparseGraphContent = (gc: GraphContent): Result<string> =>
    isCompoundGraph(gc) ? unparseCompoundGraph(gc) :
    isAtomicGraph(gc) ? unparseNode(gc) :
    makeFailure("unparseGraphContent: Not an option")

export const unparseCompoundGraph = (g: CompoundGraph): Result<string> =>
    bind(mapResult(unparseEdge, g.edges), 
        (edgeStrs: string[]): Result<string> =>
            makeOk(reduce(concatLines,"" , edgeStrs)))

export const concatLines = (strA: string, strB: string): string =>
    strA + '\n\t' + strB

export const unparseEdge = (edge: Edge): Result<string> =>
    safe2((fromNode: string, toNode: string): Result<string> => 
        
        edge.label !== undefined ? 
            makeOk(`${fromNode} -->|${edge.label}|${toNode}`) :
        makeOk(`${fromNode} -->${toNode}`))
        
        (unparseNode(edge.from), (unparseNode(edge.to))) 

export const unparseNode = (node: Node): Result<string> =>
    isNodeRef(node) ? makeOk(`${node.id}`) :
    isNodeDecl(node) ? makeOk(`${node.id}["${node.label}"]`) :
    makeFailure("unparseNode: not an option")