import { Result, makeOk, makeFailure, bind, mapResult, safe2 } from "../shared/result";
import { reduce } from "ramda";

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
export type DirectionType = "LR" | "TD";

export interface Graph { tag: "Graph"; header: Header; content: GraphContent; }
export interface Header { tag: "Header"; dir: Dir; }
export interface Dir { tag: "Dir"; val: DirectionType ; }
export interface CompoundGraph { tag: "CompoundGraph"; edges: Edge[]; }

export interface Edge { tag: "Edge"; from: Node; to: Node; label?: string; }

export interface NodeDecl { tag: "NodeDecl"; id: string; label: string; }
export interface NodeRef { tag: "NodeRef"; id: string; }

// Type value constructors for disjoint types
export const makeGraph = (head: Header, content: GraphContent) : Graph => 
                                    ({tag: "Graph", header: head, content: content});
export const makeHeader = (dir: Dir) : Header => 
                                    ({tag: "Header", dir: dir});
export const makeDir = (val: DirectionType) : Dir => 
                                    ({tag: "Dir", val: val});
export const makeCompoundGraph = (edges: Edge[]) : CompoundGraph => 
                                    ({tag: "CompoundGraph", edges: edges});
export const makeEdge = (from: Node, to: Node, label?: string) : Edge =>
                                    ({tag: "Edge", from: from, to: to, label: label});
export const makeNodeDecl = (id: string, label: string) : NodeDecl => 
                                    ({tag: "NodeDecl", id: id, label: label});
export const makeNodeRef = (id: string) : NodeRef =>
                                    ({tag: "NodeRef", id: id});

// Type predicates for disjoint types
export const isGraph = (x: any): x is Graph => x.tag === "Graph";
export const isHeader = (x: any): x is Header => x.tag === "Header";
export const isDir = (x: any): x is Dir => x.tag === "Dir";
export const isCompoundGraph = (x: any): x is CompoundGraph => x.tag === "CompoundGraph";
export const isEdge = (x: any): x is Edge => x.tag === "Edge";
export const isNodeDecl = (x: any): x is NodeDecl => x.tag === "NodeDecl";
export const isNodeRef = (x: any): x is NodeRef => x.tag === "NodeRef";

export const isAtomicGraph = (x: any): x is AtomicGraph => isNodeDecl(x);
export const isGraphContent = (x: any): x is GraphContent => isAtomicGraph(x) || isCompoundGraph(x);