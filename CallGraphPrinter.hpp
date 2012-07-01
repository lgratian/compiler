// CallGraphPrinter.hpp
// Copyright (c) Lup Gratian
//
// Prints a call graph to a Graphviz DOT file.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_CALL_GRAPH_PRINTER_HPP
#define PC_ANALYSIS_CALL_GRAPH_PRINTER_HPP

#include "DotPrinterBase.hpp"
#include "CallGraph.hpp"
#include "../IR/Unit.hpp"
#include "../Base/String.hpp"
#include "../Base/SharedPointer.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class CallGraphPrinter : public DotPrinterBase {
private:
    FunctionReference* GetFunctionReference(Function* funct, Unit* unit) {
        auto functionType = unit->Types().GetPointer(funct->GetType());
        return unit->References().GetFunctionRef(funct, functionType);
    }

    void CreateLinks(CallNodeBase* node) {
        if(node->IsNodeGroup() == false) {
            auto callNode = static_cast<CallNode*>(node);

            callNode->ForEachCallSite([this, node](CallSite* site) -> bool {
                CallGraphPrinter *printer = this;
                printer->Link(node, site);

                site->ForEachCalledNode([printer, site](CallNodeBase* calledNode) -> bool {
                    printer->Link(site, calledNode);
                    return true;
                });

                return true;
            });
        }
    }

    void CreateLinks(ExternalUnknownCallNode* node) {
        node->ForEachCalledNode([this, node](CallNodeBase* calledNode) -> bool {
            Link(node, calledNode);
            return true;
        });
    }

public:
    CallGraphPrinter(const string& file) : DotPrinterBase(file) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    void Print(CallGraph* callGraph, Unit* unit) {
        // Print the external and unknown nodes.
        NewNode(ExternalCallNode::GetExternalNode(), "EXTERNAL", 
                COLOR_LIGHT_PINK, SHAPE_RECT);
        NewNode(UnknownCallNode::GetUnknownNode(), "UNKNOWN", 
                COLOR_LIGHT_GREEN, SHAPE_RECT);

        // Print the nodes for each function, then create the links.
        for(auto funct = unit->Functions().First(); funct; funct = funct->Next) {
            auto functionRef = GetFunctionReference(funct->Value, unit);
            auto node = callGraph->GetNode(functionRef);
            NewNode(node, *funct->Value->Name(), COLOR_LIGHT_BLUE, SHAPE_TRAPEZIUM);

            node->ForEachCallSite([this](CallSite* site) -> bool {
                string name = string::Format(L"%d", site->GetCallId());
                NewNode(site, name, COLOR_LIGHT_GREY, SHAPE_ELLIPSE);
                return true;
            });
        }

        auto& nodeGroups = callGraph->GetCallNodeGroups();

        for(int i = 0; i < nodeGroups.Count(); i++) {
            auto nodeGroup = nodeGroups[i];
            string name = string::Format(L"Group: %d", nodeGroup->NodeCount());
            NewNode(nodeGroup, name, COLOR_YELLOW, SHAPE_DIAMOND);

            nodeGroup->ForEachNode([this, nodeGroup](CallNodeBase* node) -> bool {
                Link(nodeGroup, node);
                return true;
            });
        }

        for(auto funct = unit->Functions().First(); funct; funct = funct->Next) {
            auto functionRef = GetFunctionReference(funct->Value, unit);
            auto node = callGraph->GetNode(functionRef);
            CreateLinks(node);
        }

        CreateLinks(ExternalCallNode::GetExternalNode());
        CreateLinks(UnknownCallNode::GetUnknownNode());
        Close();
    }
};

} // namespace Analysis
#endif