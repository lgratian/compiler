// DotPrinterBase.hpp
// Copyright (c) Lup Gratian
//
// Implements basic support for generating Graphviz DOT files.
//---------------------------------------------------------------------------------------
#ifndef PC_ANALYSIS_DOT_PRINTER_BASE_HPP
#define PC_ANALYSIS_DOT_PRINTER_BASE_HPP

#include "../Base/String.hpp"
#include "../Base/Dictionary.hpp"
#include <fstream>
using namespace Base;

namespace Analysis {

class DotPrinterBase {
protected:
	Dictionary<const void*, int> objectToNode_;
	std::wofstream fout_;
	bool closed_;
	int nodes_;

public:
	static const char* COLOR_WHITE;
	static const char* COLOR_BLACK;
	static const char* COLOR_GREY;
	static const char* COLOR_LIGHT_GREY;
	static const char* COLOR_RED;
	static const char* COLOR_DARK_RED;
	static const char* COLOR_LIGHT_RED;
	static const char* COLOR_BLUE;
	static const char* COLOR_DARK_BLUE;
	static const char* COLOR_LIGHT_BLUE;
	static const char* COLOR_YELLOW;
	static const char* COLOR_DARK_YELLOW;
	static const char* COLOR_GREEN;
	static const char* COLOR_DARK_GREEN;
	static const char* COLOR_LIGHT_GREEN;
	static const char* COLOR_PINK;
	static const char* COLOR_LIGHT_PINK;

	static const char* SHAPE_RECT;
	static const char* SHAPE_SQUARE;
	static const char* SHAPE_CIRCLE;
	static const char* SHAPE_ELLIPSE;
	static const char* SHAPE_DIAMOND;
	static const char* SHAPE_TRAPEZIUM;
	static const char* SHAPE_HEXAGON;
	static const char* SHAPE_PARALLELOGRAM;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	DotPrinterBase(const string& file) : 
            nodes_(0), fout_(file.Chars()), closed_(false) {
		fout_<<"digraph {\n";
	}

	virtual ~DotPrinterBase() {
		Close();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Creates a node for an object, having the specified attributes.
	// If the node was already created nothing is done.
	void NewNode(const void* object, const string& text, 
                 const char* color = COLOR_LIGHT_GREY,
				 const char* shape = SHAPE_RECT) {
		if(objectToNode_.ContainsKey(object) == false) {
			int n = nodes_++;
			objectToNode_.Add(object, n);
			fout_<<"n"<<n<<"[shape="<<shape<<", style=filled, ";
			fout_<<"color="<<color<<", label=\""<<text.Chars()<<"\"];\n";
		}
	}

	// Creates a link between the specified object.
	void Link(const void* a, const void* b, const string& label = L"", 
			  const char* color = COLOR_BLACK, bool dashed = false) {
		int na = objectToNode_[a];
		int nb = objectToNode_[b];
		fout_<<"n"<<na<<" -> "<<"n"<<nb;
		fout_<<"[label=\""<<label.Chars()<<"\", fontsize=12, labeldistance=10";
		if(dashed) fout_<<", style=dotted, penwidth=2";
		else fout_<<", penwidth=2";
		fout_<<", color="<<color<<"];\n";
	}

	// Closes the associated file.
	void Close() {
		if(closed_ == false) {
			fout_<<"}\n";
			fout_.flush();
			fout_.close();
			closed_ = true;
		}
	}
};

} // namespace Analysis
#endif