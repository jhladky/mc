MAIN_FILES=util.sml ast.sml ast2Iloc.sml cfg.sml iloc.sml json2Ast.sml main.sml static.sml symbolTable.sml targetAmd64.sml iloc2Amd64.sml unorderedSet.sml regAlloc.sml ilocUtil.sml ifeGraph.sml copyProp.sml

TEST_UNORDERED_SET_FILES=util.sml unorderedSet.sml testUnorderedSet.sml

compiler: $(MAIN_FILES)
	mlton compiler.mlb

testUnorderedSet: $(TEST_UNORDERED_SET_FILES)
	mlton testUnorderedSet.mlb

all: compiler


clean:
	rm -f compiler testUnorderedSet
