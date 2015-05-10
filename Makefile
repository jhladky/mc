MAIN_FILES=util.sml ast.sml ast2Iloc.sml cfg.sml iloc.sml json2Ast.sml main.sml static.sml symbolTable.sml targetAmd64.sml cfg2Amd64.sml unorderedSet.sml regAlloc.sml ilocUtil.sml ifeGraph.sml

TEST_UNORDERED_SET_FILES=cfg.sml targetAmd64.sml unorderedSet.sml testUnorderedSet.sml

mc: $(MAIN_FILES)
	mlton mc.mlb

testUnorderedSet: $(TEST_UNORDERED_SET_FILES)
	mlton testUnorderedSet.mlb

all: mc


clean:
	rm -f mc testUnorderedSet
