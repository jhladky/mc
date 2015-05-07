FILES=util.sml ast.sml ast2Iloc.sml cfg.sml iloc.sml json2Ast.sml main.sml static.sml symbolTable.sml targetAmd64.sml cfg2Amd64.sml unorderedSet.sml regAlloc.sml ilocUtil.sml

all: mc

mc: $(FILES)
	mlton mc.mlb

clean:
	rm -f mc
