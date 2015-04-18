FILES=ast.sml ast2Cfg.sml cfg.sml iloc.sml json2Ast.sml main.sml printAst.sml printCfg.sml staticCheck.sml

all: mc

mc: $(FILES)
	mlton mc.mlb

clean:
	rm -f mc
