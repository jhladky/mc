FILES=ast.sml ast2Cfg.sml cfg.sml iloc.sml json2Ast.sml main.sml staticCheck.sml targetAmd64.sml cfg2Amd64.sml

all: mc

mc: $(FILES)
	mlton mc.mlb

clean:
	rm -f mc
