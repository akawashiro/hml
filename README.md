# hml
ML風の関数型言語のMIPSコンパイラです。だいたいMinCaml<http://esumii.github.io/min-caml/>のHaskellクローンです。

## プログラムの概観
構文解析→アルファ変換→クロージャ変換→K正規化→フラット化→MIPSの擬似アセンブラに変換→関数呼び出しを処理→レジスタ割当

## ghc-mod を走らせるためにはdist/setup-configを削除する
