# hml
ML風の関数型言語をMIPSアセンブラに変換するコンパイラです  
プログラムの大部分はMiniML(<http://esumii.github.io/min-caml/>)を参考にしています。

## インストールの方法と使い方
インストールにはstackが必要です  
stackのインストールについては<https://docs.haskellstack.org/en/stable/README/>を参照してください  
インストールは以下のコマンドでできます  

```shell
git clone git@github.com:akawashiro/hml.git
cd hml
stack build
```
arith.mlをコンパイルして結果をarith.asmに保存する例です  
```shell
stack exec hml-exe -- arith.ml > arith.asm
```
spimを使うとアセンブラの実行結果を確認できます
```shell
spim -file arith.asm
```
-dオプションでソースコードをMIPSアセンブラに変換する過程をすべて見ることができます
```shell
stack exec hml-exe -- -d arith.ml
```

## プログラムの解説
このコンパイラはML言語のひとつの式を受け取って、対応するMIPSアセンブラを出力します  
コンパイラの内部処理は以下の流れに沿って行われます
1. 構文解析(src/Parse.hs)  
Parsecを使ってML言語を構文木に変換します。
1. アルファ変換(src/Alpha.hs)  
すべての変数名を一意なものに付け替えます  
プログラムの意味は全く変わりませんが、この後の処理を簡単にするために行っています。
1. クロージャ変換(src/Closure.hs)  
ネストした関数定義をすべてトップレベルに持ち上げるために、ローカル変数を引数として受け取るように変更します
1. K正規化(src/KNormal.hs)  
1 + 2*3 などのアセンブラで実現できない複雑な演算を  
```haskell
x = 2 * 3
x + 1
```
などの簡単な形に分解します
1. フラット化(src/Flat.hs)  
すべての関数定義をトップレベルに持ち上げます
1. MIPSの擬似アセンブラに変換
1. 関数呼び出しを処理
1. レジスタ割当
1. スタックの確保と割当
