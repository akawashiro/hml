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
2. アルファ変換(src/Alpha.hs)  
すべての変数名を一意なものに付け替えます  
プログラムの意味は全く変わりませんが、この後の処理を簡単にするために行っています。
3. クロージャ変換(src/Closure.hs)  
ネストした関数定義をすべてトップレベルに持ち上げるために、ローカル変数を引数として受け取るように変更します
4. K正規化(src/KNormal.hs)  
1 + 2 * 3 などのアセンブラで実現できない複雑な演算を一時変数を用いて簡単な形に変換します
5. フラット化(src/Flat.hs)
すべての関数定義をトップレベルに持ち上げます  
6. MIPSの擬似アセンブラに変換(src/Declare.hs)  
プログラムを構文木から擬似アセンブラに変換します  
この擬似アセンブラではレジスタの割当やスタックに関する処理が省略されています
7. 関数呼び出しを処理(src/Call.hs)  
関数の呼び出しをジャンプ命令で置き換えます
またメイン関数のreturn命令をMIPSのsyscallの一つであるprintで置き換えます
8. レジスタ割当(src/RegisterAllocate.hs)  
アルファ変換で命名した変数名をレジスタ名で置き換えます  
使用するレジスタ数を最小にするためにレジスタをできるだけ再利用するようにしています
9. スタックの確保と割当(src/Stack.hs)
関数ごとにスタックのサイズを計算し、スタックを確保する命令を挿入します  
関数を呼び出すとき、システムコールを呼び出すときにレジスタをスタックに退避します
