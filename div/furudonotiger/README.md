# FurudonoTiger compiler
Modern compiler imprementation in ML に沿ってコンパイラを実装する。
**Standar ML** で記述。in ML.

進捗 : 4章. 意味解析 (抽象構文木の生成)まで実装できた（つもり）

## 実行方法

1. このディレクトリで Standard ML New J.. のREPLを起動
1. `CM.make "sources.cm"` をREPLで実行
    - 今の時点ではパーサが手に入る
    - ファイルを開いて、それを解析し、ASTをプリントするSMLの関数がREPLのスコープに現れるということ
1. `Parse.parse "test.tig"` をREPLで実行
   - `Parse.parser`が例の関数

今はここまで。
これから意味解析に取り掛かる。

## ファイルの説明

## ビルドの設定ファイル
- sources.cm : よくわかんないけど、勘で書いている。教科書は古くて所々違う。仕様書を読んで頑張る。仕様書は優しい。
  
### Lexical Analysis
- tiger.lex : 字句解析器の記述。 ML-Lexで処理する。

### Parsing
- tiger.grm : 構文解析の記述  ML-Yaccで処理する。
- absyn.sml : 抽象構文木の定義

### Semantic Analysis
#### Symbol Table
コンパイラでは識別子を型や値に対応させる。その対応を **symbol table**で表現する。
- symbol を使う (宣言ではない出現** ときにsymbol tableを参照する。
- local variable にはscopeがある。TigerでのscopeはSMLと似たようなノリで決まる。

#### Environment
- **Set of bindings** として定義。教科書では\sigma がメタ変数として使われる。
- envとtableは似たような概念。気持ちとしてはenvが抽象的で、tableがデータ構造な感じ？
- `X + Y`はenv X を env Yで拡張するの意味。シンボルに重複があればYが優先される。
- 実行時には今のenvみたいなものがあって、プログラムの意味に応じて環境が更新される。

#### 実装方法
##### 命令的なスタイル
`current-table`みたいなglobal varを用意する。スコープの変化に応じてそれを破壊的に更新する。
スコープを抜けるときに前の状態に戻せるように工夫する。(**undo stack**を用いる。)
- シンボルが環境に追加されるときは:
  1. global な環境にbinding を追加する
  2. undo stack にも同様に追加する
- スコープを抜けるときは:
  1. undo スタックを巻き戻す
  2. 巻き戻して得られた環境をglobalな環境として設定する


##### 関数型なスタイル
永続的なデータ構造を用いる。
二分木を使うやり方が紹介されている。

#### Symbol モジュール
テーブルへのアクセスで文字列比較をするのはバカらしいので、識別子の内部表現として **symbol**を用いる。
De Bruijn index みたいなノリか。違いは以下の通り:
- De Bruijn だと"symbol"から名前を復元できない。
- De Bruijn だとsymbol 空間がコンパクト目になりそう。
- De Bruijn だとindexが0始まりの自然数になるので、そのままデータ構造アクセスに使えそう。最適化として有用かはわからないけど。

Symbol の実装に対する要件は以下の通り
- 等価性の比較
- hash-keyを高速に生成できる
- 全順序が入っている
