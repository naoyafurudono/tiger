# FurudonoTiger compiler
Modern compiler imprementation in ML に沿ってコンパイラを実装する。
**Standar ML** で記述。in ML.

進捗 : 4章. 意味解析 (抽象構文木の生成)まで実装できた（つもり）

## 実行方法

1. このディレクトリで Standard ML New J.. のREPLを起動
1. `CM.make "sources.cm"` をREPLで実行
    - 今の時点ではパーサが手に入る
    - ファイルを開いて、それを解析し、ASTをプリントするSMLの関数がREPLのスコープに現れるということ
1. `Parser.parse "test.tig"` をREPLで実行
   - `Parser.parser`が例の関数

今はここまで。

## ファイルの説明

## ビルドの設定ファイル
- sources.cm : よくわかんないけど、勘で書いている。教科書は古くて、所々違う。仕様書を読んで頑張る。仕様書は優しい。
  
### 字句解析
- tiger.lex : 字句解析器の仕様？ ML-Lexで処理する。

### 構文解析
- tiger.grm : 構文解析の記述  ML-Yaccで処理する。
- absyn.sml : 抽象構文木の定義
- 