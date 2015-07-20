# Kirabern 言語仕様
Tiger 言語リファレンスマニュアルを参考に言語仕様をまとめておきます。

## 字句について
### 識別子(id)
`[a-zA-Z_]` からはじまり、 `[a-zA-Z0-9_]` が続くもの。

### コメント
`//` から行の最後まで。

または `/*` から `*/` まで。入れ子にはできない。

### キーワード
while, for, to, break, var, fn, type, if, else, null

## 分離記号
, : ; ( ) [ ] { } . + - * / = == != < <= > >= && || ->

## 宣言
### 型宣言(tydec)
* type id = ty

隣り合う型宣言は相互に再帰することができる。

### ty
* tyid （型に別名をつける）
* { tyfields } （レコード）

### tyid
* id
* tyid[] （配列を表す）

### tyfields
* id : tyid { , id : tyid }

### 変数(vardec)
* var id = exp ;
* var id : tyid = exp ;

### 関数(fundec)
* fn id ( tyfields ) exp （戻り値なし）
* fn id ( tyfields ) -> tyid exp

隣り合う関数は相互に再帰することができる。

## 変数と式
### 左辺値(lvalue)
* id
* lvalue . id
* lvalue [ exp ]

### 式(exp)
* lvalue
* null （レコード値 null）
* { exp; exp; ... exp } （最後の値を返す）
* { exp; exp; ... exp; } （値を返さない）
* [0-9]+
* \- 記号
* "文字列" （エスケープは [F#](https://msdn.microsoft.com/ja-jp/library/dd323829.aspx) と同じ）
* id ( ) （関数呼び出し）
* id ( exp { , exp } )
* 算術演算子 +, -, \*, /
* 比較演算子 ==, !=, >, <, >=, <=
* 条件演算子 &&, ||
* new tyid { id = exp { , id = exp } } （レコード生成）
* new tyid [ exp ] （配列生成）
* lvalue = exp
* if ( exp ) exp
* if ( exp ) exp else exp
* while ( exp ) exp
* for ( id = exp to exp ) exp
* break
* ( exp )
* 宣言

## プログラム
* exp; exp; ... exp;
