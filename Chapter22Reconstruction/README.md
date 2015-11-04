# 『型システム入門』 演習 22.7.1 の解答

## これは何か

自然数とブール値をプリミティブに持ち let 多相を持つ型付きラムダ計算の型再構築器（型推論器）です。

## 使い方

`recon` プログラムは標準入力から式を受け取り標準出力に対応する型を出力します。

## 文法

受け取る式の文法は下記の通りです。`t` は項、`x` は変数です。TaPL のものとの違いは、今のところ変数は自然数（0 を含む）の十進数表記です。気が向いたら単語にします。ラムダ抽象は `\` でも書けます。

```
t := x
     zero
     succ t
     pred t
     true
     false
     iszero t
     if t then t else t
     \x. t
     λx. t
     t t
     let x = t in t
     (t)

x := 0, 1, 2, …
```

型は下記の通りです。`recon` プログラムではプリティプリンターを実装していないので ADT がそのまま出力されます。

```
T := X
     Nat
     Bool
     T → T
     ∀X Y … Z. T
```

## ビルド

[`stack`](https://github.com/commercialhaskell/stack/releases) コマンドをインストールします。`stack setup` で初期化します。

`stack build` でビルドします。出力されるファイルの場所は標準出力に表示されます。

## テスト

`stack test` でテストが実行されます。
