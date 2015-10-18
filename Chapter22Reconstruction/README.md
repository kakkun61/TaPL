# 『型システム入門』 演習 22.7.1 の解答

## これは何か

自然数とブール値をプリミティブに持ち let 多相を持つ型付きラムダ計算の型再構築器（型推論器）です。

## どう使うか

[`stack`](https://github.com/commercialhaskell/stack/releases) コマンドをインストールします。`stack setup` で初期化します。

REPL で使うことを想定しているのでこのディレクトリーで `stack ghci` コマンドで REPL を起動します。

関数の使い方はテスト（_test/ReconSpec.hs_）を参照してください。

## テスト

`stack test` でテストが実行されます。
