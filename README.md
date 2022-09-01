# read-book

しばらくlispファイルがscriptとしてなぜか動かなくなっていので、
rosscriptに書き直したりしました。


このプログラムは日本語と英語が混ざっている文章をそれぞれ、きれいな日本語と英語で読み上げるために開発しました。

文章での説明よりも実際に動いている様子を見てもらった方がわかりやすいかもしれません。


[![Common Lisp READ-BOOK ver3.4 日本語英語混成文読み上げ装置](http://img.youtube.com/vi/L-Wyjoe8--k/0.jpg)](https://www.youtube.com/watch?v=L-Wyjoe8--k)

*ver3では自動的にHTML化して英単語上にマウスを持って行くと単語の意味が表示されるようにしました。（後に廃止）

*また英語の意味の表示回数を指定出来るようにしました。

*ver3.3ではしおり機能をつけました。読みかけてた位置から読み上げを再開出来るようにしました。

sbclを使っているため、他の処理系には対応していません。

festival,open-jtalkといった音声合成装置のインストール

あとはopen-jtalkに関してはシェル上からojtalkでワンライナー実行出来るように設定する必要があります。

ver3.5では表示された内容に関してファイルに書き込む機能を追加しようと考えています。
←リダイレクトコマンドで対応する事にした。その際、(setf cl-rainbow:*enabled* nil) に切り替えておく必要あり。

# 使い方
```shell
$ ros install biofermin2/read-book
$ cd ~/.roswell/local-projects/biofermin2/read-book/roswell/
$ ros build read-book.ros
$ mv read-book ~/.roswell/bin/
$ read-book <file>
```

※最近sbcl ver2以上にあげてからはそのままscriptファイルとして実行出来なくなりました。
  roswellでscriptを書き直す必要がありそうです。→書き直しました。[2022-08-07]
