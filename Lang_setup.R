install.packages("showtext")




library(showtext)

font_add(
  "jp",
  "/usr/share/fonts/opentype/noto/NotoSerifCJK-Bold.ttc"
)

showtext_auto()


par(family = "jp")




print("日本語フォントを設定しました。")


