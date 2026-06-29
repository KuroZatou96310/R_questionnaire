
# Linux側からRでデータベースの中身を見る。
# コンソールにコード入力して確認して。

library(DBI)
library(RSQLite)

con <- dbConnect(
    SQLite(),
    "/srv/shiny-server/enquete_app_data/questionnaire.db"
)


# 文字コード変更
Sys.setlocale("LC_CTYPE", "C.UTF-8")


# テーブル一覧
dbListTables(con)

# テーブルの中身を確認
dbReadTable(con, "answers")

# クエリを実行して、テーブルの中身を確認
dbGetQuery(con, "SELECT * FROM answers LIMIT 10")

# テーブルの構造を確認
dbGetQuery(con, "PRAGMA table_info(answers)")





# データベース接続を閉じる
dbDisconnect(con)