# Research Compendium
Research Compendium内の各フォルダの機能を理解した上でファイルを保存してください。
- paper:卒論・修論用のRmdファイルが用意されています
 - paper_word:卒論・修論用のQmdファイルが用意されています
 - analysis：解析用ファイルを入れる用のフォルダです
- (analysis内) data：解析用データを入れるフォルダです
- (analysis内) function：解析用Rの関数を入れるフォルダです
- (analysis内) high_load：高負荷な解析で使う関数を入れるフォルダです
- materials：研究で使った材料を入れるフォルダです
- exercises：ゼミで行う演習課題用のフォルダです
- labnote：ラボノートを保管するフォルダです

# 日々の研究活動で使うRコマンド
## jsPsych課題作成テンプレートの準備
以下のように，set_cbat("認知課題名（英語）","jsPsychのバージョン")を実行すると，exerciseフォルダ内に指定した認知課題名のフォルダを作成し，必要なjsPsych関連ファイルがダウンロードされます。
``` r
psyinfr::set_cbat("stroop","7.1.1")
```
## 研究開始
その日の研究の開始時に以下の関数を実行すると，GitHubのリポジトリからプルを行った上で，ラボノートを作成します。ラボノートは自動で開きますので，適宜メモをとりながら研究を実施して，適宜knitをしてください。

``` r
psyinfr::researchIn()
``` 
## 研究終了

その日の研究の終了時に以下の関数を実行すると，ラボノートを保存した上で，変更加えたファイルにコミットを加えた上で，GitHubに自動的にプッシュします。

``` r
psyinfr::researchOut()
```
