$ErrorActionPreference = 'Stop'
$ppt = New-Object -ComObject PowerPoint.Application
$pres = $ppt.Presentations.Add()
$pres.PageSetup.SlideWidth = 960
$pres.PageSetup.SlideHeight = 540
$slide = $pres.Slides.Add(1, 12)

function Rgb([int]$r,[int]$g,[int]$b) { return $r + ($g -shl 8) + ($b -shl 16) }
$navy = Rgb 34 64 112
$blue = Rgb 53 106 230
$pale = Rgb 239 246 255
$ink = Rgb 23 32 51
$muted = Rgb 69 82 108
$white = Rgb 255 255 255

function AddText($text,[double]$x,[double]$y,[double]$w,[double]$h,[double]$size,[int]$color,[bool]$bold=$false,[int]$align=1) {
  $box = $slide.Shapes.AddTextbox(1,$x,$y,$w,$h)
  $box.TextFrame2.TextRange.Text = $text
  $box.TextFrame2.TextRange.Font.Name = 'Yu Gothic'
  $box.TextFrame2.TextRange.Font.NameFarEast = 'Yu Gothic'
  $box.TextFrame2.TextRange.Font.Size = $size
  $box.TextFrame2.TextRange.Font.Fill.ForeColor.RGB = $color
  $box.TextFrame2.TextRange.Font.Bold = $bold
  $box.TextFrame2.TextRange.ParagraphFormat.Alignment = $align
  $box.TextFrame2.VerticalAnchor = 3
  return $box
}

function AddCard([double]$x,[double]$y,[double]$w,[double]$h,[string]$num,[string]$title,[string]$body) {
  $card = $slide.Shapes.AddShape(5,$x,$y,$w,$h)
  $card.Fill.ForeColor.RGB = $pale
  $card.Line.ForeColor.RGB = Rgb 184 208 245
  $card.Line.Weight = 1.2
  $circle = $slide.Shapes.AddShape(9,$x+9,$y+8,28,28)
  $circle.Fill.ForeColor.RGB = $blue
  $circle.Line.Visible = 0
  AddText $num ($x+9) ($y+7) 28 28 18 $white $true 2 | Out-Null
  AddText $title ($x+44) ($y+8) ($w-52) 25 15 $navy $true 1 | Out-Null
  AddText $body ($x+14) ($y+43) ($w-28) ($h-50) 11 $ink $false 1 | Out-Null
}

$header = $slide.Shapes.AddShape(5,12,10,936,58)
$header.Fill.ForeColor.RGB = $navy
$header.Line.Visible = 0
AddText '▮▮▮  アンケート分析画面の機能紹介' 32 16 520 42 25 $white $true 1 | Out-Null
AddText 'アンケートの回答結果を、さまざまな切り口で分析できます' 560 17 360 35 13 $white $false 2 | Out-Null

AddCard 12 82 245 160 '1' 'アンケートID・パスワードで分析' "アンケートIDとパスワードを入力すると、`nそのアンケートの分析が行えます。"
AddCard 12 255 245 115 '5' '回答条件の絞り込み' "特定の条件に絞って、`nその結果だけを分析できます。"
AddCard 12 383 245 130 '6' 'CSVでデータ出力' "分析結果のデータを`nCSVファイルでダウンロードできます。"
AddCard 756 82 192 130 '2' '単純集計' "設問ごとの回答分布や、`n基本的な統計量を確認できます。"
AddCard 756 222 192 170 '3' 'クロス集計' "複数の設問を組み合わせて、`n回答の傾向を比較できます。"
AddCard 756 402 192 111 '4' '数値分析' "数値型の設問について、`n相関や分布などを確認できます。"

$imgMain = 'C:\Users\BlacK\AppData\Local\Temp\codex-clipboard-c7f2977b-075f-46d3-a63c-e074ca986524.png'
$imgHeat = 'C:\Users\BlacK\AppData\Local\Temp\codex-clipboard-c534bf66-c191-4722-a752-c4ae21a34434.png'
$imgScatter = 'C:\Users\BlacK\AppData\Local\Temp\codex-clipboard-9615766d-9b56-472b-afb2-c6e262985d73.png'
$slide.Shapes.AddPicture($imgMain, 0, -1, 272, 82, 455, 430) | Out-Null
$slide.Shapes.AddPicture($imgHeat, 0, -1, 782, 296, 143, 84) | Out-Null
$slide.Shapes.AddPicture($imgScatter, 0, -1, 782, 445, 143, 63) | Out-Null

$out = Join-Path $PSScriptRoot '..\docs\questionnaire_analytics_feature_intro_editable.pptx'
if (Test-Path $out) { Remove-Item $out -Force }
$pres.SaveAs($out,24)
$pres.Close(); $ppt.Quit()
[Runtime.InteropServices.Marshal]::ReleaseComObject($slide) | Out-Null
[Runtime.InteropServices.Marshal]::ReleaseComObject($pres) | Out-Null
[Runtime.InteropServices.Marshal]::ReleaseComObject($ppt) | Out-Null
Write-Output $out

