# ============ Config ============
$TargetFolder = "E:\Project\NDD-Brain-Atals\STseq\Step.ViewWeb\WT28M"
$FileTypes    = "*.png"
$ActionName   = "WT28M旋转90"
$ActionSet    = "NDD文章"
# ================================

$LogFile   = "$TargetFolder\_done.log"
$ErrorFile = "$TargetFolder\_error.log"

# Resume: load already-processed files
$done = @{}
if (Test-Path $LogFile) {
    Get-Content $LogFile | ForEach-Object { $done[$_] = $true }
    Write-Host "Resume: skipping $($done.Count) already-processed files" -ForegroundColor Cyan
}

Write-Host "Initializing Photoshop..." -ForegroundColor Cyan
$psApp = New-Object -ComObject Photoshop.Application
$psApp.DisplayDialogs = 3
$psApp.Preferences.RulerUnits = 1  # 1 = Pixels
$psApp.Visible = $true

Write-Host "Reading files..." -ForegroundColor Cyan
$files = Get-ChildItem -Path $TargetFolder -Recurse -Include $FileTypes
$total = $files.Count

if ($total -eq 0) {
    Write-Host "No files found!" -ForegroundColor Red
    Pause; Exit
}

$current   = 0
$skipped   = 0
$errors    = 0
$processed = 0
$startTime = $null   # 延迟到第一个文件实际处理时才开始计时

foreach ($file in $files) {
    $current++

    if ($done.ContainsKey($file.FullName)) {
        $skipped++
        continue
    }

    # 计算预计完成时间
    if ($processed -gt 0) {
        $elapsed    = (Get-Date) - $startTime
        $secPerFile = $elapsed.TotalSeconds / $processed
        $remaining  = $total - $current
        $etaTime    = (Get-Date).AddSeconds($remaining * $secPerFile)
        $etaStr     = " | 预计完成: " + $etaTime.ToString("HH:mm:ss")
    } else {
        $etaStr = ""
    }

    Write-Progress -Activity "Processing images" `
                   -Status "[$current/$total] $($file.Name)$etaStr" `
                   -PercentComplete ($current / $total * 100)

    $doc = $null
    if ($null -eq $startTime) { $startTime = Get-Date }  # 第一个文件开始处理时启动计时
    try {
        $doc = $psApp.Open($file.FullName)
        $psApp.DoAction($ActionName, $ActionSet)
        $doc.Save()
        $doc.Close(2)
        $doc = $null
        Add-Content -Path $LogFile -Value $file.FullName
        $processed++
        # 每 200 个文件清理一次 Photoshop 缓存，防止内存累积变慢
        if ($current % 200 -eq 0) { $psApp.Purge(4) }
    } catch {
        $msg = "$($file.FullName) | $($_.Exception.Message)"
        Write-Host "Error: $msg" -ForegroundColor Red
        Write-Host "检测到错误，强制终止 Photoshop 并重启..." -ForegroundColor Yellow

        # 强制结束所有残留 Photoshop 进程
        Get-Process -Name "Photoshop" -ErrorAction SilentlyContinue |
            Stop-Process -Force -ErrorAction SilentlyContinue
        Start-Sleep -Seconds 15   # 等待进程完全退出

        $psApp = New-Object -ComObject Photoshop.Application
        $psApp.DisplayDialogs = 3
        $psApp.Preferences.RulerUnits = 1
        $psApp.Visible = $true
        Start-Sleep -Seconds 8    # 等待 Photoshop 完全初始化

        Write-Host "Photoshop 已重启，重试当前文件..." -ForegroundColor Green

        # 重试当前文件一次
        try {
            $doc = $psApp.Open($file.FullName)
            $psApp.DoAction($ActionName, $ActionSet)
            $doc.Save()
            $doc.Close(2)
            $doc = $null
            Add-Content -Path $LogFile -Value $file.FullName
            $processed++
            Write-Host "重试成功: $($file.Name)" -ForegroundColor Green
        } catch {
            $msg = "$($file.FullName) | 重试失败: $($_.Exception.Message)"
            Write-Host "Retry Error: $msg" -ForegroundColor Red
            Add-Content -Path $ErrorFile -Value $msg
            $errors++
        } finally {
            if ($doc) { try { $doc.Close(2) } catch {} }
        }
    } finally {
        if ($doc) { try { $doc.Close(2) } catch {} }
    }
}

Write-Progress -Activity "Processing images" -Completed
try { $psApp.DisplayDialogs = 1 } catch { Write-Host "Photoshop 已关闭，跳过收尾设置" -ForegroundColor Yellow }

Write-Host ""
Write-Host "Done! Total: $total  Skipped: $skipped  Errors: $errors" -ForegroundColor Green
if ($errors -gt 0) {
    Write-Host "Error log: $ErrorFile" -ForegroundColor Red
}
Pause
