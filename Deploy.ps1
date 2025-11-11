param(
    [string]$AppPoolName = "UserManagement",                # Nom du pool d'applications
    [string]$SourceDll   = "C:\Users\danyleblanc\Documents\DL-Projets\FeedFlow\Win64\Release\FeedFlow.dll",   # DLL compilée
    [string]$TargetDll   = "C:\inetpub\wwwroot\scripts\FeedFlow.dll" # DLL déployée
)

Write-Host "=== Déploiement ISAPI DLL ==="

# 1. Arrêter le pool d'applications
Write-Host "Arrêt du pool d'applications $AppPoolName..."
Stop-WebAppPool -Name $AppPoolName

# 2. Attendre que le pool soit vraiment arrêté
do {
    Start-Sleep -Seconds 1
    $state = (Get-WebAppPoolState -Name $AppPoolName).Value
    Write-Host "État actuel du pool: $state"
} while ($state -ne "Stopped")

# 3. Copier la nouvelle DLL
Write-Host "Copie de la nouvelle DLL..."
Copy-Item $SourceDll $TargetDll -Force

# 4. Redémarrer le pool d'applications
Write-Host "Redémarrage du pool d'applications $AppPoolName..."
Start-WebAppPool -Name $AppPoolName

Write-Host "Déploiement terminé avec succès ✅"
