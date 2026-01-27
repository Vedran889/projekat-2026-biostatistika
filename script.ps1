param(
	[string]$Address,
	[string]$Commit="Inicijalni snimak"
)

# Get-ExecutionPolicy -Scope CurrentUser RemoteSigned
Set-StrictMode -Version Latest # “Be strict. Treat mistakes as errors instead of silently ignoring them.”
$ErrorActionPreference = "Stop"

if (-not $Address){
	throw "Unesite adresu do udaljenog repozitorijuma koristeci -Address flag."
}

if (-not (Test-Path ".git")){
	Write-Host "Initializing git repository..."
	git init	
}

Write-Host "Applying local git configuration..."

$configs = @{
    "user.name"="Vedran889"
    "user.email"="vedran.prodanovic03@gmail.com"
    "core.autocrlf"="false"
    "core.longpaths"="true"
    "init.defaultBranch"="master"
    "alias.ci"="!git commit -m"
    "alias.br"="branch"
    "alias.st"="status"
    "alias.co"="checkout"
    "alias.sw"="switch"
    "alias.brv"="!git branch -avv"
    "alias.rv"="!git remote -v"
    "alias.lg"="!git --no-pager log --oneline --graph --all --decorate=full --parents --shortstat --relative-date"
    "alias.add-remote"="!git remote add"
    "alias.rm-remote"="!git remote remove"
    "alias.change-to-main"="!git br -m master main"
    "alias.force-push"="!git push origin main --force"
    "alias.try-push"="!git push origin main --dry-run"
    "alias.vuci"="!git pull origin main"
    "alias.gurni"="!git push origin main"
    "alias.auh"="!git pull origin main --allow-unrelated-histories"

}
foreach($key in $configs.GetEnumerator()){
	git config --local  $key.Key $key.Value
} 
Write-Host "Git local configuration applied successfully"


$branch=git branch --show-current
if($branch -eq "master"){
	Write-Host "Changing branch name from master to main..."
	git change-to-main
}

if (-not (git remote | Select-String "^origin$")) {
    	Write-Host "Adding URL for remote repository..."
	    git add-remote origin $Address

}

 if (git remote | Select-String "^origin$") {
    	Write-Host "Pulling files from remote repository..."
        try{
            git vuci
        }
        catch [System.Management.Automation.NativeCommandError]{
                Write-Host "Bacen izuzetak NativeCommandError, nastavljamo sa radom..."
                
                
        }
	    

}

Write-Host "Adding files to be staged..."
git add .

Write-Host "Writing a commit..."
git ci "$Commit"

Write-Host "Pushing files to remote repository..."
    try{
          git gurni
    }
    catch [System.Management.Automation.NativeCommandError]{
          Write-Host "Bacen izuzetak NativeCommandError, nastavljamo sa radom..."
    }

Write-Host "Completed."

<# List all exceptions
[AppDomain]::CurrentDomain.GetAssemblies() |
    ForEach-Object { $_.GetTypes() } |
    Where-Object { $_ -is [System.Type] -and $_.IsSubclassOf([System.Exception]) } |
    Sort-Object FullName |
    Select-Object FullName
#>