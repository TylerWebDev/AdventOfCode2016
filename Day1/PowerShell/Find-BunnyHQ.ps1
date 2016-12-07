$scriptdir = split-path -parent $MyInvocation.MyCommand.Definition
$directions = (Get-Content $scriptdir\input.txt).Replace(' ',"").Split(',')
$global:heading = "N"
$global:blocks = 0

function Go($heading,$blocks) {
    $global:heading = $heading
    $global:blocks += $blocks
}

function TurnAndWalk($direction) {
    $turn = $direction[0]
    $steps = [int]$direction.Substring(1)
    switch($heading) {
        "N" {
            if($turn -eq "L") {
                Go "W" -$steps
            }
            else {
                Go "E" $steps
            }
        }
        "S" {
            if ($turn -eq "L") {
                Go "E" $steps
            }
            else {
                Go "W" -$steps
            }
        }
        "E" {
            if ($turn -eq "L") {
                Go "N" $steps
            }
            else {
                Go "S" -$steps
            }
        }
        "W" {
            if ($turn -eq "L") {
                Go "S" -$steps
            }
            else {
                Go "N" $steps
            }
        }
    }
}

foreach ($direction in $directions) {
    TurnAndWalk($direction)
}

Write-Host You are $global:blocks away.