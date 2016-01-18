rem del ebin\*.beam
erl +P 102400 +K true -smp auto -s make all -s c q
pause
