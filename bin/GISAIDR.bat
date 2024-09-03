@echo off
SET batPath=%~dp0
SET binDir=%batPath:~0,-1%
::rem Execute the R script
Rscript %binDir%\GISAIDR %*
