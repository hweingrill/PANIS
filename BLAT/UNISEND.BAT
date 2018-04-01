CD ..
@if not exist "TIDESADV.26" goto NIXIS
copy TIDESADV.26 DESADV\UN121231.000
copy TIDESADV.26 DESUNII.121231
del TIDESADV.26
CD BLAT
blat -of UNIDES.OF -s DESADV -body "DESADV fÅr Lieferung 31.12.12" -attach N:\TIFAKT\DESUNII.121231 -server mail01.titz.at -f office@titz.at -log uni.log  
del ..\desunii.*
goto ENDE
:NIXIS
@echo.
@echo       DESADV UNIMARKT (26) nicht vorhanden
@echo.
@pause
:ENDE
