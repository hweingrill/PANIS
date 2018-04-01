xm /p2 +jqkr cobol PANIS    omf (gnt);
xm /p2 +jqkr cobol PANANZ   omf (gnt);
xm /p2 +jqkr cobol PANARTAN omf (gnt);
xm /p2 +jqkr cobol PANDAU   omf (gnt);
xm /p2 +jqkr cobol PANDEBAN omf (gnt);
xm /p2 +jqkr cobol PANLAG   omf (gnt);
xm /p2 +jqkr cobol PANLFDRU omf (gnt);
xm /p2 +jqkr cobol PANLIEF  omf (gnt);
xm /p2 +jqkr cobol PANLIST  omf (gnt);
xm /p2 +jqkr cobol PANRECH  omf (gnt);
xm /p2 +jqkr cobol PANRAST  omf (gnt);
xm /p2 +jqkr cobol PANSTAM  omf (gnt);
xm /p2 +jqkr cobol PANTUED  omf (gnt);
xm /p2 +jqkr cobol PANVORL  omf (gnt);
xm /p2 +jqkr cobol PANBSKOM omf (gnt);
xm /p2 +jqkr cobol PANARCH  omf (gnt);
xm /p2 +jqkr cobol PAREZ    omf (gnt);
xm /p2 +jqkr cobol PARVORL  omf (gnt);
xm /p2 +jqkr cobol PARDRU   omf (gnt);
xm /p2 +jqkr cobol PARHINW  omf (gnt);
xm /p2 +jqkr cobol PARKOMKA omf (gnt);
xm /p2 +jqkr cobol PAKOKRED omf (gnt);
xm /p2 +jqkr cobol PAKOANZ  omf (gnt);
xm /p2 +jqkr cobol PAREZANL omf (gnt);
xm /p2 +jqkr cobol PARCONV  omf (gnt);
del *.int
del *.i?0
del *.idy
del *.csi
run library PAREZ.LBT
run library PANIS.LBT
del *.g*
pak a /exe a:panpar p*.lbr
@echo .. es folgt copy auf E:\
pause
COPY PA*.LBR e:\
