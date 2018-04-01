XM cobol PAREZ    omf(gnt);
XM cobol PANANZ   omf(gnt);
XM cobol PARVORL  omf(gnt);
XM cobol PARDRU   omf(gnt);
XM cobol PARHINW  omf(gnt);
XM cobol PARKOMKA omf(gnt);
XM cobol PAREZANL omf(gnt);
XM cobol PARLGKOR omf(gnt);
XM cobol PAKOKRED omf(gnt);
XM cobol PAKOANZ  omf(gnt);
del *.int
del *.idy
del *.csi
XM RUN library PAREZ.LBT
pak a /exe a:PARLBR PAREZ.LBR
@echo .. es folgt copy auf E:\
pause
COPY PAREZ.LBR e:\
