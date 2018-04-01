XM COBOL panarc    omf(gnt);
XM COBOL panarch   omf(gnt);
del *.csi
del *.int
del *.idy
XM RUN library PANARC.LBT
del *.g??
pak a /exe a:\paCLBR panarc.lbr
@echo .. es folgt copy auf E:\
pause
COPY PANARC.LBR e:\
