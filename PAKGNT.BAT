XM COBOL pakoLAG   omf(gnt);
XM COBOL pakoVORL  omf(gnt);
XM COBOL pakoSTAM  omf(gnt);
XM COBOL pakoANZ   omf(gnt);
XM COBOL pakoERF   omf(gnt);
XM COBOL pakoJOUR  omf(gnt);
XM COBOL pakoLIS   omf(gnt);
XM COBOL pakoBIL   omf(gnt);
XM COBOL pakoKRED  omf(gnt);
XM COBOL parKOMKA  omf(gnt);
XM COBOL panARTAN  omf(gnt);
XM COBOL panANZ    omf(gnt);
XM COBOL parLGCV   omf(gnt);
del *.csi
del *.int
del *.idy
XM RUN library PAKOLAG.LBT
del *.g??
pak a /exe a:\pakLBR pakolag.lbr
@echo .. es folgt copy auf E:\
pause
COPY PAKOLAG.LBR e:\
