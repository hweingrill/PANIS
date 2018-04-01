xm cobol PANLFDRU omf (gnt);
xm cobol PANBS    omf (gnt);
del *.int
del *.i?0
del *.idy
del *.csi
xm run library PANBS.LBT
pak a /exe a:panBS PANBS.LBR
