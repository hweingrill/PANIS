xm +jqr cobol BERICHT  omf(gnt);
del *.int
del *.idy
del *.csi
xm +jqr RUN library BERICHT.LBT
del BERI*.gnt
\cobol\PAK a /exe a:\beri bericht.lbr
