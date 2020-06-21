call Clean.bat

del /Q *.ntp*
del /Q *.nds

SoluMake add -origlang:en -lang:fi;de;ja Sport.ini Sport.ntp
copy Sport.ntp SportEmpty.ntp

SoluMake translate -engine:Microsoft -lang:fi;de Sport.ntp

SoluMake exchange Sport.ntpx Sport.ntp
SoluMake exchange -lang:fi SportFi.ntpx Sport.ntp
SoluMake exchange -lang:de SportDe.ntpx Sport.ntp
SoluMake exchange -lang:ja SportJa.ntpx Sport.ntp

rem call Export.bat

SoluMake add -origlang:en -lang:fi;de;ja Sport.ini Pseudo.ntp
SoluMake fillpseudo Pseudo.ntp
copy Pseudo.ntp PseudoComplete.ntp
SoluMake removepseudo Pseudo.ntp

SoluMake add -origlang:en -lang:fi;de;ja Sport.ini PseudoFi.ntp
SoluMake -lang:fi fillpseudo PseudoFi.ntp
