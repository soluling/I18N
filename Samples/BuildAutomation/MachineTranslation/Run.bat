rem Let's make a copy of our Soluling project
copy Project1.ntp Work.ntp

rem Scan for changed items
SoluMake scan Work.ntp

rem Translate German and French using DeepL
SoluMake translate -lang:fr;de -engine:DeepL Work.ntp

rem Translate Finnish using Google Tranlate
SoluMake translate -lang:fi -engine:Google Work.ntp

rem Create localized files
SoluMake build Work.ntp