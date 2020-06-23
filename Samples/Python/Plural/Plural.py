import gettext

gettext.bindtextdomain('Plural', 'locale')
gettext.textdomain('Plural')
_ = gettext.gettext

print(_("Plural Sample"))

for value in [0, 1, 2, 3, 4, 5, 11, 21, 101, 111]:
  # count: Amount of files
  print(gettext.ngettext("{count} file", "{count} files", value).format(count=value))