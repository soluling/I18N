import time, datetime, locale

now = datetime.datetime.now().time()
print(now)

now = datetime.datetime.now()
print(now)

print("Default locale and encoding: " + str(locale.getdefaultlocale(locale.LC_ALL)))

def Process(id):
  if id != "":
    locale.setlocale(locale.LC_ALL, id)

  print()
  print(f"{id}: {locale.getlocale(locale.LC_ALL)}")
  print(now.strftime("%c"))
  return

for id in ['', 'en', 'fi', 'de', 'ja']:
  Process(id)

print()
