import datetime
from django.http import HttpResponse
from django.utils.translation import gettext as _
from django.utils.formats import date_format
from django.utils.translation import get_language_info

def index(request):
  now = datetime.datetime.now()
  dateTime = date_format(now, format='SHORT_DATETIME_FORMAT')

  info = get_language_info(request.LANGUAGE_CODE)
  infoStr = f" {request.LANGUAGE_CODE}: {info['name']} {info['name_local']} {info['name_translated']} {info['bidi']}"

  # Do not use string interpolcation f"..." but string.format
  # Translators: dateTime: Date time as a string
  return HttpResponse(_("Hello, today is {dateTime}").format(dateTime=dateTime) + infoStr) 
