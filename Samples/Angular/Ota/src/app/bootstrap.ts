import { LOCALE_ID, provideZonelessChangeDetection } from '@angular/core';
import { registerLocaleData } from '@angular/common';
import { bootstrapApplication } from '@angular/platform-browser';
import { LocaleService } from '@soluling/angular';

import { AppComponent } from './app.component';

import de from '@angular/common/locales/de';
import fi from '@angular/common/locales/fi';
import ja from '@angular/common/locales/ja';

// Angular's locale data of the languages of this release, for Angular's own pipes and the
// components that use LOCALE_ID. The sample's own dates don't need it: the localDate pipe of
// @soluling/angular formats them with the browser's Intl API, which knows every language, also one
// added over the air later.
const LOCALE_DATA: Record<string, unknown[]> = { de, fi, ja };

for (const [id, data] of Object.entries(LOCALE_DATA))
  registerLocaleData(data, id);

// LOCALE_ID is the language of the loaded translations when its locale data is registered - Angular
// uses the data of the language part, such as fi for fi-FI - and otherwise the original language.
// Angular fails as soon as it formats with a locale it has no data for, and a language added over
// the air, after this release, has none.
function getLocaleId(service: LocaleService): string
{
  const id = service.localeId;
  return ((id in LOCALE_DATA) || (id.split('-')[0] in LOCALE_DATA)) ? id : 'en-US';
}

export function bootstrap()
{
  bootstrapApplication(AppComponent,
  {
    providers:
    [
      provideZonelessChangeDetection(),

      LocaleService,
      { provide: LOCALE_ID, deps: [LocaleService], useFactory: getLocaleId },
    ]
  }).catch(error => console.error(error));
}
