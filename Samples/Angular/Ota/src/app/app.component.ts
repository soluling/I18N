import { Component, inject, signal } from '@angular/core';
import { getOtaLanguages, LocalDatePipe, LocalDisplayNamePipe, LocaleService, setCookie } from '@soluling/angular';

import { LANGUAGE_COOKIE, OTA_OPTIONS, SOLULING_PROJECT_GUID } from '../ota.config';

// The languages of the application itself: the original language and the localized files in
// src/locale. The languages published in Soluling Cloud are added to these. The template names each
// in its own language, such as suomi or 日本語, with the localDisplayName pipe.
const APP_LANGUAGES = ['en', 'fi', 'de', 'ja'];

@Component({
  selector: 'app-root',
  imports: [LocalDatePipe, LocalDisplayNamePipe],
  templateUrl: './app.component.html',
})
export class AppComponent
{
  readonly projectGuid = SOLULING_PROJECT_GUID;
  // The language of the loaded translations, such as sv - not LOCALE_ID, which is the original
  // language for a language without locale data in this release (see bootstrap.ts)
  readonly localeId = inject(LocaleService).localeId;
  readonly name = 'John';
  readonly count = 3;
  readonly today = new Date();

  // A text in code
  readonly languageLabel = $localize`:The label of the language list@@language:Language`;

  readonly languages = signal<string[]>(APP_LANGUAGES);

  constructor()
  {
    if (this.projectGuid)
      this.addPublishedLanguages();
  }

  // Add the languages published in Soluling Cloud, so that a language added there is in the list
  // without a new build. After the first visit they come from the browser at once.
  private addPublishedLanguages()
  {
    getOtaLanguages(this.projectGuid, OTA_OPTIONS)
      .then(published =>
      {
        let ids = [...APP_LANGUAGES];

        for (let id of published)
        {
          if (!ids.some(existing => existing.toLowerCase() === id.toLowerCase()))
            ids.push(id);
        }

        this.languages.set(ids);
      });
  }

  // The language of the loaded translations, such as fi or zh-Hant. Without translations it is the
  // original language, en-US, which is en in the list.
  isSelected(id: string): boolean
  {
    let locale = this.localeId.toLowerCase();
    id = id.toLowerCase();

    return (locale === id) || ((locale.split('-')[0] === id) && !this.languages().some(language => language.toLowerCase() === locale));
  }

  // $localize translates the texts when the application starts, so load the page again in the new language
  changeLanguage(id: string)
  {
    setCookie(LANGUAGE_COOKIE, id);
    location.reload();
  }
}
