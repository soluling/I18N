import { loadTranslations } from '@angular/localize';
import { getOtaTranslations, getTranslationsEx, Translations } from '@soluling/angular';

import { LANGUAGE_COOKIE, LOCALE_URL, OTA_OPTIONS, SOLULING_PROJECT_GUID } from './ota.config';

// Get the translations of the user's language before the application starts, because $localize
// translates every text once, when the application starts.
function getAppTranslations(): Promise<Translations>
{
  // The language selected by the user (the cookie), or the browser's languages
  if (!SOLULING_PROJECT_GUID)
    return getTranslationsEx(LOCALE_URL, { cookieName: LANGUAGE_COOKIE, format: 'arb' });

  // Over the air: the translations published in Soluling Cloud. After the first visit they come
  // from the browser's storage at once, and a newer published file is used on the next load. If
  // nothing has been downloaded yet and the download fails, the application's own files are used.
  return getOtaTranslations(SOLULING_PROJECT_GUID, OTA_OPTIONS);
}

getAppTranslations().then(translations =>
{
  if (translations)
    loadTranslations(translations);

  // Import the application only now, so that its texts use the loaded translations
  import('./app/bootstrap').then(module => module.bootstrap());
});
