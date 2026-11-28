import { OtaOptions } from '@soluling/angular';

// To try your own project without changing this file, copy it to ota.config.local.ts (git ignores
// it), change the copy, and run npm run start:local. It replaces this file in that build.

// The GUID of your project in Soluling Cloud. Find it on the Overview tab of the project's Manage
// page; turn on Publish artifacts on its Integrations tab. Leave it empty to use only the application's own localized
// files in src/locale, as the Runtime sample does.
export const SOLULING_PROJECT_GUID = '';

// The cookie that keeps the language the user selected
export const LANGUAGE_COOKIE = 'language';

// The localized files that Soluling builds next to the extracted src/locale/messages.arb: fi.arb,
// de.arb, and ja.arb. angular.json copies them to the locale folder of the application, where
// getTranslationsEx loads <id>.arb.
export const LOCALE_URL = 'locale';

// The options of getOtaTranslations and getOtaLanguages
export const OTA_OPTIONS: OtaOptions =
{
  cookieName: LANGUAGE_COOKIE,
  format: 'arb',
  fallbackUrl: LOCALE_URL,

  // Check for newer translations and languages on every load, so you see a change at once when you
  // try the sample. Leave it out in your application: the default checks once a day.
  checkInterval: 0,
};
