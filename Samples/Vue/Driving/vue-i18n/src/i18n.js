import { createI18n } from 'vue-i18n';

// The original language
import en from './locales/en.json';

// Add each language here. Soluling writes these files.
import de from './locales/de.json';
import fi from './locales/fi.json';
import ja from './locales/ja.json';

export const STORAGE_KEY = 'language';

const messages = { en, de, fi, ja };

// The language selected by the user, or the language of the browser without the region code
function getInitialLanguage() {
  const language = localStorage.getItem(STORAGE_KEY) || (navigator.language || 'en').split(/[-_]/)[0];
  return messages[language] ? language : 'en';
}

export default createI18n({
  legacy: false,
  locale: getInitialLanguage(),
  fallbackLocale: 'en',
  messages,
});
