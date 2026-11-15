import { useState } from 'react';
import { createRoot } from 'react-dom/client';
import { IntlProvider } from 'react-intl';
import App from './App';
import './index.css';

// Add each language here
import messages_de from './translations/de.json';
import messages_fi from './translations/fi.json';
import messages_ja from './translations/ja.json';

const messages = {
  de: messages_de,
  fi: messages_fi,
  ja: messages_ja,
};

const STORAGE_KEY = 'language';

// The language selected by the user, or the language of the browser without the region code
function getInitialLanguage() {
  return localStorage.getItem(STORAGE_KEY) || (navigator.language || 'en').split(/[-_]/)[0];
}

function Root() {
  const [language, setLanguage] = useState(getInitialLanguage);

  function changeLanguage(value) {
    localStorage.setItem(STORAGE_KEY, value);
    setLanguage(value);
  }

  // English is the original language, so it has no messages. React Intl uses the default messages.
  return (
    <IntlProvider locale={language} defaultLocale="en" messages={messages[language]}>
      <App language={language} onLanguageChange={changeLanguage} />
    </IntlProvider>
  );
}

createRoot(document.getElementById('root')).render(<Root />);
