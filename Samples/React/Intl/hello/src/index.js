import React from 'react';
import ReactDOM from 'react-dom';
import registerServiceWorker from './registerServiceWorker';
import './index.css';
import App from './App';
import {IntlProvider} from "react-intl";
import {addLocaleData} from "react-intl";
import locale_en from 'react-intl/locale-data/en';
import locale_de from 'react-intl/locale-data/de';
import locale_fi from 'react-intl/locale-data/fi';
import messages_en from "./translations/translations.json";
import messages_de from "./translations/de/translations.json";
import messages_fi from "./translations/fi/translations.json";

const messages = {
  'en': messages_en,
  'de': messages_de,
  'fi': messages_fi
};

const language = navigator.language.split(/[-_]/)[0];  // language without region code

addLocaleData([...locale_en, ...locale_de, ...locale_fi]);

ReactDOM.render(
  <IntlProvider locale={language} messages={messages[language]}>
    <App/>
  </IntlProvider>,
  document.getElementById('root')
);

registerServiceWorker();
