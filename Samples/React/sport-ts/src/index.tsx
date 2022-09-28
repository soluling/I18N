import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { IntlProvider, injectIntl } from 'react-intl';
import { Sports } from './Sports';
import { store } from './store';

import 'bootstrap/dist/css/bootstrap.min.css';
import './index.css';

// Add each language here
import messages_de from "./translations/de.json";
import messages_fi from "./translations/fi.json";
import messages_ja from "./translations/ja.json";
import messages_sv from "./translations/sv.json";

injectIntl(Sports);

// Add each language here
const messages = 
{
  'de': messages_de,
  'fi': messages_fi,
  'ja': messages_ja,
  'sv': messages_sv,
};

let language = navigator.language || 'en';
language = language.split(/[-_]/)[0];  // language without region code

const getKeyValue = <T extends object, U extends keyof T>(key: U) => (obj: T) => obj[key];
const msg = getKeyValue<Record<string, Record<string,string>>, string>(language)(messages)

ReactDOM.render(
  <React.StrictMode>
    <Provider store={store}>
      <IntlProvider locale={language} messages={msg}>
        <Sports />
      </IntlProvider>,
    </Provider>
  </React.StrictMode>,
  document.getElementById('root')
);
