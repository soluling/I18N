import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import App from './App';
import registerServiceWorker from './registerServiceWorker';
import {I18nextProvider} from 'react-i18next';
import i18next from 'i18next';
import common_en from "./translations/en/common.json";
import common_fi from "./translations/fi/common.json";

i18next.init({
  interpolation: { escapeValue: false },  // React already does escaping
  lng: 'fi',                              // language to use
    resources: {
      en: {
        common: common_en               // 'common' is our custom namespace
      },
      fi: {
        common: common_fi
      },
    },
});

ReactDOM.render(
  <I18nextProvider i18n={i18next}>
      <App/>
  </I18nextProvider>,
  document.getElementById('root')
);

registerServiceWorker();
