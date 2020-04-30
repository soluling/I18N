import React from 'react';
import './App.css';
import { Trans } from '@lingui/macro';
import catalogFi from './locale/fi/messages.js'

function App() {
  return (
    <div className="App">
      <h1><Trans>Multilingual Sample</Trans></h1>
      <p><Trans>Hello World!</Trans></p>
    </div>
  );
}

export default App;