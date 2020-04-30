import React, { Component } from 'react';
import { FormattedMessage, FormattedHTMLMessage } from 'react-intl';
import './App.css';

class App extends Component {
  render() {
    return (
      <div className="App">
        <header className="App-header">
          <h1 className="App-title">
            <FormattedMessage id="app.title"
              defaultMessage="Welcome {who}"
              description="Welcome header on app main page"
              values={{ who: 'Jaakko' }}/>
          </h1>
        </header>
        <p className="App-intro">
          <FormattedHTMLMessage id="app.intro"
            defaultMessage="To get started, edit <code>src/App.js</code> and save to reload."
            description="Text on main page"/>
        </p>
        <p className="App-intro">
          <FormattedMessage id="app.plural"
            defaultMessage="{ski, plural, one {I have {ski} ski} other {I have {ski} skis}}"
            description="Text on main page"
            values={{ ski: 1 }}/>
        </p>
      </div>
    );
  }
}

export default App;
