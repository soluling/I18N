import React, { Component } from 'react';
import { translate, Trans } from 'react-i18next';
import './App.css';

class App extends Component 
{
  render() {
    var name = "John";

    return (
      <div className="App">
        <header className="App-header">
          <h1 className="App-title"> 
            { this.props.t('welcome.title', { framework: "react-i18next" }) }
          </h1>
        </header>
        <p className="App-intro">
          <Trans i18nKey='welcome.intro'>
            To get started, edit <code>src/App.js</code> and save to reload.
          </Trans>
        </p>
        <p className="App-intro">
          <Trans i18nKey='welcome.pattern'>
            Hello {{name}}!
          </Trans>
        </p>
      </div>
    );
  }
}

export default translate('common')(App);
