import React from 'react';
import ReactDOM from 'react-dom';
import Table from 'react-bootstrap/Table';
import { IntlProvider, FormattedMessage, injectIntl } from 'react-intl';

import 'bootstrap/dist/css/bootstrap.min.css';
import './index.css';

// Add each language here
import messages_de from "./translations/de.json";
import messages_fi from "./translations/fi.json";
import messages_ja from "./translations/ja.json";
import messages_sv from "./translations/sv.json";

class Sports extends React.Component 
{
  componentDidMount() 
  {
    fetch(`https://soluling.com/sportapi/sports`)
      .then(res => res.json())
      .then(result => this.setState({ sports: result }));
  }

  render() 
  {
    if (this.state)
    {
      const intl = this.props.intl;

      const items = [];

      for (let sport of this.state.sports)
      {
        let olympicAsString = "";

        if (sport.olympic === "Summer")
          olympicAsString = intl.formatMessage({id: "Summer"});
        else if (sport.olympic === "Winter")
          olympicAsString = intl.formatMessage({id: "Winter"});

        let goalieAsString = "";

        if (sport.goalie)
          goalieAsString = intl.formatMessage({id: "Yes"});
        else if (sport.goalie === false)
          goalieAsString = intl.formatMessage({id: "No"});
  
        items.push(
          <tr key={sport.id}>
            <td>{sport.id}</td>
            <td>{sport.languages[0].name}</td>
            <td>{olympicAsString}</td>
            <td>{sport.fieldPlayers}</td>
            <td>{goalieAsString}</td>
            <td>{sport.languages[0].origin}</td>
            <td>{sport.languages[0].description}</td>
          </tr>
        );
      }

      return (
        <div>
          <h1><FormattedMessage id="Sports" /></h1>
          <Table striped bordered hover>
            <thead>
              <tr>
                <th><FormattedMessage id="Id" /></th>
                <th><FormattedMessage id="Sport name" /></th>
                <th><FormattedMessage id="Olympic" /></th>
                <th><FormattedMessage id="Number of payers" /></th>
                <th><FormattedMessage id="Goalkeeper" /></th>
                <th><FormattedMessage id="Origin" /></th>
                <th><FormattedMessage id="Description" /></th>
              </tr>
            </thead>
            <tbody>
              {items}
            </tbody>
          </Table>
        </div>
      );
    }
    else
    {
      return <p>Loading</p>
    }
  }
}

Sports = injectIntl(Sports);

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

ReactDOM.render(
  <IntlProvider locale={language} messages={messages[language]}>
    <Sports />,
  </IntlProvider>,
  document.getElementById('root')
);