import Table from 'react-bootstrap/Table';
import { FormattedMessage, useIntl } from 'react-intl';
import { useSportsQuery } from './api';

import './Sports.css';

export const Sports: React.FC = () => {
  const intl = useIntl();
  const { data, error, isLoading, isFetching, isSuccess} = useSportsQuery();

  if (isLoading) 
    return <p>Loading...</p>
  else if (isFetching) 
    return <p>Fetching...</p>
  else if (error) 
    return <p>Something went wrong</p>
  else if (isSuccess && data)
  {
    const items = [];

    for (let sport of data)
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
    return <p>No data</p>
}
