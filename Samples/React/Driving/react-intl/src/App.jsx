import { useEffect, useState } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';

export default function App({ language, onLanguageChange }) {
  const intl = useIntl();
  const [distance, setDistance] = useState('220');
  const [speed, setSpeed] = useState('88');
  const [result, setResult] = useState(null);

  const canCalculate = (Number(distance) > 0) && (Number(speed) > 0);

  // A string in code
  const pageTitle = intl.formatMessage({ id: 'pageTitle', defaultMessage: 'Driving Time Calculator' });

  useEffect(() => {
    document.title = pageTitle;
  }, [pageTitle]);

  function calculate(event) {
    event.preventDefault();

    const totalMinutes = Math.round(60*Number(distance)/Number(speed));

    setResult({ hours: Math.floor(totalMinutes/60), minutes: totalMinutes % 60 });
  }

  return (
    <form onSubmit={calculate}>
      <h1><FormattedMessage id="drivingTime" defaultMessage="Driving Time" /></h1>

      <label>
        <FormattedMessage id="distance" defaultMessage="Driving distance" />
        <input
          type="number"
          title={intl.formatMessage({ id: 'distanceHint', defaultMessage: 'Distance in kilometers' })}
          value={distance}
          onChange={event => setDistance(event.target.value)}
        />
      </label>

      <label>
        <FormattedMessage id="speed" defaultMessage="Driving speed" />
        <input
          type="number"
          title={intl.formatMessage({ id: 'speedHint', defaultMessage: 'Speed in kilometers per hour' })}
          value={speed}
          onChange={event => setSpeed(event.target.value)}
        />
      </label>

      <button type="submit" disabled={!canCalculate}>
        <FormattedMessage id="calculate" defaultMessage="Calculate" />
      </button>

      {result && (
        <p>
          <FormattedMessage
            id="result"
            defaultMessage="Driving time is {hours, plural, =0 {} one {# hour} other {# hours}} {minutes, plural, one {# minute} other {# minutes}}"
            description="hours, minutes: The driving time"
            values={result}
          />
        </p>
      )}

      <label>
        <FormattedMessage id="language" defaultMessage="Language" />
        <select value={language} onChange={event => onLanguageChange(event.target.value)}>
          <option value="en">English</option>
          <option value="fi">suomi</option>
          <option value="de">Deutsch</option>
          <option value="ja">日本語</option>
        </select>
      </label>
    </form>
  );
}
