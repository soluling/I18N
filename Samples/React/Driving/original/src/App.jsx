import { useState } from 'react';

// The original English application that has not been internationalized
export default function App() {
  const [distance, setDistance] = useState('220');
  const [speed, setSpeed] = useState('88');
  const [result, setResult] = useState(null);

  const canCalculate = (Number(distance) > 0) && (Number(speed) > 0);

  function calculate(event) {
    event.preventDefault();

    const totalMinutes = Math.round(60*Number(distance)/Number(speed));

    setResult({ hours: Math.floor(totalMinutes/60), minutes: totalMinutes % 60 });
  }

  return (
    <form onSubmit={calculate}>
      <h1>Driving Time</h1>

      <label>
        Driving distance
        <input
          type="number"
          title="Distance in kilometers"
          value={distance}
          onChange={event => setDistance(event.target.value)}
        />
      </label>

      <label>
        Driving speed
        <input
          type="number"
          title="Speed in kilometers per hour"
          value={speed}
          onChange={event => setSpeed(event.target.value)}
        />
      </label>

      <button type="submit" disabled={!canCalculate}>Calculate</button>

      {result && <p>Driving time is {result.hours} hour(s) {result.minutes} minute(s)</p>}
    </form>
  );
}
