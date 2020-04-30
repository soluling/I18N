import React from 'react';
import ReactDOM from 'react-dom';
import { IntlProvider, FormattedMessage, injectIntl } from 'react-intl';
import './index.css';

// Add each language here
import messages_de from "./translations/de.json";
import messages_fi from "./translations/fi.json";
import messages_ja from "./translations/ja.json";

function Square(props)
{
  return (
    <button 
      className="square" 
      onClick={props.onClick}
    >
      {props.value}
    </button>
  );
}

class Board extends React.Component 
{
  renderSquare(i) 
  {
    return (
      <Square 
        value={this.props.squares[i]}
        onClick={() => this.props.onClick(i)} />
    )
  }

  render() 
  {
    return (
      <div>
        <div className="board-row">
          {this.renderSquare(0)}
          {this.renderSquare(1)}
          {this.renderSquare(2)}
        </div>
        <div className="board-row">
          {this.renderSquare(3)}
          {this.renderSquare(4)}
          {this.renderSquare(5)}
        </div>
        <div className="board-row">
          {this.renderSquare(6)}
          {this.renderSquare(7)}
          {this.renderSquare(8)}
        </div>
      </div>
    );
  }
}

class Game extends React.Component 
{
  constructor(props) 
  {
    super(props);

    this.state = 
    {
      history: [{
        squares: Array(9).fill(null),
      }],
      stepNumber: 0,
      xIsNext: true,
    };
  }

  handleClick(i) 
  {
    const history = this.state.history.slice(0, this.state.stepNumber + 1);    
    const current = history[history.length - 1];
    const squares = current.squares.slice();    

    if (calculateWinner(squares) || squares[i]) 
    {
      return;
    }    

    squares[i] = this.state.xIsNext ? 'X' : 'O';

    this.setState(
      {
        history: history.concat([{
          squares: squares,
        }]),
        stepNumber: history.length,
        xIsNext: !this.state.xIsNext,
      });
  }

  jumpTo(step) 
  {
    this.setState({
      stepNumber: step,
      xIsNext: (step % 2) === 0,
    });
  }  
  
  render() 
  {
    const history = this.state.history;
    const current = history[this.state.stepNumber];    
    const winner = calculateWinner(current.squares);
    const intl = this.props.intl;

    const moves = history.map((step, move) => 
    {
      const desc = move ?
        intl.formatMessage({id: 'Go to move # {move}', defaultMessage: 'Go to move # {move}'}, { move: move}) :
        intl.formatMessage({id: 'Go to game start'});
        
      return (
        <li key={move}>
          <button onClick={() => this.jumpTo(move)}>{desc}</button>
        </li>
      );
    });

    let status;

    if (winner) 
      status = intl.formatMessage({id: 'Winner: {winner}', defaultMessage: 'Winner: {winner}', description: 'winner: Name of the winner'}, { winner: winner});
    else 
      status = intl.formatMessage({id: 'Next player: {player}', defaultMessage: 'Next player: {player}', description: 'player: Name of the next player'}, { player: (this.state.xIsNext ? 'X' : 'O')});

    return (
      <div>
        <h1><FormattedMessage id="Tic-Tac-Toe Game" /></h1>
        <div className="game">
          <br/>
          <div className="game-board">
            <Board 
              squares={current.squares}
              onClick={(i) => this.handleClick(i)}
            />
          </div>
          <div className="game-info">
            <div>{status}</div>
            <ol>{moves}</ol>
          </div>
        </div>
      </div>
    );
  }
}

Game = injectIntl(Game);

function calculateWinner(squares) 
{
  const lines = [
    [0, 1, 2],
    [3, 4, 5],
    [6, 7, 8],
    [0, 3, 6],
    [1, 4, 7],
    [2, 5, 8],
    [0, 4, 8],
    [2, 4, 6],
  ];

  for (let i = 0; i < lines.length; i++) 
  {
    const [a, b, c] = lines[i];
  
    if (squares[a] && squares[a] === squares[b] && squares[a] === squares[c]) 
    {
      return squares[a];
    }
  }

  return null;
}

// ========================================

// Add each language here
const messages = 
{
  'de': messages_de,
  'fi': messages_fi,
  'ja': messages_ja
};

let language = navigator.language || 'en';
language = language.split(/[-_]/)[0];  // language without region code

ReactDOM.render(
  <IntlProvider locale={language} messages={messages[language]}>
    <Game />,
  </IntlProvider>,
  document.getElementById('root')
);
