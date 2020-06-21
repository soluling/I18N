DROP TABLE Simple;

CREATE TABLE Simple
(
  Id INTEGER NOT NULL,
  Value VARCHAR(50) NOT NULL,
  ValueF VARCHAR(50) NOT NULL,
  ValueD VARCHAR(50) NOT NULL,
  ValueJ VARCHAR(50) NOT NULL,
  PRIMARY KEY(Id)
);

INSERT INTO Simple VALUES(0, 'Soccer', '', '', '');
INSERT INTO Simple VALUES(1, 'Ice hockey', '', '', '');
INSERT INTO Simple VALUES(2, 'Basketball', '', '', '');