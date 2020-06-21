DROP TABLE Composite;

CREATE TABLE Composite
(
  Id INTEGER NOT NULL,
  GroupId VARCHAR(10) NOT NULL,
  ItemId INTEGER NOT NULL,
  Language VARCHAR(10) NOT NULL,
  Value VARCHAR NOT NULL,
  Value_fi VARCHAR NOT NULL,
  Value_xx VARCHAR NOT NULL,
  PRIMARY KEY(Id)
);

INSERT INTO Composite VALUES(1, 'country', 0, 'en', 'Finland', '', '');
INSERT INTO Composite VALUES(2, 'country', 1, 'en', 'Germany', '', '');
INSERT INTO Composite VALUES(3, 'country', 2, 'en', 'Japan', '', '');
INSERT INTO Composite VALUES(4, 'sport', 0, 'en', 'Soccer', '', '');
INSERT INTO Composite VALUES(5, 'sport', 1, 'en', 'Ice hockey', '', '');
INSERT INTO Composite VALUES(6, 'sport', 2, 'en', 'Basketball', '', '');
