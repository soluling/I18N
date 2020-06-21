CREATE DATABASE "Multiple.fdb" default character set utf8;

CREATE TABLE Localize
(
  Id INTEGER NOT NULL,
  Language VARCHAR(10) NOT NULL,
  Sport VARCHAR(50) NOT NULL,
  PRIMARY KEY(Id, Language)
);

INSERT INTO Localize VALUES(0, 'en', 'Soccer');
INSERT INTO Localize VALUES(1, 'en', 'Ice hockey');
INSERT INTO Localize VALUES(2, 'en', 'Basketball');

CREATE TABLE DoNotLocalize
(
  Id INTEGER NOT NULL,
  Code VARCHAR(50) NOT NULL,
  PRIMARY KEY(Id)
);

INSERT INTO DoNotLocalize VALUES(0, 'ABC');
INSERT INTO DoNotLocalize VALUES(1, 'D2F');
INSERT INTO DoNotLocalize VALUES(2, 'FFF45');
