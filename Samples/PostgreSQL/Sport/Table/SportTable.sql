DROP TABLE IF EXISTS Sport;
DROP TABLE IF EXISTS Sport_fi;
DROP TABLE IF EXISTS Sport_de;
DROP TABLE IF EXISTS Sport_fr;
DROP TABLE IF EXISTS Sport_ja;

CREATE TABLE Sport
(
  Id INTEGER NOT NULL,
  Name VARCHAR(50) NOT NULL,
  FieldPlayers INTEGER NOT NULL,
  Goalie INTEGER NOT NULL,
  Origin VARCHAR(50) NOT NULL,
  Description VARCHAR(500) NOT NULL,
  PRIMARY KEY(Id)
);

CREATE TABLE Sport_fi
(
  Id INTEGER NOT NULL,
  Name VARCHAR(50) NOT NULL,
  Origin VARCHAR(50) NOT NULL,
  Description VARCHAR(500) NOT NULL,
  PRIMARY KEY(Id)
);

CREATE TABLE Sport_de
(
  Id INTEGER NOT NULL,
  Name VARCHAR(50) NOT NULL,
  Origin VARCHAR(50) NOT NULL,
  Description VARCHAR(500) NOT NULL,
  PRIMARY KEY(Id)
);

CREATE TABLE Sport_fr
(
  Id INTEGER NOT NULL,
  Name VARCHAR(50) NOT NULL,
  Origin VARCHAR(50) NOT NULL,
  Description VARCHAR(500) NOT NULL,
  PRIMARY KEY(Id)
);

CREATE TABLE Sport_ja
(
  Id INTEGER NOT NULL,
  Name VARCHAR(50) NOT NULL,
  Origin VARCHAR(50) NOT NULL,
  Description VARCHAR(500) NOT NULL,
  PRIMARY KEY(Id)
);

INSERT INTO Sport VALUES(0, 'Soccer', 10, 1, 'England', 'Soccer is a sport played between two teams of eleven players with a spherical ball.');
INSERT INTO Sport VALUES(1, 'Ice hockey', 5, 1, 'Canada', 'Ice hockey is a team sport played on ice, in which skaters use sticks to direct a puck into the opposing team''s goal.');
INSERT INTO Sport VALUES(2, 'Basketball', 5, 0, 'United States', 'Basketball is a team sport in which two teams of five players try to score points by throwing a ball through the top of a basketball hoop while following a set of rules.');
