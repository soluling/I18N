CREATE DATABASE "SportAnsi.gdb";

CREATE TABLE Sport
(
  Id INTEGER NOT NULL,
  Language VARCHAR(10) NOT NULL,
  Name VARCHAR(50) NOT NULL,
  FieldPlayers INTEGER NOT NULL,
  Goalie INTEGER NOT NULL,
  Origin VARCHAR(50) NOT NULL,
  Description VARCHAR(500) NOT NULL,
  PRIMARY KEY(Id, Language)
);

INSERT INTO Sport VALUES(0, 'en', 'Soccer', 10, 1, 'England', 'Soccer is a sport played between two teams of eleven players with a spherical ball.');
INSERT INTO Sport VALUES(1, 'en', 'Ice hockey', 5, 1, 'Canada', 'Ice hockey is a team sport played on ice, in which skaters use sticks to direct a puck into the opposing team''s goal.');
INSERT INTO Sport VALUES(2, 'en', 'Basketball', 5, 0, 'United States', 'Basketball is a team sport in which two teams of five players try to score points by throwing a ball through the top of a basketball hoop while following a set of rules.');
