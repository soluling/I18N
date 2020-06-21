DROP TABLE Sport;

CREATE TABLE Sport
(
  Id INTEGER NOT NULL,
  Name NVARCHAR(50) NOT NULL,
  Name_fi NVARCHAR(50) NOT NULL,
  Name_de NVARCHAR(50) NOT NULL,
  Name_fr NVARCHAR(50) NOT NULL,
  Name_ja NVARCHAR(50) NOT NULL,
  FieldPlayers INTEGER NOT NULL,
  Goalie INTEGER NOT NULL,
  Origin NVARCHAR(50) NOT NULL,
  Origin_fi NVARCHAR(50) NOT NULL,
  Origin_de NVARCHAR(50) NOT NULL,
  Origin_fr NVARCHAR(50) NOT NULL,
  Origin_ja NVARCHAR(50) NOT NULL,
  Description NVARCHAR(500) NOT NULL,
  Description_fi NVARCHAR(500) NOT NULL,
  Description_de NVARCHAR(500) NOT NULL,
  Description_fr NVARCHAR(500) NOT NULL,
  Description_ja NVARCHAR(500) NOT NULL,
  PRIMARY KEY(Id)
);

INSERT INTO Sport VALUES(0, 'Soccer', '', '', '', '', 10, 1, 'England', '', '', '', '', 'Soccer is a sport played between two teams of eleven players with a spherical ball.', '', '', '', '');
INSERT INTO Sport VALUES(1, 'Ice hockey', '', '', '', '', 5, 1, 'Canada', '', '', '', '', 'Ice hockey is a team sport played on ice, in which skaters use sticks to direct a puck into the opposing team''s goal.', '', '', '', '');
INSERT INTO Sport VALUES(2, 'Basketball', '', '', '', '', 5, 0, 'United States', '', '', '', '', 'Basketball is a team sport in which two teams of five players try to score points by throwing a ball through the top of a basketball hoop while following a set of rules.', '', '', '', '');
