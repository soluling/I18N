USE SportFieldNull;

DROP TABLE Sport;

CREATE TABLE Sport
(
  Id INTEGER NOT NULL,
  Name NVARCHAR(50) NOT NULL,
  Name_fi NVARCHAR(50),
  Name_de NVARCHAR(50),
  Name_fr NVARCHAR(50),
  Name_ja NVARCHAR(50),
  FieldPlayers INTEGER NOT NULL,
  Goalie INTEGER NOT NULL,
  Origin NVARCHAR(50) NOT NULL,
  Origin_fi NVARCHAR(50),
  Origin_de NVARCHAR(50),
  Origin_fr NVARCHAR(50),
  Origin_ja NVARCHAR(50),
  Description NVARCHAR(500) NOT NULL,
  Description_fi NVARCHAR(500),
  Description_de NVARCHAR(500),
  Description_fr NVARCHAR(500),
  Description_ja NVARCHAR(500),
  PRIMARY KEY(Id)
);

INSERT INTO Sport VALUES(0, 'Soccer', NULL, NULL, NULL, NULL, 10, 1, 'England', NULL, NULL, NULL, NULL, 'Soccer is a sport played between two teams of eleven players with a spherical ball.', NULL, NULL, NULL, NULL);
INSERT INTO Sport VALUES(1, 'Ice hockey', NULL, NULL, NULL, NULL, 5, 1, 'Canada', NULL, NULL, NULL, NULL, 'Ice hockey is a team sport played on ice, in which skaters use sticks to direct a puck into the opposing team''s goal.', NULL, NULL, NULL, NULL);
INSERT INTO Sport VALUES(2, 'Basketball', NULL, NULL, NULL, NULL, 5, 0, 'United States', NULL, NULL, NULL, NULL, 'Basketball is a team sport in which two teams of five players try to score points by throwing a ball through the top of a basketball hoop while following a set of rules.', NULL, NULL, NULL, NULL);
