USE SportImage;

DROP TABLE Sport;

CREATE TABLE Sport
(
  Id INTEGER NOT NULL,
  Language VARCHAR(10) NOT NULL,
  Name NVARCHAR(50) NOT NULL,
  Picture IMAGE NOT NULL,
  FieldPlayers INTEGER NOT NULL,
  Goalie INTEGER NOT NULL,
  Origin NVARCHAR(50) NOT NULL,
  Description NVARCHAR(500) NOT NULL,
  PRIMARY KEY(Id, Language)
);

INSERT INTO Sport (Id, Language, Name, FieldPlayers, Goalie, Origin, Description, Picture) SELECT 0, 'en', 'Soccer', 10, 1, 'England', 'Soccer is a sport played between two teams of eleven players with a spherical ball.', * FROM OPENROWSET(BULK N'D:/NT/Deploy/Samples/Common/64x64/Soccer_Ball.png', SINGLE_BLOB) rs;
INSERT INTO Sport (Id, Language, Name, FieldPlayers, Goalie, Origin, Description, Picture) SELECT 1, 'en', 'Ice hockey', 5, 1, 'Canada', 'Ice hockey is a team sport played on ice, in which skaters use sticks to direct a puck into the opposing team''s goal.', * FROM OPENROWSET(BULK N'D:/NT/Deploy/Samples/Common/64x64/Hockey_IceSkate.png', SINGLE_BLOB) rs;
INSERT INTO Sport (Id, Language, Name, FieldPlayers, Goalie, Origin, Description, Picture) SELECT 2, 'en', 'Basketball', 5, 0, 'United States', 'Basketball is a team sport in which two teams of five players try to score points by throwing a ball through the top of a basketball hoop while following a set of rules.', * FROM OPENROWSET(BULK N'D:/NT/Deploy/Samples/Common/64x64/Basketball_Ball.png', SINGLE_BLOB) rs;
