DROP TABLE Comment;

CREATE TABLE Comment
(
  Id INTEGER NOT NULL,
  Name VARCHAR NOT NULL,
  NameComment VARCHAR NOT NULL,
  Description VARCHAR NOT NULL,
  DescriptionComment VARCHAR NOT NULL,
  PRIMARY KEY(Id)
);

INSERT INTO Comment VALUES(0, 'Soccer', 'Name comment', 'Soccer is a sport played between two teams of eleven players with a spherical ball.', 'Description comment');
INSERT INTO Comment VALUES(1, 'Ice hockey', '', 'Ice hockey is a team sport played on ice, in which skaters use sticks to direct a puck into the opposing team''s goal.', '');
INSERT INTO Comment VALUES(2, 'Basketball', '', 'Basketball is a team sport in which two teams of five players try to score points by throwing a ball through the top of a basketball hoop while following a set of rules.', '');