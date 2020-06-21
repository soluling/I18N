DROP TABLE Composite;

CREATE TABLE Composite
(
  GroupId VARCHAR(10) NOT NULL,
  ItemId INTEGER NOT NULL,
  Language VARCHAR(10) NOT NULL,
  Value VARCHAR NOT NULL,
  PRIMARY KEY(GroupId, ItemId, Language)
);

INSERT INTO Composite VALUES('country', 0, 'en', 'Finland');
INSERT INTO Composite VALUES('country', 1, 'en', 'Germany');
INSERT INTO Composite VALUES('country', 2, 'en', 'Japan');
INSERT INTO Composite VALUES('sport', 0, 'en', 'Soccer');
INSERT INTO Composite VALUES('sport', 1, 'en', 'Ice hockey');
INSERT INTO Composite VALUES('sport', 2, 'en', 'Basketball');
