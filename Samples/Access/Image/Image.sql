CREATE TABLE Images
(
  Id INTEGER NOT NULL,
  LangId VARCHAR(10) NOT NULL,
  Name VARCHAR(100) NOT NULL,
  Data IMAGE NOT NULL,
  PRIMARY KEY(Id, LangId)
);