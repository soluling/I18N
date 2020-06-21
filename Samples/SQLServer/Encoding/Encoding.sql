USE Encoding;

DROP TABLE Encoding;

CREATE TABLE Encoding
(
  Id INTEGER NOT NULL,
  Language VARCHAR(10) NOT NULL,
  AnsiName VARCHAR(50) NOT NULL,
  WideName NVARCHAR(50) NOT NULL,
  PRIMARY KEY(Id, Language)
);

INSERT INTO Encoding VALUES(0, 'en', 'One', 'One');
INSERT INTO Encoding VALUES(1, 'en', 'Two', 'Two');