USE Data;

DROP TABLE Data;

CREATE TABLE Data
(
  Name VARCHAR(50) NOT NULL,
  Language VARCHAR(10) NOT NULL,
  Description VARCHAR(100) NOT NULL,
  Data IMAGE NOT NULL,
  PRIMARY KEY(Name, Language)
);

INSERT INTO Data (Name, Language, Description, Data) SELECT 'Text', 'en', 'A text file', * FROM OPENROWSET(BULK N'D:/NT/Deploy/Samples/Common/Sample.txt', SINGLE_BLOB) rs;
INSERT INTO Data (Name, Language, Description, Data) SELECT 'Xml', 'en', 'An XML file', * FROM OPENROWSET(BULK N'D:/NT/Deploy/Samples/XML/Simple/Simple.xml', SINGLE_BLOB) rs;
INSERT INTO Data (Name, Language, Description, Data) SELECT 'Image', 'en', 'An image file', * FROM OPENROWSET(BULK N'D:/NT/Deploy/Samples/Common/16x16/drink_blue.png', SINGLE_BLOB) rs;
INSERT INTO Data (Name, Language, Description, Data) SELECT 'Sound', 'en', 'A sound file', * FROM OPENROWSET(BULK N'D:/NT/Deploy/Samples/Common/chimes.wav', SINGLE_BLOB) rs;
INSERT INTO Data (Name, Language, Description, Data) SELECT 'SingleZip', 'en', 'A zip file that contains one file', * FROM OPENROWSET(BULK N'D:/NT/Deploy/Samples/Common/Single.zip', SINGLE_BLOB) rs;
INSERT INTO Data (Name, Language, Description, Data) SELECT 'MultiZip', 'en', 'A zip file that contains several files', * FROM OPENROWSET(BULK N'D:/NT/Deploy/Samples/Common/Multi.zip', SINGLE_BLOB) rs;