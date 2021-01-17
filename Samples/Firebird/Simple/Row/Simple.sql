CREATE DATABASE 'Simple.fdb' default character set utf8;

CREATE TABLE Simple
(
  Id INTEGER NOT NULL,
  Language VARCHAR(10) NOT NULL,
  Sport VARCHAR(50) NOT NULL,
  PRIMARY KEY(Id, Language)
);

INSERT INTO Simple VALUES(0, 'en', 'Soccer');
INSERT INTO Simple VALUES(1, 'en', 'Ice hockey');
INSERT INTO Simple VALUES(2, 'en', 'Basketball');
