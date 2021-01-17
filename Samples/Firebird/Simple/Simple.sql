CREATE DATABASE 'Simple.fdb' default character set utf8;

CREATE TABLE Simple
(
  Id INTEGER NOT NULL,
  Sport VARCHAR(50) NOT NULL,
  PRIMARY KEY(Id)
);

INSERT INTO Simple VALUES(0, 'Soccer');
INSERT INTO Simple VALUES(1, 'Ice hockey');
INSERT INTO Simple VALUES(2, 'Basketball');
