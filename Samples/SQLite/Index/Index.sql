DROP TABLE IndexSample;

CREATE TABLE IndexSample
(
  Id INTEGER NOT NULL,
  Value TEXT NOT NULL,
  TableRow TEXT NOT NULL,
  TableColumn INTEGER NOT NULL,
  PRIMARY KEY(Id)
);

CREATE UNIQUE INDEX TableIdx ON IndexSample(TableColumn, TableRow); 

INSERT INTO IndexSample VALUES(0, 'One', '0', 0);
INSERT INTO IndexSample VALUES(1, 'Two', '0', 1);
INSERT INTO IndexSample VALUES(2, 'Three', '1', 0);
INSERT INTO IndexSample VALUES(3, 'Four', '2', 0);
INSERT INTO IndexSample VALUES(4, 'Five', '2', 1);
