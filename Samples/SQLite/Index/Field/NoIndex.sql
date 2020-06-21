DROP TABLE IndexSample;

CREATE TABLE IndexSample
(
  TableRow TEXT NOT NULL,
  TableColumn INTEGER NOT NULL,
  Value TEXT NOT NULL,
  Value_fi TEXT NOT NULL,
  Value_de TEXT NOT NULL,
  Value_ja TEXT NOT NULL,
  PRIMARY KEY(TableRow, TableColumn)
);

INSERT INTO IndexSample VALUES('0', 0, 'One', '', '', '');
INSERT INTO IndexSample VALUES('0', 1, 'Two', '', '', '');
INSERT INTO IndexSample VALUES('1', 0, 'Three', '', '', '');
INSERT INTO IndexSample VALUES('2', 0, 'Four', '', '', '');
INSERT INTO IndexSample VALUES('2', 1, 'Five', '', '', '');
