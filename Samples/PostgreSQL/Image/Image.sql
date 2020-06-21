DROP TABLE Images;

CREATE TABLE Images
(
  Id INTEGER NOT NULL,
  Language VARCHAR(10) NOT NULL,
  Name VARCHAR(100) NOT NULL,
  Data BYTEA,
  PRIMARY KEY(Id, Language)
);

INSERT INTO Images VALUES(0, 'en', 'Flag', null);
INSERT INTO Images VALUES(1, 'en', 'Car', null);
INSERT INTO Images VALUES(2, 'en', 'Address book', null);

/*
INSERT INTO Images VALUES(0, 'en', 'Flag', lo_import('D:/NT/Deploy/Samples/Common/64x64/flag_usa.png'));
INSERT INTO Images VALUES(1, 'en', 'Car', lo_import('D:/NT/Deploy/Samples/Common/64x64/car_sedan_blue.png'));
INSERT INTO Images VALUES(2, 'en', 'Address book', lo_import('D:/NT/Deploy/Samples/Common/64x64/address_book.png'));
*/