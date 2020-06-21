USE images;

DROP TABLE IF EXISTS Images;

CREATE TABLE Images
(
  Id INTEGER NOT NULL,
  Language VARCHAR(10) NOT NULL,
  Name VARCHAR(100) NOT NULL,
  Data BLOB NOT NULL,
  PRIMARY KEY(Id, Language)
);

INSERT INTO Images VALUES(0, 'en', 'Flag', LOAD_FILE('D:/NT/Deploy/Samples/Common/64x64/flag_usa.png'));
INSERT INTO Images VALUES(1, 'en', 'Car', LOAD_FILE('D:/NT/Deploy/Samples/Common/64x64/car_sedan_blue.png'));
INSERT INTO Images VALUES(2, 'en', 'Address book', LOAD_FILE('D:/NT/Deploy/Samples/Common/64x64/address_book.png'));
