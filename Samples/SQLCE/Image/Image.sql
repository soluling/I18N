CREATE TABLE Images
(
  Id INTEGER NOT NULL,
  Language NVARCHAR(10) NOT NULL,
  Name NVARCHAR(100) NOT NULL,
  Data IMAGE NOT NULL,
  PRIMARY KEY(Id, Language)
)

INSERT INTO Images (Id, Language, Name, Data) SELECT 0, 'en', 'Flag', * FROM OPENROWSET(BULK N'D:/NT/Deploy/Samples/Common/64x64/flag_usa.png', SINGLE_BLOB) rs;
INSERT INTO Images (Id, Language, Name, Data) SELECT 1, 'en', 'Car', * FROM OPENROWSET(BULK N'D:/NT/Deploy/Samples/Common/64x64/car_sedan_blue.png', SINGLE_BLOB) rs;
INSERT INTO Images (Id, Language, Name, Data) SELECT 2, 'en', 'Address book', * FROM OPENROWSET(BULK N'D:/NT/Deploy/Samples/Common/64x64/address_book.png', SINGLE_BLOB) rs;