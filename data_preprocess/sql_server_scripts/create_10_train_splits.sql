-- This script takes as input the train.csv and test.csv files and generated 10 random splits of train.csv with those ID's removed that have all 'Ref' columns as null.
-- NOTE: currently the path for the csv files is hard-coded ('C:\myD\workarea\Kaggle567Workarea\mlclass_567_kaggle_rain\data_CSV\train.csv'), change this path to the actual pata where these files are present.

-- create tables for TRAIN and TEST data on which we'll bulk insert
CREATE TABLE [dbo].[TRAIN] (
[Id] numeric(18,0),
[minutes_past] numeric(18,0),
[radardist_km] numeric(18,0),
[Ref] real,
[Ref_5x5_10th] real,
[Ref_5x5_50th] real,
[Ref_5x5_90th] real,
[RefComposite] real,
[RefComposite_5x5_10th] real,
[RefComposite_5x5_50th] real,
[RefComposite_5x5_90th] real,
[RhoHV] real,
[RhoHV_5x5_10th] real,
[RhoHV_5x5_50th] real,
[RhoHV_5x5_90th] real,
[Zdr] real,
[Zdr_5x5_10th] real,
[Zdr_5x5_50th] real,
[Zdr_5x5_90th] real,
[Kdp] real,
[Kdp_5x5_10th] real,
[Kdp_5x5_50th] real,
[Kdp_5x5_90th] real,
[Expected] real
)
CREATE TABLE [dbo].[TEST] (
[Id] numeric(18,0),
[minutes_past] numeric(18,0),
[radardist_km] numeric(18,0),
[Ref] real,
[Ref_5x5_10th] real,
[Ref_5x5_50th] real,
[Ref_5x5_90th] real,
[RefComposite] real,
[RefComposite_5x5_10th] real,
[RefComposite_5x5_50th] real,
[RefComposite_5x5_90th] real,
[RhoHV] real,
[RhoHV_5x5_10th] real,
[RhoHV_5x5_50th] real,
[RhoHV_5x5_90th] real,
[Zdr] real,
[Zdr_5x5_10th] real,
[Zdr_5x5_50th] real,
[Zdr_5x5_90th] real,
[Kdp] real,
[Kdp_5x5_10th] real,
[Kdp_5x5_50th] real,
[Kdp_5x5_90th] real
)

-- Bulk insert TRAIN data
BULK
INSERT TRAIN
FROM 'C:\myD\workarea\Kaggle567Workarea\mlclass_567_kaggle_rain\data_CSV\train.csv'
WITH
(
FIELDTERMINATOR = ',',
ROWTERMINATOR = '0x0a',
FIRSTROW = 2
)
-- Bulk insert TEST data
BULK
INSERT TEST
FROM 'C:\myD\workarea\Kaggle567Workarea\mlclass_567_kaggle_rain\data_CSV\test.csv'
WITH
(
FIELDTERMINATOR = ',',
ROWTERMINATOR = '0x0a',
FIRSTROW = 2
)

--
-- query to get TRAIN_NEW out of TRAIN (remove ID's with all nulls in ref column)
select * into TRAIN_NEW from TRAIN where Id in(select Distinct Id from TRAIN where ref is not null)

-- split TRAIN_NEW into 10 parts
-- drop table ID_BIN_TABLE -- drop of already there
select id, NTILE(10) OVER(ORDER BY ABS(CHECKSUM(NEWID()))) id_tag into ID_BIN_TABLE
from TRAIN_NEW group by id;

-- drop table TRAIN_NEW_SPLITX
SELECT * into TRAIN_NEW_SPLIT1 FROM TRAIN_NEW tn
WHERE tn.id in (select ibt.id from ID_BIN_TABLE ibt where ibt.id_tag = 1);

SELECT * into TRAIN_NEW_SPLIT2 FROM TRAIN_NEW tn
WHERE tn.id in (select ibt.id from ID_BIN_TABLE ibt where ibt.id_tag = 2);

SELECT * into TRAIN_NEW_SPLIT3 FROM TRAIN_NEW tn
WHERE tn.id in (select ibt.id from ID_BIN_TABLE ibt where ibt.id_tag = 3);

SELECT * into TRAIN_NEW_SPLIT4 FROM TRAIN_NEW tn
WHERE tn.id in (select ibt.id from ID_BIN_TABLE ibt where ibt.id_tag = 4);

SELECT * into TRAIN_NEW_SPLIT5 FROM TRAIN_NEW tn
WHERE tn.id in (select ibt.id from ID_BIN_TABLE ibt where ibt.id_tag = 5);

SELECT * into TRAIN_NEW_SPLIT6 FROM TRAIN_NEW tn
WHERE tn.id in (select ibt.id from ID_BIN_TABLE ibt where ibt.id_tag = 6);

SELECT * into TRAIN_NEW_SPLIT7 FROM TRAIN_NEW tn
WHERE tn.id in (select ibt.id from ID_BIN_TABLE ibt where ibt.id_tag = 7);

SELECT * into TRAIN_NEW_SPLIT8 FROM TRAIN_NEW tn
WHERE tn.id in (select ibt.id from ID_BIN_TABLE ibt where ibt.id_tag = 8);

SELECT * into TRAIN_NEW_SPLIT9 FROM TRAIN_NEW tn
WHERE tn.id in (select ibt.id from ID_BIN_TABLE ibt where ibt.id_tag = 9);

SELECT * into TRAIN_NEW_SPLIT10 FROM TRAIN_NEW tn
WHERE tn.id in (select ibt.id from ID_BIN_TABLE ibt where ibt.id_tag = 10);

-- After this need to 'Manually' export these tables from 1 to 10 into CSV format and store them under the directory - '/data_CSV/splits_10/old_without_valid_time/', and run add_valid_time_to_train_test.R after that