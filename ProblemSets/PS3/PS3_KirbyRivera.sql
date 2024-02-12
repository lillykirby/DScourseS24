-- a) Read in CSV and import data
.mode csv
.import "/home/ouecon008/DScourseS24/ProblemSets/PS3/FL_insurance_sample.csv" insurance

-- b) First 10 rows
SELECT * FROM insurance LIMIT 10;

-- c) Unique counties
SELECT DISTINCT COUNTY from insurance;

-- d) Average property appreciation 2011 to 2012
SELECT AVG(tiv_2012 - tiv_2011) FROM insurance;

-- e) Construction frequency table
SELECT construction, COUNT(*) FROM insurance GROUP BY construction;