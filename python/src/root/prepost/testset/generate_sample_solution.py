'''
Created on Oct 24, 2015

@author: vaibhav
'''


import pyodbc
import csv
import decimal

cnxn = pyodbc.connect('DRIVER={SQL Server};SERVER=BATMAN;DATABASE=KaggleRain;UID=sa;PWD=Password01')
outfile="../../../../../data_CSV/sample_solution_test.csv"

of = open(outfile, 'w', newline='')
csvOut = csv.writer(of, delimiter=',')

cursor = cnxn.cursor()
hourIdLst = [];
cursor.execute("SELECT id from TEST group by id order by id")
for row in cursor.fetchall():
    hourIdLst.append(str(row[0]))


for hourId in hourIdLst:
    cursor.execute("SELECT minutes_past,ref from TEST where id=" + hourId + " order by minutes_past")
    minPastList = []
    refList = []
    for row in cursor.fetchall():
        minPastList.append(row[0])
        refList.append(row[1])
    
    lenMinPastList = len(minPastList)
    validTimeList = [None]*(lenMinPastList)
    validTimeList[0] = minPastList[0]
    for i in range(1,lenMinPastList): # get time intervals for which ref is valid
        validTimeList[i] = minPastList[i] - minPastList[i-1]
    
    validTimeList[lenMinPastList-1] = validTimeList[lenMinPastList-1] + 60 - sum(validTimeList)
    validTimeList[:] = [x / 60 for x in validTimeList]
    
    summ=0
    
    for i in range(0,lenMinPastList):
        dbz = refList[i];
        hours = validTimeList[i];
        if dbz not in (None, ""):
            mmperhr = pow(pow(10, dbz/10)/200, 0.625);
            summ = summ + decimal.Decimal(mmperhr)*hours;

    
    #if int(hourId) % 5 == 0:
    print(hourId)
    csvOut.writerow([hourId,summ])



##print(', '.join(hourIdLst));
of.close()
    
