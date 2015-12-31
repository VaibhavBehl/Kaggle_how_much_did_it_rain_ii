import sys
# import pydevd

# pydevd.settrace()
first = True;
curid = 0;
data = [];
count = 0;
outfh = open(sys.argv[2], "w");
with open(sys.argv[1]) as fh:
	for line in fh:
		count = count + 1;
		if first == True:
			first = False;
			continue;
		fields = line.strip().split(",");
		row_id = int(fields[0]);  # Id field
		if(count % 100000 == 0 or count>8022700):
			print('count -> ' + str(count));
			print('Row Id->' + str(row_id))
		if(not row_id == curid or count == 8022757):  # hardcoded last row value
			if(len(data) == 0):
				outfh.write(str(curid) + ",0.0\n");
			else: 
				data.sort(key=lambda x:x[0]);
				minutes = map(lambda x: x[0], data);
				ref = map(lambda x: x[1], data);
				#last = 60 - minutes[-1];
				minuteList = list(minutes);
				valid_time = [0] * len(minuteList);
				valid_time[0] = minuteList[0];
				for n in range(1,len(minuteList)):
					valid_time[n] = minuteList[n] - minuteList[n-1];
				valid_time[-1] = valid_time[-1] + 60 - sum(valid_time);
				total_rain = 0;
				for dbz, hours in zip(ref, valid_time):
					#if(dbz > 0.0):
					if dbz not in (None, ""):
						total_rain = total_rain +  ((pow(pow(10, dbz/10)/200, 0.625)) * hours /60.0);
				outfh.write(",".join([str(curid),str(total_rain)]) + "\n");
			data = [];
			curid = row_id;	
		minutes = int(fields[1]);
		if len(fields[3]) == 0:  # Ref field
			ref = None;
		else:
			ref = float(fields[3]);
		data.append((minutes, ref));
