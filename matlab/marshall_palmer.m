function marshall_palmer()
% specify minutes_past and red values for each id

% values for Id#1
minutes_past = [1 5 8 12 15 19 23 26 30 33 37 41 44 48 51 55 59]';
ref = [nan 10 nan 14 10.5 8 6.5 16 7 12 nan nan nan nan nan nan nan]';

valid_time = zeros(length(minutes_past),1);
valid_time(1) = minutes_past(1);
for i=2:length(valid_time)
    valid_time(i) = minutes_past(i) - minutes_past(i-1);
end
valid_time(length(valid_time)) = valid_time(length(valid_time)) + 60 - sum(valid_time);
valid_time = valid_time/60;

summ = 0;

for i=1:length(valid_time)
    dbz = ref(i);
    hours = valid_time(i);
    if not(isnan(ref(i)))
        mmperhr = power(power(10, dbz/10)/200, 0.625);
        summ = summ + mmperhr*hours;
    end
end

disp(summ);
fprintf('%0.6f \n',summ);