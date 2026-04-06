%change first number after Tfinal to the cluster you want
Q = table(datestr(Tfinal{50, 7}), 'VariableNames', {'Date'}); 

%% 

writetable(Q, 'bb_2016_c50.csv') 

% %%
% 
% outFolder = "C:\Users\allisonpayne/Local Documents/Github/Manuscripts/beakedwhales/bw-seasonality/data/raw";
% 
% % Loop through rows of Tfinal
% for i = 1:height(Tfinal)
% 
%     % Create the table for this row
%     Q = table(datestr(Tfinal{i, 7}), 'VariableNames', {'Date'});
% 
%     % Create full filename (e.g., "1.csv")
%     fileName = fullfile(outFolder, sprintf('%d.csv', i));
% 
%     % Write the table
%     writetable(Q, fileName);
% 
% end
% 
% %%
% 
% % Preallocate cell array for dates
% allDates = cell(height(Tfinal),1);
% 
% % Loop through rows and convert to string dates
% for i = 1:height(Tfinal)
%     allDates{i} = datestr(Tfinal{i,7});
% end
% 
% % Build final table: Row number + Date string
% Q = table((1:height(Tfinal))', allDates, ...
%           'VariableNames', {'RowNumber','Date'});
% 
% % Output file
% outFile = "C:\Users\allisonpayne/Local Documents/Github/Manuscripts/beakedwhales/bw-seasonality/";
% 
% % Save one combined CSV
% writetable(Q, outFile);
% outFolder = "C:\Users\allisonpayne/Local Documents/Github/Manuscripts/beakedwhales/bw-seasonality/data/raw";
% 
% % Loop through rows of Tfinal
% for i = 1:height(Tfinal)
% 
%     % Create the table for this row
%     Q = table(datestr(Tfinal{i, 7}), 'VariableNames', {'Date'});
% 
%     % Create full filename (e.g., "1.csv")
%     fileName = fullfile(outFolder, sprintf('%d.csv', i));
% 
%     % Write the table
%     writetable(Q, fileName);
% 
% end
% 
% %%
% 
% % Preallocate cell array for dates
% allDates = cell(height(Tfinal),1);
% 
% % Loop through rows and convert to string dates
% for i = 1:height(Tfinal)
%     allDates{i} = datestr(Tfinal{i,7});
% end
% 
% % Build final table: Row number + Date string
% Q = table((1:height(Tfinal))', allDates, ...
%           'VariableNames', {'RowNumber','Date'});
% 
% % Output file
% outFile = "C:\Users\allisonpayne/Local Documents/Github/Manuscripts/beakedwhales/bw-seasonality/";
% 
% % Save one combined CSV
% writetable(Q, outFile);