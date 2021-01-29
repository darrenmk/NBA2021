%% MLOG file format: 
% column 1: raw RT from visual onset
% column 3: good trial = 1, missed or early response = -2
% column 4: frame offset at 60Hz (sync = -4 because of inertial onset delay)

clearvars
restoredefaultpath
addpath(genpath('E:\Documents\Projects\React4\'));

T = readtable('React4Subjects.csv'); % consider excluding sona subjects
keepSubject = ~(logical(T.below90)==1 | logical(T.poorData)==1); % only include subjects with over 90% response rate and not a fast guesser
S = T(keepSubject,:);

%% Age groups
%logical indices for age groups (change later to have an approximately equal distribution)
g1 = S.age < 20;
g2 = S.age >= 20 & S.age < 40;
g3 = S.age >= 40 & S.age < 55;
g4 = S.age >= 55;
groups = [g1,g2,g3,g4];

oldCodes = [100 -10 -7 -4 -1 2 200]; % codes in rawData
newCodes = [-Inf -100 -50 0 50 100 Inf]; % new codes in react4Data

%% DATA READER
clearvars react4*
for si = 1:size(S,1)
    
    clearvars sid group block cond rts
    gi = find(groups(si,:));
    
    for bi = 1:2
        clearvars soa
        
        file = ['react' num2str(S.sID(si),'%03d') '_' num2str(bi) '_0.MLOG'];
        raw = dlmread(file);
        
        %logical indexing
        bad1 = raw(:,3) == -2; % index row with response error = -2
    	if size(raw,2)>4
            bad2 = raw(:,5) ~= 0; % index rows longer than 4
            omitted = bad1 | bad2;
        else
            omitted = bad1; % use later to convert bad trials to very high RT
        end
        raw(omitted,:) = [];
        
        vesIdx = raw(:,4)==100; % vestibular only index
        visIdx = raw(:,4)==200; % visual only index
        soaIdx = raw(:,4)<99; % soa index
        
        soa(soaIdx,1) = (raw(soaIdx,4)+4)*(1000/60)'; % adjust for frame offset and convert to SOA
        soa(vesIdx,1) = -Inf;
        soa(visIdx,1) = Inf;
        
        adjIdx = ismember(raw(:,4),[-7, -10]);
        rawIdx = ~adjIdx;
        
        adjRTs = raw(:,1)-soa(:,1);
        
        rts = raw(:,1);
        rts(adjIdx) = adjRTs(adjIdx);
        
        s(1:105,bi) = NaN;
        s(1:length(raw),bi) = si;
        
        g(1:105,bi) = NaN;
        g(1:length(raw),bi) = gi;
        
        b(1:105,bi) = NaN;
        b(1:length(raw),bi) = bi;
        
        c(1:105,bi) = NaN;
        c(1:length(raw),bi) = soa;
        
        r(1:105,bi) = NaN;
        r(1:length(raw),bi) = rts;
    end
    
    %% Make output file!
    missingTrials = 210-sum(sum(~isnan(r)));
    subjectRows = (210*si-209: 210*si-missingTrials)';
    
    % make by group as well !
    myData(subjectRows,1) = s(~isnan(s)); % subject id
    myData(subjectRows,2) = g(~isnan(g)); % group number
    myData(subjectRows,3) = b(~isnan(b)); % block number
    myData(subjectRows,4) = c(~isnan(c)); % condition
    myData(subjectRows,5) = r(~isnan(r)); % RT
    
end

r4Data = myData; % make sure to evaluate this first
r4Data(~any(r4Data,2),:) = []; % do this to get rid of empty rows

%% Outlier removal (commented out because there are no outliers in the data)
%{
for ci = 1:7
    
    idx = r4Data(:,3)==newCodes(ci);
    tempArray = r4Data(idx,4); % these are the RTs
    tempArray2 = log(tempArray); % (natural) log transformed RTs to make data normally distributed
    
    % OUTLIER REMOVAL PER CONDITION
    lowOutliers = tempArray2 < quantile(tempArray2,0.25) - iqr(tempArray2)*3; % index low outliers
    highOutliers = tempArray2 > quantile(tempArray2,0.75) + iqr(tempArray2)*3; % index high outliers
    allOutliers = highOutliers | lowOutliers;
    
    idx(idx) = allOutliers;
    
    idx2(:,ci) = idx;
    nOutliers(ci) = sum(allOutliers);
end

outliers = logical(sum(idx2,2)); % collapse into a single logical vector
r4Data(outliers,:) = []; % be careful about running this in isolation
%}

%% Subject means
for si = 1:size(S,1)

    subjectData = r4Data(r4Data(:,1)==si,:);

    for ci = 1:7 % subject condition index
        
        conditionIndex = subjectData(:,4)==newCodes(ci);
        conditionRTs = subjectData(conditionIndex,5);
        subjectAll{si,ci} = conditionRTs;
        subjectRT(si,ci) = mean(conditionRTs);
        
        for bi = 1:2 % subject condition block index
            
            blockData = r4Data(r4Data(:,1)==si & r4Data(:,3)==bi,:);
            blockIndex = blockData(:,4)==newCodes(ci);
            blockRTs = blockData(blockIndex,5);
            
            subjectBlockAll{si,bi,ci} = blockRTs;
            subjectBlockRT(si,bi,ci) = mean(blockRTs);
        end
    end
end

% Run these here to determine bad subjects
% below90p = S.sID(sum(subjectTrials(:,2:end),2) < (30*6)*(27/30));
% fastGuesser = S.sID(subjectRT(:,1) < subjectRT(:,2));

%% Grand and group means
for ci = 1:7
    
    conditionData = r4Data(r4Data(:,4)==newCodes(ci),:);
    
    grandAll{1,ci} = conditionData(:,5);
    grandRT(1,ci) = mean(conditionData(:,5));
    grandSe(1,ci) = std(conditionData(:,5))/sqrt(size(S,1));
    
    for gi = 1:size(groups,2)
        
        groupData = conditionData(conditionData(:,2)==gi,:);
        
        groupAll{gi,ci} = groupData(:,5);
        groupRT(gi,ci) = mean(groupData(:,5));
        groupSe(gi,ci) = std(groupData(:,5))/sqrt(sum(groups(:,gi)));
    end
end
clearvars -except S r4Data subject* grand* group* newCodes

%% Convert response times to CDFs to calculate race model inequality
timecourse = 0:0.1:3000;

for ci = 1:7 % convert grandAll to grandCDF
    nInf = size(S,1)*30 - length(grandAll{ci}); % determine number of missing trials
    grandInf{1,ci} = vertcat(grandAll{ci},inf(nInf,1)); % set missing trials to infinity to correct CDF
    grandMR(ci) = (sum(~isfinite(grandInf{1,ci})))/(size(S,1)*30);
    
    [F,t] = ecdf(grandInf{1,ci});
    [tUnique, tUniqueIdx] = unique(t); 
    grandCDFs(1,ci,:) = interp1(tUnique(1:end-1), F(tUniqueIdx(1:end-1)), timecourse, 'previous');
    
    for si = 1:size(S,1) % convert subjectAll to subjectCDF
        nInf = 30 - length(subjectAll{si,ci});
        if length(subjectAll{si,ci}) >= 2
            subjectInf{si,ci} = vertcat(subjectAll{si,ci},inf(nInf,1));
            subjectMR(si,ci) = sum(~isfinite(subjectInf{si,ci}))/30;
        elseif length(subjectAll{si,ci}) == 1
            subjectInf{si,ci} = vertcat(subjectAll{si,ci},2999,inf(nInf-1,1)); % add dummy second trial
            subjectMR(si,ci) = 29/30;
        else
            subjectInf{si,ci} = vertcat(2998,2999,inf(nInf-2,1)); % add two dummy trials
            subjectMR(si,ci) = 30/30;
        end
        
        [F,t] = ecdf(subjectInf{si,ci});
        [tUnique, tUniqueIdx] = unique(t); 
        subjectCDFs(si,ci,:) = interp1(tUnique(1:end-1), F(tUniqueIdx(1:end-1)), timecourse, 'previous');
        
    end
    
    for gi = 1:size(groups,2) % convert groupAll to groupCDF
        nInf = sum(groups(:,gi))*30 - length(groupAll{gi,ci});
        groupInf{gi,ci} = vertcat(groupAll{gi,ci},inf(nInf,1));
        groupMR(gi,ci) = (sum(~isfinite(groupInf{gi,ci})))/(sum(groups(:,gi))*30);
        
        [F,t] = ecdf(groupInf{gi,ci});
        [tUnique, tUniqueIdx] = unique(t); 
        groupCDFs(gi,ci,:) = interp1(tUnique(1:end-1), F(tUniqueIdx(1:end-1)), timecourse, 'previous');
        
    end
end

grandCDFs(isnan(grandCDFs(1,:,1:10000))) = 0; % set starting values to zero
grandCDFves = grandCDFs(1,1,:);
grandCDFvis = grandCDFs(1,7,:);
subjectCDFs(isnan(subjectCDFs(:,:,1:10000))) = 0; % set starting values to zero
subjectCDFves = subjectCDFs(:,1,:);
subjectCDFvis = subjectCDFs(:,7,:);
groupCDFs(isnan(groupCDFs(:,:,1:10000))) = 0; % set starting values to zero
groupCDFves = groupCDFs(:,1,:);
groupCDFvis = groupCDFs(:,7,:);

soas = [-100 -50 0 50 100];

for soai = 1:5 % calculate upper bound of race model

    mySoa = soas(soai);
    colShift = abs(fix(mySoa*10));
    
    if mySoa < 0 % vestibular first
        grandRaceModel(1,soai,:) = grandCDFves(1,:) + [zeros(1,colShift) grandCDFvis(1,1:end-colShift)];
    elseif mySoa == 0 % sync
        grandRaceModel(1,soai,:) = grandCDFves(1,:) + grandCDFvis(1,:);
    elseif mySoa > 0 % visual first
        grandRaceModel(1,soai,:) = grandCDFvis(1,:) + [zeros(1,colShift) grandCDFves(1,1:end-colShift)];
    end
    
    grandRaceModel(grandRaceModel>1)=NaN; % cap values at NaN
    grandViolation(1,soai,:) = grandCDFs(1,soai+1,:)-grandRaceModel(1,soai,:);
    
    temp = grandViolation(1,soai,:);
    grandRMV(1,soai) = sum(temp(temp>=0)) * 0.1; % times bin width(0.1ms)
    
    for si = 1:size(S,1) % calculate upper bound race model for each subject
        if mySoa < 0 % vestibular
            subjectRaceModel(si,soai,:) = subjectCDFves(si,:) + [zeros(1,colShift) subjectCDFvis(si,1:end-colShift)];
        elseif mySoa == 0 % sync
            subjectRaceModel(si,soai,:) = subjectCDFves(si,:) + subjectCDFvis(si,:);
        elseif mySoa > 0 % visual
            subjectRaceModel(si,soai,:) = subjectCDFvis(si,:) + [zeros(1,colShift) subjectCDFves(si,1:end-colShift)];
        end
    
        subjectRaceModel(subjectRaceModel>1)=NaN; % cap values at NaN
        subjectViolation(si,soai,:) = subjectCDFs(si,soai+1,:)-subjectRaceModel(si,soai,:);
    
        temp = subjectViolation(si,soai,:);
        subjectRMV(si,soai) = sum(temp(temp>=0)) * 0.1; % times bin width (0.1ms)
    end
    
    for gi = 1:size(groups,2) % calculate upper bound race model for each group
        if mySoa < 0 % vestibular
            groupRaceModel(gi,soai,:) = groupCDFves(gi,:) + [zeros(1,colShift) groupCDFvis(gi,1:end-colShift)];
        elseif mySoa == 0 % sync
            groupRaceModel(gi,soai,:) = groupCDFves(gi,:) + groupCDFvis(gi,:);
        elseif mySoa > 0 % visual
            groupRaceModel(gi,soai,:) = groupCDFvis(gi,:) + [zeros(1,colShift) groupCDFves(gi,1:end-colShift)];
        end
    
        groupRaceModel(groupRaceModel>1)=NaN; % cap values at NaN
        groupViolation(gi,soai,:) = groupCDFs(gi,soai+1,:)-groupRaceModel(gi,soai,:);
        
        temp = groupViolation(gi,soai,:);
        groupRMV(gi,soai) = sum(temp(temp>=0)) * 0.1; % times bin width (0.1ms)
    end
end

%% Index of coactivation 
%CRE = crossmodal response enhancement (Colonius and Diederich 2017)
soaVes = [0 0 0 50 100];
soaVis = [100 50 0 0 0];

clearvars *CRE
for soai = 1:5 % overall
    grandMinUni = min(grandRT(1)+soaVes(soai),grandRT(7)+soaVis(soai));
    grandCRE(soai) = ((grandMinUni-grandRT(soai+1))/grandMinUni)*100; % this is the same as taking the AUC!
    
    for si = 1:size(S,1) % by subject
        subjectMinUni = min(subjectRT(si,1)+soaVes(soai),subjectRT(si,7)+soaVis(soai));
        subjectCRE(si,soai) = ((subjectMinUni-subjectRT(si,soai+1))/subjectMinUni)*100;
    end
    
    for gi = 1:size(groups,2) % by group
        groupMinUni = min(groupRT(gi,1)+soaVes(soai),groupRT(gi,7)+soaVis(soai));
        groupCRE(gi,soai) = ((groupMinUni-groupRT(gi,soai+1))/groupMinUni)*100;
    end
end
clearvars *MinUni

clearvars -except S r4Data subject* g* timecourse
save /home/darren/Documents/Projects/React4/Scripts/React4Variables
