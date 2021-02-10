%% MLOG file format: 
% column 1: raw RT from visual onset
% column 3: good trial = 1, missed or early response = -2
% column 4: frame offset at 60Hz (sync = -4 because of inertial onset delay)

clearvars
restoredefaultpath
addpath(genpath('D:\Data\Documents\Scripts\NBA2021'));

T = readtable('React4Subjects.csv'); % consider excluding sona subjects
keepSubject = ~(logical(T.below90)==1 | logical(T.poorData)==1); % only include subjects with over 90% response rate and not a fast guesser
S = T(keepSubject,:);

oldCodes = [100 -10 -7 -4 -1 2 200]; % codes in rawData
newCodes = [-Inf -100 -50 0 50 100 Inf]; % new codes in react4Data

%% DATA READER
clearvars react4*
data_append = [];
for si = 1:size(S,1)
    
    file_append = [];
    for bi = 1:2
        
        file = ['react' num2str(S.sID(si),'%03d') '_' num2str(bi) '_0.MLOG'];
        raw = dlmread(file);
        
        % remove trials that were an error
        bad1 = raw(:,3) == -2; % index row with response error = -2
    	if size(raw,2)>4
            bad2 = raw(:,5) ~= 0; % index rows longer than 4
            omitted = bad1 | bad2;
        else
            omitted = bad1; % use later to convert bad trials to very high RT
        end
        raw(omitted,:) = [];
        
        % adjust the SOA
        soa = (raw(:,4)+4)*(1000/60)'; % turns the frame offset into an SOA in ms. Frame offset of -4 is when cues are sync. Add 4 to get 0 offset at sync, then scale by frame rate (60Hz) to get SOA in ms 
        soa(raw(:,4)==100,1) = -Inf; % set vestibular only to SOA of -Inf
        soa(raw(:,4)==200,1) = Inf; % set visual only to SOA of Inf
        
        % adjust the RT
        adjIdx = ismember(raw(:,4),[-7, -10]); % this gets the index of vestibular first SOAs
        adjRTs = raw(:,1)-soa(:,1); % the RT timer started at the visual cue, so for vestibular first SOAs we need to adjust the RT
        rts = raw(:,1); % first set the RTs to the raw values
        rts(adjIdx) = adjRTs(adjIdx); % then adjust vestiublar first RTs to account for frame offset
        
        new = zeros(size(raw,1),4);
        new(:,1) = S.sID(si);
        new(:,2) = bi;
        new(:,3) = soa;
        new(:,4) = rts;
        
        file_append = vertcat(file_append, new);
    end
    
    data_append = vertcat(data_append, file_append);
end

r4Data = data_append;

%% Subject Mean RTs
clearvars subject*
for ci = 1:7 % condition index
    
    grandAll{1,ci} = r4Data(r4Data(:,3) == newCodes(ci), 4);
    grandRT(1,ci) = mean(r4Data(r4Data(:,3) == newCodes(ci), 4));
    
    for si = 1:size(S,1) % subject index 
        
        subjectData = r4Data(r4Data(:,1)==S.sID(si),:);

        tempSubjectRTs = subjectData(subjectData(:,3)==newCodes(ci),4);
        subjectAll{si,ci} = tempSubjectRTs;
        subjectRT(si,ci) = mean(tempSubjectRTs);
        
        for bi = 1:2 % block index
            
            blockData = r4Data(r4Data(:,1)==S.sID(si) & r4Data(:,2)==bi,:);
            
            tempBlockRTs = blockData(blockData(:,3)==newCodes(ci),4);
            subjectBlockAll{si,bi,ci} = tempBlockRTs;
            subjectBlockRT(si,bi,ci) = mean(tempBlockRTs);
        end
    end
end

% determine bad subjects
% below90p = S.sID(sum(subjectTrials(:,2:end),2) < (30*6)*(27/30));
% fastGuesser = S.sID(subjectRT(:,1) < subjectRT(:,2));

%% Race Model Inequality
% Convert response times to CDFs
timecourse = 0:0.1:3000;

%% Create CDFs from overall distribution
for ci = 1:7
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
end

grandCDFs(isnan(grandCDFs(1,:,1:10000))) = 0; % set starting values to zero
grandCDFves = grandCDFs(1,1,:);
grandCDFvis = grandCDFs(1,7,:);
subjectCDFs(isnan(subjectCDFs(:,:,1:10000))) = 0; % set starting values to zero
subjectCDFves = subjectCDFs(:,1,:);
subjectCDFvis = subjectCDFs(:,7,:);

soas = [-100 -50 0 50 100];

%% Calculate the Race Model Inequality, Race Model Violation Area, and scalar RMV score
for soai = 1:5 % calculate upper bound of race model for each SOA

    mySoa = soas(soai);
    colShift = abs(fix(mySoa*10));
    
    if mySoa < 0 % vestibular first
        grandRaceModel(1,soai,:) = grandCDFves(1,:) + [zeros(1,colShift) grandCDFvis(1,1:end-colShift)]; % shift visual CDF based on vestibular-first SOA
    elseif mySoa == 0 % sync
        grandRaceModel(1,soai,:) = grandCDFves(1,:) + grandCDFvis(1,:); % do not shift CDFs
    elseif mySoa > 0 % visual first
        grandRaceModel(1,soai,:) = grandCDFvis(1,:) + [zeros(1,colShift) grandCDFves(1,1:end-colShift)]; % shift vestibular CDF base on visual-first SOA
    end
    
    grandRaceModel(grandRaceModel>1)=NaN; % cap values at NaN
    grandViolation(1,soai,:) = grandCDFs(1,soai+1,:)-grandRaceModel(1,soai,:);
    
    temp = grandViolation(1,soai,:);
    grandRMV(1,soai) = sum(temp(temp>=0)) * 0.1; % multiply by bin width (0.1ms) to get area in ms
    
    for si = 1:size(S,1) % calculate upper bound race model for each subject
        if mySoa < 0 % vestibular
            subjectRaceModel(si,soai,:) = subjectCDFves(si,:) + [zeros(1,colShift) subjectCDFvis(si,1:end-colShift)]; % shift visual CDF based on vestibular-first SOA
        elseif mySoa == 0 % sync
            subjectRaceModel(si,soai,:) = subjectCDFves(si,:) + subjectCDFvis(si,:);  % do not shift CDFs
        elseif mySoa > 0 % visual
            subjectRaceModel(si,soai,:) = subjectCDFvis(si,:) + [zeros(1,colShift) subjectCDFves(si,1:end-colShift)]; % shift vestibular CDF base on visual-first SOA
        end
    
        subjectRaceModel(subjectRaceModel>1)=NaN; % cap values at NaN
        subjectViolation(si,soai,:) = subjectCDFs(si,soai+1,:)-subjectRaceModel(si,soai,:);
    
        temp = subjectViolation(si,soai,:);
        subjectRMV(si,soai) = sum(temp(temp>=0)) * 0.1; % multiply by bin width (0.1ms) to get area in ms
    end
end

%% Calculate Multisensory Responsement Enhancement (MRE) scores 
soaVes = [0 0 0 50 100];
soaVis = [100 50 0 0 0];

% RTstar is RT divided by Detection Rate (Rach et al. 2011)
grandRTstar = grandRT ./ (1-grandMR); 
subjectRTstar = subjectRT ./ (1-subjectMR);

clearvars *MRE
for soai = 1:5 % MRE overall
    
    grandMinUni = min(grandRTstar(1)+soaVes(soai),grandRTstar(7)+soaVis(soai));
    grandMRE(soai) = ((grandMinUni-grandRTstar(soai+1))/grandMinUni)*100;
    
    for si = 1:size(S,1) % MRE by subject
        subjectMinUni = min(subjectRTstar(si,1)+soaVes(soai),subjectRTstar(si,7)+soaVis(soai));
        subjectMRE(si,soai) = ((subjectMinUni-subjectRTstar(si,soai+1))/subjectMinUni)*100;
    end
end
clearvars *MinUni

%% Finally, make a csv file with 7 rows per subject (1 per condition)
X.sID = repelem(S.sID,7);
X.age = repelem(S.age,7);
X.gender = repelem(S.gender,7);
X.cond = repmat(["Ves","Com","Com","Com","Com","Com","Vis"]',size(S,1),1);
X.soa = repmat([-Inf,-100,-50,0,50,100,Inf]',size(S,1),1);
X.rt = reshape(subjectRT',[size(S,1)*7,1]);
X.mr = reshape(subjectMR',[size(S,1)*7,1]);
X.rmv = reshape([nan(size(S,1),1) subjectRMV nan(size(S,1),1)]',[size(S,1)*7,1]);
X.mre = reshape([nan(size(S,1),1) subjectMRE nan(size(S,1),1)]',[size(S,1)*7,1]);
X.gameHr = repelem(S.videoGameHours,7);
X.driveHr = repelem(S.drivingHours,7);
X.compHr = repelem(S.computerHours,7);
csvData = struct2table(X);
writetable(csvData, 'React4Data.csv')

clearvars -except S *CDF* *RaceModel* grand* timecourse
save CDFs
