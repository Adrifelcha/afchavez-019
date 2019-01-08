%% Cheating (Example 6.7)

clear;

%% Data
load Cheatingdata d truez;

%% Constants
k = sum(d, 2);  % Total correct per participant
p = length(k);  % Number of participants
n = 40;         % Total trials

%% Trinity constants

% Graphical model script
modelName = 'Cheating';

% Parameters to monitor
params = {'z', 'phi', 'mubon', 'muche', 'lambdabon', 'lambdache', 'pc'};

% MCMC properties
nChains    = 3;   % number of MCMC chains
nBurnin    = 1e3; % number of discarded burn-in samples
nSamples   = 2e3; % number of collected samples
nThin      = 1;   % number of samples between those collected
doParallel = 0;   % whether MATLAB parallel toolbox parallizes chains

% Assign MATLAB variables to the observed nodes
data = struct('k', k, ...
    'p', p, ...
    'n', n, ...
    'truth', truez);

% Generator for initialization
generator = @()struct('z', round(rand(1, p)), ...
    'phi', 0.5, ...
    'mubon', 0.5, ...
    'mudiff', 0.1, ...
    'lambdabon', 10, ...
    'lambdache', 10);

% Which engine to use
engine = 'jags';

%% Sample using Trinity

tic; % start clock
[stats, chains, diagnostics, info] = callbayes(engine, ...
    'model'           , [modelName '.txt']                        , ...
    'data'            , data                                      , ...
    'outputname'      , 'samples'                                 , ...
    'init'            , generator                                 , ...
    'allowunderscores', 1                                         , ...
    'initfilename'    , modelName                                 , ...
    'scriptfilename'  , modelName                                 , ...
    'logfilename'     , modelName                                 , ...
    'nchains'         , nChains                                   , ...
    'nburnin'         , nBurnin                                   , ...
    'nsamples'        , nSamples                                  , ...
    'monitorparams'   , params                                    , ...
    'thin'            , nThin                                     , ...
    'workingdir'      , ['/tmp/' modelName]                       , ...
    'verbosity'       , 0                                         , ...
    'saveoutput'      , true                                      , ...
    'parallel'        , doParallel                                , ...
    'modules'         , {'dic'} );
fprintf('%s took %f seconds!\n', upper(engine), toc); % show timing

%% Inspect the results
% First, inspect the convergence of each parameter
disp('Convergence statistics:')
grtable(chains, 1.05)

% Now check some basic descriptive statistics averaged over all chains
disp('Descriptive statistics for all chains:')
codatable(chains)

%% Analysis

% Constants
lo   = 0;                  % lower bound for proportion correct
hi   = 100;                  % upper bound for proportion correct
step = 1;               % bins width for proportion correct
scale = 5;              % histogram scaling

% Bins for histograms
binCenters = lo + step/2 : step : hi - step/2; % bin centers for proportion correct
binEdges   = lo : step : hi;                   % bin edges for proportion correct
binsQuestions = 0 : n;

% Counts
countBonafide = hist(k(truez == 0), binsQuestions);
countCheater  = hist(k(truez == 1), binsQuestions);
countPropotionCorrect = histc(chains.pc(:), binEdges);                           % histogram counts
countPropotionCorrect = countPropotionCorrect(1 : end-1);                        % remove extra bin count at end from histc
countPropotionCorrect = countPropotionCorrect/max(countPropotionCorrect);        % normalize to maximum of 1

% Accuracy of classification for different thresholds
proportionCorrect = zeros(size(binsQuestions));
for i = 1 : length(binsQuestions)
    t = zeros(p, 1);
    t(k >= binsQuestions(i)) = 1;                     % called cheaters at threshold bins(i)
    proportionCorrect(i) = sum(t == truez);    % total correctly classified
end;
proportionCorrect = proportionCorrect/p; 

% Data and model performance figure
figure(1); clf;
set(gcf, ...
    'color'             ,            'w' , ...
    'units'             ,   'normalized' , ...
    'position'          ,  [.2 .2 .6 .6] , ...
    'paperpositionmode' ,         'auto' );

% Data space axes
subplot(2, 1, 1); cla; hold on;
set(gca, ...
    'xlim'              ,        [0 n+1] , ...
    'xtick'             ,     5 : 5 : n  , ...
    'box'               ,          'off' , ...
    'tickdir'           ,          'out' , ...
    'fontsize'          ,            14  );

% Labels
ylabel('Number of people', 'fontsize',16);

% Data distribution
H = bar(binsQuestions, [countBonafide; countCheater]');
set(H(1), 'facecolor', 'k');
set(H(2), 'facecolor', 'w');
set(H, 'barwidth', 1);

% Legend
L = legend('Bona fide', 'Cheater', ...
           'location', 'northwest');
set(L, 'box', 'off');

% Correct axes
subplot(2, 1, 2); cla; hold on;
set(gca, ...
    'xlim'              ,        [0 n+1] , ...
    'xtick'             ,     5 : 5 : n  , ...
    'ytick'             ,  0.4 : 0.1 : 1 , ...
    'ygrid'             ,           'on' , ...
    'box'               ,          'off' , ...
    'tickdir'           ,          'out' , ...
    'ticklength'        ,       [0.01 0] , ...
    'fontsize'          ,            14  );

% Labels
xlabel('Number of items recalled correctly',    'fontsize', 16);
ylabel('Proportion correct', 'fontsize', 16);

% Plot accuracy of classification over question correct thresholds
plot(binsQuestions, proportionCorrect,'k-', ...
    'linewidth', 2);

% Plot distribution of accuracy of model
for i = 1 : length(binCenters)
    if countPropotionCorrect(i) > 0
        plot([0 countPropotionCorrect(i) * scale], 1/p * ones(1,2) * binCenters(i), 'k-', ...
        'linewidth', 2, ...
            'color', 0.7*ones(1, 3));
    end;
end;
      
% Tidy
set(gca, 'layer', 'top');

% Utility and decision making figure
figure(2); clf; hold on;
set(gcf, ...
    'color'             ,            'w' , ...
    'units'             ,   'normalized' , ...
    'position'          ,  [.2 .2 .6 .6] , ...
    'paperpositionmode' ,         'auto' );

% Data space axes
axis([0 n+1 0 1]);
axis square;
set(gca, ...
    'xtick'             ,      5 : 5 : n , ...
    'ytick'             ,  0.1 : 0.1 : 1 , ...
    'box'               ,          'off' , ...
    'tickdir'           ,          'out' , ...
    'ticklength'        ,       [0.01 0] , ...
    'fontsize'          ,            14  );

% Labels
xlabel('Number of Items Recalled Correctly', 'fontsize', 16);
ylabel('Cheater Classification', 'fontsize', 16);

% Plot expected classification
plot(k, codatable(chains, 'z', @mean), 'kx', ...
    'markersize', 6, ...
    'linewidth', 1);

% Plot guiding linees
plot( [ 0 30]  , [.2 .2] , 'k--');
plot( [30 30]  , [ 0 .2] , 'k--');
plot( [ 0 35]  , [.5 .5] , 'k--');
plot( [35 35]  , [ 0 .5] , 'k--');