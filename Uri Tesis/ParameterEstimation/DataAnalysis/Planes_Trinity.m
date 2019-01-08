%%  Planes (Example 5.6)

clear;

%% Data
x = 10;     % Number in first sample
k = 4;      % Number "re-captured" in second sample
n = 5;      % Size of second sample
tmax = 50;  % Total possible number

%% Constants
minPossible = (x + n - k);  % Logically minimum possible number

%% Trinity constants

% Graphical model script
modelName = 'PlanesJ';        % Note JAGS script, using dinterval

% Parameters to monitor
params = {'t'};

% MCMC properties
nChains    = 3;   % number of MCMC chains
nBurnin    = 1e3; % number of discarded burn-in samples
nSamples   = 2e3; % number of collected samples
nThin      = 1;   % number of samples between those collected
doParallel = 0;   % whether MATLAB parallel toolbox parallizes chains

% Assign MATLAB variables to the observed nodes
data = struct('k', k, ...
    'x', x, ...
    'n', n, ...
    'tmax', tmax);

% Generator for initialization
generator = @()struct('t', ceil(rand * (tmax  - minPossible) + minPossible));

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
bins = x + n - k : tmax;    % (x + n - k) is minimum logically possible number for t

% Histogram counts
count = histc(chains.t(:), bins);  % histogram counts
count = count/sum(count);          % normalize

% Figure
figure(1); clf; hold on;
set(gcf, ...
    'color'             ,            'w' , ...
    'units'             ,   'normalized' , ...
    'position'          , [.2 .2 .6 .4]  , ...
    'paperpositionmode' ,         'auto' );

% Axes
set(gca, ...
    'xlim'              , [minPossible-1 tmax+1] , ...
    'xtick'             , [minPossible tmax] , ...
    'box'               ,              'off' , ...
    'tickdir'           ,              'out' , ...
    'fontsize'          ,                14  );

% Labels
xlabel('Number of planes', 'fontsize', 16);
ylabel('Posterior mass', 'fontsize',16);

% Plot histogram of posterior mass
bar(bins, count, ...
    'facecolor', 'k');
