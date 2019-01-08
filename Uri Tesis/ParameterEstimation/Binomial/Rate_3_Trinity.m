%% Inferring A Common Rate (Example 3.3)

clear;

%% Data
k1 = 5;
n1 = 10;
k2 = 7;
n2 = 10;

%% Trinity constants

% Graphical model script
modelName = 'Rate_3';

% Parameters to monitor
params = {'theta'};

% MCMC properties
nChains    = 3;   % number of MCMC chains
nBurnin    = 0;   % number of discarded burn-in samples
nSamples   = 5e3; % number of collected samples
nThin      = 1;   % number of samples between those collected
doParallel = 0;   % whether MATLAB parallel toolbox parallizes chains

% Assign MATLAB variables to the observed nodes
data = struct('k1', k1, ...
              'n1', n1, ...
              'k2', k2, ...
              'n2', n2);

% Generator for initialization
generator = @()struct('theta', rand);

% Which engine to use
engine = 'jags';

%% Sample using Trinity

tic; % start clock
[stats, chains, diagnostics, info] = callbayes(engine, ...
    'model'           , [modelName '.txt']                        , ...
    'data'            , data                                      , ...
    'outputname'      , 'samples'                                 , ...
    'init'            , generator                                 , ...
    'datafilename'    , modelName                                 , ...
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

% Drawing constants
lo   = 0;     % lower bound
hi   = 1;     % upper bound
step = 0.01;  % bins width

% Bins for histograms
binCenters = lo + step/2 : step : hi - step/2; % bin centers
binEdges   = lo : step : hi;                   % bin edges

% Histogram counts
count = histc(chains.theta(:), binEdges);  % histogram counts
count = count(1 : end-1);                  % remove extra bin count at end from histc
count = count/sum(count)/step;             % scale according to total samples and bin width to get density

% Figure
figure(3); clf; hold on;
set(gcf, ...
    'color'             ,            'w' , ...
    'units'             ,   'normalized' , ...
    'position'          , [.2 .2 .6 .6]  , ...
    'paperpositionmode' ,         'auto' );

% Axes
set(gca, ...
    'xtick'             ,   -1 : 0.2 : 1  , ...
    'box'               ,          'off' , ...
    'tickdir'           ,          'out' , ...
    'fontsize'          ,            14  );

% Labels
xlabel('Rate \theta', 'fontsize', 16);
ylabel('Posterior density', 'fontsize',16);

% Plot density
plot(binCenters, count, 'k-');


