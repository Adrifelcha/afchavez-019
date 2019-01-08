%% Kappa Coefficient of Agreement (Example 5.3)

clear;

%% Data
dataset = 1;

switch dataset
    case 1, y = [14 4 5 210];   % Influenza
    case 2, y = [20 7 103 417]; % Hearing Loss
    case 3, y = [0 0 13 157];   % Rare Disease
end;

%% Constants
n = sum(y);

%% Trinity constants

% Graphical model script
modelName = 'Kappa';

% Parameters to monitor
params = {'kappa', 'xi', 'psi', 'alpha', 'beta', 'gamma', 'pi'};

% MCMC properties
nChains    = 3;   % number of MCMC chains
nBurnin    = 1e3; % number of discarded burn-in samples
nSamples   = 1e3; % number of collected samples
nThin      = 1;   % number of samples between those collected
doParallel = 0;   % whether MATLAB parallel toolbox parallizes chains

% Assign MATLAB variables to the observed nodes
data = struct('y', y, ...
              'n', n);

% Generator for initialization
generator = @()struct('alpha' , rand, ...
                      'beta'  , rand, ...
                      'gamma' , rand);

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

