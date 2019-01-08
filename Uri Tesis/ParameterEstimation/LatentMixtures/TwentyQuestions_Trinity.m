%%  Twenty Questions (Example 6.3)

clear;

%% Data
dataset = 1;    % Choose the dataset

switch dataset
    case 1, % No missing values
        k = [1 1 1 1 0 0 1 1 0 1 0 0 1 0 0 1 0 1 0 0;
            0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0;
            0 0 1 0 0 0 1 1 0 0 0 0 1 0 0 0 0 0 0 0;
            0 0 0 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0 0 0;
            1 0 1 1 0 1 1 1 0 1 0 0 1 0 0 0 0 1 0 0;
            1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1 0 1 0 0;
            0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0;
            0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0;
            0 1 1 0 0 0 0 1 0 1 0 0 1 0 0 0 0 1 0 1;
            1 0 0 0 0 0 1 0 0 1 0 0 1 0 0 0 0 0 0 0];
    case 2, % Missing values
        k = [1 1 1 1 0 0 1 1 0 1 0 0 nan 0 0 1 0 1 0 0;
            0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0;
            0 0 1 0 0 0 1 1 0 0 0 0 1 0 0 0 0 0 0 0;
            0 0 0 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0 0 0;
            1 0 1 1 0 1 1 1 0 1 0 0 1 0 0 0 0 1 0 0;
            1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1 0 1 0 0;
            0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0;
            0 0 0 0 nan 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0;
            0 1 1 0 0 0 0 1 0 1 0 0 1 0 0 0 0 1 0 1;
            1 0 0 0 0 0 1 0 0 1 0 0 1 0 0 0 0 nan 0 0];
end;

%% Constants
[np,nq] = size(k); % Number of people and questions

%% Trinity constants

% Graphical model script
modelName = 'TwentyQuestions';

% Parameters to monitor
params = {'p', 'q', 'k'};

% MCMC properties
nChains    = 3;   % number of MCMC chains
nBurnin    = 1e3; % number of discarded burn-in samples
nSamples   = 2e3; % number of collected samples
nThin      = 1;   % number of samples between those collected
doParallel = 0;   % whether MATLAB parallel toolbox parallizes chains

% Assign MATLAB variables to the observed nodes
data = struct('np', np, ...
    'nq', nq, ...
    'k', k);

% Generator for initialization
generator = @()struct('p', rand(1, np), ...
    'q', rand(1, nq));

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
