%% Malingering, With Individual Differences (Example 6.6)

clear;

%% Data
k = [45, 45, 44, 45, 44, 45, 45, 45, 45 ,45, 30, ...
     20,  6, 44, 44, 27, 25, 17, 14, 27, 35, 30];     % total correct per participant

%% Constants
p = length(k); % number of participants
n = 45;        % number of questions

%% Trinity constants

% Graphical model script
modelName = 'Malingering_2_jags';

% Parameters to monitor
params = {'theta', 'z', 'mubon', 'lambdabon', 'mumal', 'lambdamal', 'mudiff', 'phi'};

% MCMC properties
nChains    = 3;   % number of MCMC chains
nBurnin    = 1e3; % number of discarded burn-in samples
nSamples   = 2e3; % number of collected samples
nThin      = 1;   % number of samples between those collected
doParallel = 0;   % whether MATLAB parallel toolbox parallizes chains

% Assign MATLAB variables to the observed nodes
data = struct('p', p, ...
    'n', n, ...
    'k',k);

% Generator for initialization
generator = @()struct('z', round(rand(1, p)));

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
