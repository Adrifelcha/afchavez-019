%% Difference Between Two Rates (Example 3.2)

clear;

%% Data
k1 = 0;
n1 = 5;
k2 = 5;
n2 = 10;

%% Trinity constants

% Graphical model script
modelName = 'Rate_2';

% Parameters to monitor
params = {'theta1', 'theta2', 'delta'};

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
generator = @()struct('theta1', rand, ...
                      'theta2', rand);

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

% Constants
lo   = -1;    % lower bound
hi   = 1;     % upper bound
step = 0.01;  % bins width
CI   = 0.95;  % set credible interval


% Bins for histograms
binCenters = lo + step/2 : step : hi - step/2; % bin centers
binEdges   = lo : step : hi;                   % bin edges

% Histogram counts
count = histc(chains.delta(:), binEdges);  % histogram counts
count = count(1 : end-1);                  % remove extra bin count at end from histc
count = count/sum(count)/step;             % scale according to total samples and bin width to get density

% Figure
figure(2); clf; hold on;
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
xlabel('Difference in rates \delta', 'fontsize', 16);
ylabel('Posterior density', 'fontsize',16);

% Plot density
plot(binCenters, count, 'k-');

% Summaries of Posterior
% Mean
disp(sprintf('Mean is %1.2f', codatable(chains, 'delta', @mean))); 

% Mode 
[~, index] = max(count); % find index of bin with greatest count
disp(sprintf('Mode is %1.2f', binCenters(index)));

% Median
disp(sprintf('Median is %1.2f', codatable(chains, 'delta', @median))); 

% Credible interval
sortedSamples   = sort(chains.delta(:));
lowerBoundIndex = round((1 - CI)/2 * length(sortedSamples));
upperBoundIndex = round((1 - (1 - CI)/2) * length(sortedSamples));
disp(sprintf('%d percent credible interval is [%1.2f, %1.2f]', ...
    CI*100, ...
    sortedSamples(lowerBoundIndex), ...
    sortedSamples(upperBoundIndex)));



