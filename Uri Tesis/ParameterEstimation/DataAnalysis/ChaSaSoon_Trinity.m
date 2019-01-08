%%  Change Detection (Example 5.5)

clear;

%% Data
nattempts = 950;                 % Number of attempts
nfails = 949;                    % Number of failures
n = 50;                          % Nunber of questions

%% Constants
y = [ones(nfails, 1); 0];        % Indicate which scores are censored
z = [nan*ones(nfails, 1); 30]';  % All scores except the last are unknown

%% Trinity constants

% Graphical model script
modelName = 'ChaSaSoonJ';        % Note JAGS script, using dinterval

% Parameters to monitor
params = {'theta', 'z'};

% MCMC properties
nChains    = 3;   % number of MCMC chains
nBurnin    = 1e3; % number of discarded burn-in samples
nSamples   = 2e3; % number of collected samples
nThin      = 1;   % number of samples between those collected
doParallel = 0;   % whether MATLAB parallel toolbox parallizes chains

% Assign MATLAB variables to the observed nodes
data = struct('nattempts', nattempts, ...
    'n', n, ...
    'z', z, ...
    'y', y);

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
lo   = 0;    % lower bound
hi   = 1;     % upper bound
step = 0.005;  % bins width
CI   = 0.95;  % set credible interval

% Bins for histograms
binCenters = lo + step/2 : step : hi - step/2; % bin centers
binEdges   = lo : step : hi;                   % bin edges

% Histogram counts
count = histc(chains.theta(:), binEdges);  % histogram counts
count = count(1 : end-1);                  % remove extra bin count at end from histc
count = count/sum(count)/step;             % scale according to total samples and bin width to get density

% Credible Interval
sortedSamples   = sort(chains.theta(:));
lowerBoundIndex = round((1 - CI)/2 * length(sortedSamples));
upperBoundIndex = round((1 - (1 - CI)/2) * length(sortedSamples));
disp(sprintf('%d percent credible interval for theta is [%1.2f, %1.2f]', ...
    CI*100, ...
    sortedSamples(lowerBoundIndex), ...
    sortedSamples(upperBoundIndex)));

% Figure
figure(1); clf; hold on;
set(gcf, ...
    'color'             ,            'w' , ...
    'units'             ,   'normalized' , ...
    'position'          , [.2 .2 .6 .6]  , ...
    'paperpositionmode' ,         'auto' );

% Axes
axis([0 1 0 max(count) * 1.3]);
set(gca, ...
    'xtick'             , 0.1 : 0.1 : 1  , ...
    'box'               ,          'off' , ...
    'tickdir'           ,          'out' , ...
    'fontsize'          ,            14  );

% Labels
xlabel('Rate', 'fontsize', 16);
ylabel('Posterior density', 'fontsize',16);

% Plot density
plot(binCenters, count, 'k-');

% Text label for credible interval on plot
[value, index] = max(count);
text(binCenters(index),value * 1.1, ...
    {sprintf('%1.2f - %1.2f', sortedSamples(lowerBoundIndex), sortedSamples(upperBoundIndex)), ...
     sprintf('%d%%', CI * 100)}, ...
    'horizontalalignment', 'center', ...
    'fontsize', 14);