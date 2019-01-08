%% Prior and Posterior Predictive (Example 3.4)

clear;

%% Data
dataSet = 1;
switch dataSet
    case 1, % Toy data
            k = 1;
            n = 15;
            dataTicks = [0 5 10 15];
    case 2, % Trouw nursing-home data
            k = 24;
            n = 121;
            dataTicks = [0 10:10:110 121];
end;

%% Trinity constants

% Graphical model script
modelName = 'Rate_4';

% Parameters to monitor
params = {'theta', ...
          'thetaprior', ...
          'postpredk', ...
          'priorpredk'};

% MCMC properties
nChains    = 3;   % number of MCMC chains
nBurnin    = 0;   % number of discarded burn-in samples
nSamples   = 5e3; % number of collected samples
nThin      = 1;   % number of samples between those collected
doParallel = 0;   % whether MATLAB parallel toolbox parallizes chains

% Assign MATLAB variables to the observed nodes
data = struct('k', k, ...
              'n', n);

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
lo   = 0;     % lower bound for rate
hi   = 1;     % upper bound for rate
step = 0.01;  % bins width for rate

% Bins for histograms
binCenters = lo + step/2 : step : hi - step/2; % bin centers
binEdges   = lo : step : hi;                   % bin edges
dataBins   = 0 : n;                              % discrete bins in data space

% Histogram counts
% Parameter prior
countPrior = histc(chains.thetaprior(:), binEdges);        % histogram counts
countPrior = countPrior(1 : end-1);                        % remove extra bin count at end from histc
countPrior = countPrior/sum(countPrior)/step;              % scale according to total samples and bin width to get density
% Parameter posterior
countPosterior = histc(chains.theta(:), binEdges);         % histogram counts
countPosterior = countPosterior(1 : end-1);                % remove extra bin count at end from histc
countPosterior = countPosterior/sum(countPosterior)/step;  % scale according to total samples and bin width to get density
% Data prior
countDataPrior = hist(chains.priorpredk(:), dataBins);            % histogram counts
countDataPrior = countDataPrior/sum(countDataPrior);              % normalize
% Data posterior
countDataPosterior = hist(chains.postpredk(:), dataBins);         % histogram counts
countDataPosterior = countDataPosterior/sum(countDataPosterior);  % normalize

% Figure
figure(4); clf; hold on;
set(gcf, ...
    'color'             ,            'w' , ...
    'units'             ,   'normalized' , ...
    'position'          ,  [.2 .2 .6 .6] , ...
    'paperpositionmode' ,         'auto' );

% Parameter space axes
subplot(2, 1, 1); cla; hold on;
set(gca, ...
    'xtick'             ,   0 : 0.2 : 1 , ...
    'box'               ,          'off' , ...
    'tickdir'           ,          'out' , ...
    'fontsize'          ,            14  );

% Labels
xlabel('Rate \theta', 'fontsize', 16);
ylabel('Probability density', 'fontsize',16);

% Plot density
H(1) = plot(binCenters, countPrior,     'k--');
H(2) = plot(binCenters, countPosterior, 'k-');

% Legend
L = legend(H, 'Prior', 'Posterior', ...
           'location', 'northeast');
set(L, 'box', 'off');

% Data space axes
subplot(2, 1, 2); cla; hold on;
set(gca, ...
    'xtick'             ,      dataTicks , ...
    'xlim'              ,   [-1/2 n+1/2] , ...
    'box'               ,          'off' , ...
    'tickdir'           ,          'out' , ...
    'fontsize'          ,            14  );

% Labels
xlabel('Success count',    'fontsize', 16);
ylabel('Probability mass', 'fontsize', 16);

% Plot density
H(1) = bar(dataBins, countDataPrior);
set(H(1), 'facecolor', 'none', ...
          'linewidth', 1.5   , ...
          'linestyle', '--'  , ...
          'barwidth',  0.6);
H(2) = bar(dataBins, countDataPosterior);
set(H(2), 'facecolor', 'none', ...
          'barwidth',  0.8);
      
% Tidy
uistack(H(1), 'top');     % put the narrower white prior predictive on top
set(gca, 'layer', 'top'); % put the axes on top of plotted elements

% Legend
L = legend(H, 'Prior predictive', 'Posterior predictive');
set(L, 'box',       'off', ...
       'location',  'northeast');