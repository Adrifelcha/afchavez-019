%% Prior and Posterior Predictive, Second Example (Example 3.5)

clear;

%% Data
k1 = 0;
n1 = 10;
k2 = 10;
n2 = 10;

%% Trinity constants

% Graphical model script
modelName = 'Rate_5';

% Parameters to monitor
params = {'theta', ...
          'postpredk1', ...
          'postpredk2'};

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
lo    = 0;     % lower bound for rate
hi    = 1;     % upper bound for rate
step  = 0.01;  % bins width for rate
scale = 70;    % scaling factor for 2-d histogram boxes

% Bins for histograms
binCenters = lo + step/2 : step : hi - step/2; % bin centers
binEdges   = lo : step : hi;                   % bin edges
dataBinsEdges{1} = 0 : n1;                       % discrete bins in data space
dataBinsEdges{2} = 0 : n2;                       

% Histogram counts
% Parameter posterior
count = histc(chains.theta(:), binEdges);  % histogram counts
count = count(1 : end-1);                  % remove extra bin count at end from histc
count = count/sum(count)/step;             % scale according to total samples and bin width to get density
% Data posterior
countData = hist3([chains.postpredk1(:) chains.postpredk2(:)], ...
                  'edges', dataBinsEdges);                          % histogram counts
countData = countData/sum(countData(:));                            % normalize

% Figure
figure(5); clf; hold on;
set(gcf, ...
    'color'             ,            'w' , ...
    'units'             ,   'normalized' , ...
    'position'          ,  [.2 .2 .7 .5] , ...
    'paperpositionmode' ,         'auto' );

% Parameter space axes
subplot(1, 2, 1); cla; hold on;
set(gca, ...
    'xtick'             ,   0 : 0.2 : 1 , ...
    'box'               ,          'off' , ...
    'tickdir'           ,          'out' , ...
    'fontsize'          ,            14  );

% Labels
xlabel('Rate \theta', 'fontsize', 16);
ylabel('Probability density', 'fontsize',16);

% Plot density
plot(binCenters, count, 'k-');

% Data space axes
subplot(1, 2, 2); cla; hold on;
set(gca, ...
    'xtick'             ,         [0 n1] , ...
    'ytick'             ,         [0 n2] , ... 
    'xlim'              ,  [-1/2 n1+1/2] , ...
    'ylim'              ,  [-1/2 n2+1/2] , ...
    'box'               ,           'on' , ...
    'tickdir'           ,          'out' , ...
    'fontsize'          ,            14  );

% Labels
xlabel('Success count k_1',    'fontsize', 16);
ylabel('Success count k_2', 'fontsize', 16);

% Plot density
for i = 0 : n1
    for j = 0 : n2
        % draw posterior predictive square if mass exists
        if countData(i+1, j+1) > 0
            plot(i, j, 'ks', ...
                 'markersize', scale * sqrt(countData(i+1, j+1)));
        end;
        % indicate data
        if (i == k1) && (j == k2)
            plot(i, j, 'kx', ...
                'markersize', 16, ...
                'linewidth', 4);
        end;
    end;
end;
