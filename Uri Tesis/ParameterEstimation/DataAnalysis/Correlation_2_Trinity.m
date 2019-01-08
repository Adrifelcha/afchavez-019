%% Pearson Correlation (Example 5.2)

clear;

%% Data
dataset = 4;

x = [0.8, 102;
     1.0,  98;
     0.5, 100;
     0.9, 105;
     0.7, 103;
     0.4, 110;
     1.2,  99;
     1.4,  87;
     0.6, 113;
     1.1,  89;
     1.3,  93];
 
 switch dataset
     case 3, sigmaerror = [.03  1]; % precise IQ measurements
     case 4, sigmaerror = [.03 10]; % imprecise IQ measurements
 end;
 
%% Constants
[n, ~] = size(x);
lambdaerror = 1./sigmaerror.^2; % reparameterize to precision of measurements

%% Trinity constants

% Graphical model script
modelName = 'Correlation_2';

% Parameters to monitor
params = {'r', 'mu', 'sigma'};

% MCMC properties
nChains    = 3;   % number of MCMC chains
nBurnin    = 1e3; % number of discarded burn-in samples
nSamples   = 3e3; % number of collected samples
nThin      = 1;   % number of samples between those collected
doParallel = 0;   % whether MATLAB parallel toolbox parallizes chains

% Assign MATLAB variables to the observed nodes
data = struct('x'           ,           x , ...
              'n'           ,           n , ...
              'lambdaerror' , lambdaerror);

% Generator for initialization
generator = @()struct('r'      , rand * 2 - 1  , ...
                      'mu'     ,  zeros(1, 2)  , ...
                      'lambda' ,     ones(1, 2));

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

% Drawing constants
lo   = -1;    % lower bound
hi   = 1;     % upper bound
step = 0.05;  % bins width

% Bins for histograms
binCenters = lo + step/2 : step : hi - step/2; % bin centers
binEdges   = lo : step : hi;                   % bin edges

% Histogram counts
count = histc(chains.r(:), binEdges);  % histogram counts
count = count(1 : end-1);                  % remove extra bin count at end from histc
count = count/sum(count)/step;             % scale according to total samples and bin width to get density

% Figure
figure(10 + dataset); clf;
set(gcf, ...
    'color'             ,            'w' , ...
    'units'             ,   'normalized' , ...
    'position'          ,  [.2 .2 .6 .4] , ...
    'paperpositionmode' ,         'auto' );

% Data axes, LHS
subplot(1, 2, 1); cla; hold on;
axis([0 1.5 80 115]);
set(gca, ...
    'xtick'             ,    0 : 0.25 : 1.5 , ...
    'ytick'             ,       85 : 5: 115 , ...
    'box'               ,             'off' , ...
    'tickdir'           ,             'out' , ...
    'ticklength'        ,          [0.01 0] , ...
    'fontsize'          ,               14  );

% Labels
xlabel('Response time (sec)', 'fontsize', 16);
ylabel('IQ', 'fontsize', 16);

% Plor data and error bars of data
for i = 1 : n
    plot([x(i, 1) - sigmaerror(1) x(i ,1) + sigmaerror(1)], ones(1, 2) *x (i, 2), 'k-'); % horizontal error bar
    plot(ones(1, 2) * x(i, 1), [x(i, 2) - sigmaerror(2) x(i, 2) + sigmaerror(2)], 'k-'); % vertical error bar
end;
plot(x(:, 1), x(:, 2), 'ko', ...                 % data points
                       'markersize', 4, ...
                       'markerfacecolor', 'k');

% Correlation posterior axes, RHS
subplot(1, 2, 2); cla; hold on;
set(gca, ...
    'xtick'             ,   -1 : 0.5 : 1 , ...
    'ytick'             ,             [] , ...
    'box'               ,          'off' , ...
    'tickdir'           ,          'out' , ...
    'ticklength'        ,       [0.01 0] , ...
    'fontsize'          ,            14  );

% Labels
xlabel('Correlation', 'fontsize', 16);
ylabel('Posterior Density', 'fontsize', 16);

% Plot data
plot(binCenters, count, 'k-');

% Plot point estimate line
correlation = corr(x(:, 1), x(:, 2), 'type', 'pearson');
plot(ones(1, 2) * correlation, [0 max(get(gca, 'ylim'))], 'k--');




