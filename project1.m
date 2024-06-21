clc
clear all
close all

% load the data
startdate = '01/01/1994';
enddate = '01/01/2023';
f = fred
DEY = fetch(f,'CLVMNACSCAB1GQDE',startdate,enddate)   
JPY = fetch(f,'JPNRGDPEXP',startdate,enddate) 
dey = log(DEY.Data(:,2));
jpy = log(JPY.Data(:,2));    
q = DEY.Data(:,1);

T = size(dey,1);

% Hodrick-Prescott filter
lam = 1600;
A = zeros(T,T);

% unusual rows
A(1,1)= lam+1; A(1,2)= -2*lam; A(1,3)= lam;
A(2,1)= -2*lam; A(2,2)= 5*lam+1; A(2,3)= -4*lam; A(2,4)= lam;

A(T-1,T)= -2*lam; A(T-1,T-1)= 5*lam+1; A(T-1,T-2)= -4*lam; A(T-1,T-3)= lam;
A(T,T)= lam+1; A(T,T-1)= -2*lam; A(T,T-2)= lam;

% generic rows
for i=3:T-2
    A(i,i-2) = lam; A(i,i-1) = -4*lam; A(i,i) = 6*lam+1;
    A(i,i+1) = -4*lam; A(i,i+2) = lam;
end

tauDEGDP = A\dey;
tauJPGDP = A\jpy;

% detrended GDP
deytilde = dey-tauDEGDP;
jpytilde = jpy-tauJPGDP;

% plot detrended GDP
dates = 1994:1/4:2022.1/4; dezerovec = zeros(size(dey)); jpzerovec = zeros(size(jpy));
figure
title('Detrended log(real GDP) 1994Q1-2022Q1'); hold on
plot(q, deytilde,'b', q, jpytilde,'r')
datetick('x', 'yyyy-qq')

% compute sd(dey), sd(jpy), rho(dey), rho(jpy), corr(dey,jpy) (from detrended series)
deysd = std(deytilde)*100;
deyrho = corrcoef(deytilde(2:T),deytilde(1:T-1)); deyrho = deyrho(1,2);
jpysd = std(jpytilde)*100;
jpyrho = corrcoef(jpytilde(2:T),jpytilde(1:T-1)); jpyrho = jpyrho(1,2);
corryc = corrcoef(deytilde(1:T),jpytilde(1:T)); corryc = corryc(1,2);

disp(['Percent standard deviation of detrended log real GDP for Germany: ', num2str(deysd),'.']); disp(' ')
disp(['Percent standard deviation of detrended log real GDP for Japan: ', num2str(jpysd),'.']); disp(' ')
disp(['Contemporaneous correlation between detrended log real GDP in Germany and Japan: ', num2str(corryc),'.']);
