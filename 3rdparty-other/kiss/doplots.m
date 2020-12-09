load rand01.txt
figure
hist(rand01,100)
title([num2str(length(rand01)) ' calls to rand01 in 100 bins'])
print -depsc rand01.eps

load random.txt
figure
hist(random,100)
title([num2str(length(random)) ' calls to random in 100 bins'])
print -depsc random.eps

load randnorm.txt
figure
hist(randnorm,100)
title([num2str(length(randnorm)) ' calls to randnorm in 100 bins'])
print -depsc randnorm.eps
