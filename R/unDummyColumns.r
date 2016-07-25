library(caret)
data(GermanCredit)

idx <- which(colnames(GermanCredit) == 'CreditHistory.NoCredit.AllPaid'):which(colnames(GermanCredit) == 'CreditHistory.Critical')
ind <- apply(GermanCredit[,idx],1,function(x) which(as.logical(x)))
GermanCredit$CreditHistory <- colnames(GermanCredit[,idx])[ind]
GermanCredit <- GermanCredit[,-c(idx)]

idx <- which(colnames(GermanCredit) == 'SavingsAccountBonds.lt.100'):which(colnames(GermanCredit) == 'SavingsAccountBonds.Unknown')
ind <- apply(GermanCredit[,idx],1,function(x) which(as.logical(x)))
GermanCredit$SavingsAccountBonds <- colnames(GermanCredit[,idx])[ind]
GermanCredit <- GermanCredit[,-c(idx)]

idx <- which(colnames(GermanCredit) == 'EmploymentDuration.lt.1'):which(colnames(GermanCredit) == 'EmploymentDuration.Unemployed')
ind <- apply(GermanCredit[,idx],1,function(x) which(as.logical(x)))
GermanCredit$EmploymentDuration <- colnames(GermanCredit[,idx])[ind]
GermanCredit <- GermanCredit[,-c(idx)]

idx <- which(colnames(GermanCredit) == 'Personal.Male.Divorced.Seperated'):which(colnames(GermanCredit) == 'Personal.Female.Single')
ind <- apply(GermanCredit[,idx],1,function(x) which(as.logical(x)))
GermanCredit$Personal <- colnames(GermanCredit[,idx])[ind]
GermanCredit <- GermanCredit[,-c(idx)]

idx <- which(colnames(GermanCredit) == 'CheckingAccountStatus.lt.0'):which(colnames(GermanCredit) == 'CheckingAccountStatus.none')
ind <- apply(GermanCredit[,idx],1,function(x) which(as.logical(x)))
GermanCredit$CheckingAccountStatus <- colnames(GermanCredit[,idx])[ind]
GermanCredit <- GermanCredit[,-c(idx)]

idx <- which(colnames(GermanCredit) == 'Property.RealEstate'):which(colnames(GermanCredit) == 'Property.Unknown')
ind <- apply(GermanCredit[,idx],1,function(x) which(as.logical(x)))
GermanCredit$Property <- colnames(GermanCredit[,idx])[ind]
GermanCredit <- GermanCredit[,-c(idx)]

idx <- which(colnames(GermanCredit) == 'Purpose.NewCar'):which(colnames(GermanCredit) == 'Purpose.Other')
ind <- apply(GermanCredit[,idx],1,function(x) which(as.logical(x)))
GermanCredit$Property <- colnames(GermanCredit[,idx])[ind]
GermanCredit <- GermanCredit[,-c(idx)]

idx <- which(colnames(GermanCredit) == 'OtherDebtorsGuarantors.None'):which(colnames(GermanCredit) == 'OtherDebtorsGuarantors.Guarantor')
ind <- apply(GermanCredit[,idx],1,function(x) which(as.logical(x)))
GermanCredit$OtherDebtorsGuarantors <- colnames(GermanCredit[,idx])[ind]
GermanCredit <- GermanCredit[,-c(idx)]

idx <- which(colnames(GermanCredit) == 'Job.UnemployedUnskilled'):which(colnames(GermanCredit) == 'Job.Management.SelfEmp.HighlyQualified')
ind <- apply(GermanCredit[,idx],1,function(x) which(as.logical(x)))
GermanCredit$Job <- colnames(GermanCredit[,idx])[ind]
GermanCredit <- GermanCredit[,-c(idx)]

idx <- which(colnames(GermanCredit) == 'Housing.Rent'):which(colnames(GermanCredit) == 'Housing.ForFree')
ind <- apply(GermanCredit[,idx],1,function(x) which(as.logical(x)))
GermanCredit$Housing <- colnames(GermanCredit[,idx])[ind]
GermanCredit <- GermanCredit[,-c(idx)]

idx <- which(colnames(GermanCredit) == 'OtherInstallmentPlans.Bank'):which(colnames(GermanCredit) == 'OtherInstallmentPlans.None')
ind <- apply(GermanCredit[,idx],1,function(x) which(as.logical(x)))
GermanCredit$OtherInstallmentPlans <- colnames(GermanCredit[,idx])[ind]
GermanCredit <- GermanCredit[,-c(idx)]

head(GermanCredit)

