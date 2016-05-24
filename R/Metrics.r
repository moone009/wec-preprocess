

RMSE <- function(y,y_pred){sqrt(mean((y-y_pred)^2))}
MSE <- function(y,y_pred){mean((y-y_pred)^2)}
MAE <- function(y,y_pred){mean(y-y_pred)}
