##ML training scripts 

Within this folder, you'll find scripts used to train various classification algorithms to predict jitter events.
All training scripts rely on pre-processed feature/response data that live within the jitter_data_set folder. 

A summary of scripts:

- logistic_binary_1h_reduced_response: This trains a logistic regression model to predict whether there will be at least
one, 1 hour long jitter event in a day.

- cart_binary scripts: Train CART model to predict whether there will be at least
one, 1 hour or 3 hour long jitter event in a day.
- cart_multiclass: Train CART model to predict whether will be 0, 1-12, or 13-24 hours of Jitter in a day. 

- rf_binary: Train a Random Forest model to predict whether there will be at least one, 1 hour long jitter event in a day.
- rf_multiclass: Train Random Forest model to predict whether will be 0, 1-12, or 13-24 hours of Jitter in a day. 

For the v1 implementation, CART model is recommended.
