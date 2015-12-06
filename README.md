# ekkey-homo
Repo for throw-away key-homomorphic PRF construction

##TODO
*  Parameters used are not necessarily secure. I'm unclear on the exact boundaries for dimension and modulus given a particular tree depth and message length, so am simplifying parameter generation by allowing the dimension to be specified and taking dimension^2 <= modulus <= 2*dimension^2.
