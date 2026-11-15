import math
from scipy.stats import norm, t

def python_power_ttest(n, delta, sigma=1.0, alpha=0.05):
    """
    Power Calculation for Normal Mean Testing via Python
    
    Calculates the statistical power (1 - beta) of a one-sample z-test / t-test approximation 
    using SciPy distributions.
    
    Parameters:
    -----------
    n : int or float
        The sample size.
    delta : float
        The true difference or effect size (mu_1 - mu_0).
    sigma : float, optional
        The population standard deviation (default is 1.0).
    alpha : float, optional
        The significance level of the test (default is 0.05).
        
    Returns:
    --------
    float
        The calculated statistical power.
    """
    # Basic power calculation implementation for t-test / z-test comparison
    crit_val = norm.ppf(1 - alpha / 2)
    power = 1 - norm.cdf(crit_val - (delta * math.sqrt(n)) / sigma) + norm.cdf(-crit_val - (delta * math.sqrt(n)) / sigma)
    return power
