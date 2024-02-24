# Probability in R Project

## Overview

This R project focuses on various aspects of probability theory, including the generation of random variables, computation of marginal distributions, covariance, conditional probability, and more. The project is divided into multiple sections, each addressing specific probability-related tasks.

## Sections

### 1. Random Variable Generation and Completion (a-b)

**Objective:** Create and complete a frequency table representing a joint distribution.

**Implementation:**
- Generate an empty frequency table using `frepcomgen`.
- Complete the table with specified values using `fcompleprecom`.

**Example:**
- Generate a table and complete it with predefined values.

### 2. Marginal Distributions (c)

**Objective:** Calculate marginal distributions for both variables X and Y.

**Implementation:**
- Utilize `frepmarginal` to calculate marginal distributions for X and Y.

**Example:**
- Obtain marginal distributions for a completed joint distribution.

### 3. Covariance Calculation (d)

**Objective:** Compute the covariance of a linear combination of variables.

**Implementation:**
- Use `fpropcov` to calculate covariance given coefficients a, b, c, and d.

**Example:**
- Calculate Cov(2X + 3Y, 2X - Y) for a given joint distribution.

### 4. Conditional Probability (e-f)

**Objective:** Compute conditional probabilities for specified events.

**Implementation:**
- Implement `fPcond` for conditional probability calculation.
- Implement `fPcomun` for joint probability calculation.

**Example:**
- Calculate P(X=x | Y=y) and P(X=x, Y=y) for given values.

### 5. Covariance and Probability Calculations (g)

**Objective:** Calculate Covariance(5X + 9, 3Y - 2) and specific probabilities.

**Implementation:**
- Use `cov_calcul` for Cov(5X + 9, 3Y - 2).
- Implement probability calculations for P(0 < X < 0.8 | Y > 0.3) and P(X > 0.2 & Y < 1.7).

**Example:**
- Compute covariance and specified probabilities for a given joint distribution.

### 6. Independence and Uncorrelation (h)

**Objective:** Check independence and uncorrelation.

**Implementation:**
- Use `fverind` to check independence.
- Use `fvernecor` to check uncorrelation.

**Example:**
- Check if variables are independent and uncorrelated.

### 7. Interactive Probability Visualization (2)

**Objective:** Create a Shiny app for interactive visualization of probability concepts.

**Implementation:**
- Use Shiny, Plotly, and ggplot2 for creating interactive plots.
- Allow users to input functions and visualize corresponding probability concepts.

**Example:**
- Visualize geometric interpretations, densities, and distributions interactively.

### 8. Probability Verification App (c)

**Objective:** Develop a Shiny app for verifying probability density functions.

**Implementation:**
- Create a Shiny app with user inputs for a function and specified limits.
- Check if the input function is a valid probability density function.

**Example:**
- Verify if x^2 + y^2 is a valid probability density function within specified limits.

### 9. Random Variable Generator App (d)

**Objective:** Build a Shiny app for generating random variables.

**Implementation:**
- Allow users to input a probability density function and define the range.
- Generate either unidimensional or bidimensional random variables.

**Example:**
- Generate random variables based on user-provided functions and ranges.

### 10. Animated Probability Visualization (e)

**Objective:** Create an animated Shiny app for visualizing random variables.

**Implementation:**
- Allow users to input a function and animate changes in parameters.
- Use gganimate to create animations of density functions and cumulative distribution functions.

**Example:**
- Animate changes in parameters for a given density function.

### 11. Mean and Variance Calculator (h)

**Objective:** Develop a Shiny app for calculating mean and variance.

**Implementation:**
- Allow users to input a probability density function and a transformation function.
- Calculate and display the mean and variance of the transformed random variable.

**Example:**
- Calculate mean and variance for g(X) = X^2 given a density function.

### 12. Probability Calculator (i)

**Objective:** Build a Shiny app for calculating probabilities.

**Implementation:**
- Allow users to input a probability density function.
- Calculate and display the probability for specified intervals.

**Example:**
- Calculate the probability for a specified interval for a given probability density function.

## Dependencies

Ensure the following R packages are installed:

```R
install.packages("plotly")
install.packages("shiny")
install.packages("ggplot2")
install.packages("animate")
install.packages("gganimate")
