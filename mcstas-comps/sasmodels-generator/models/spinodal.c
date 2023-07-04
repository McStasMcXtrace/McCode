    
double Iq(double q, double gamma, double q_0);

#pragma acc routine seq
double Iq(double q, double gamma, double q_0) {
    double x = q / q_0;
    double inten = ((1 + gamma / 2) * pow(x, 2)) / (gamma / 2 + pow(x, 2 + gamma));
    return inten;
}

