    
double Iq(double q, double gauss_scale, double cor_length_static, double lorentz_scale, double cor_length_dynamic);

#pragma acc routine seq
double Iq(double q, double gauss_scale, double cor_length_static, double lorentz_scale, double cor_length_dynamic) {
    // Calculate the Gaussian and Lorentzian terms separately
    double term1 = gauss_scale * exp(-q * q * cor_length_static * cor_length_static / 2.0);
    double term2 = lorentz_scale / (1.0 + q * q * cor_length_dynamic * cor_length_dynamic);

    // Return the sum of the two terms
    return term1 + term2;
}



