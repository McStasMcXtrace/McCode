double Iq(double q, double length_tail, double length_head, double sld, double sld_head, double sld_solvent)
{
    const double qsq = q*q;
    const double drh = sld_head - sld_solvent;
    const double drt = sld - sld_solvent;    //correction 13FEB06 by L.Porcar
    const double qT = q*length_tail;
    double Pq, inten;
    Pq = drh*(sin(q*(length_head+length_tail))-sin(qT)) + drt*sin(qT);
    Pq *= Pq;
    Pq *= 4.0/(qsq);

    inten = 2.0e-4*M_PI*Pq/qsq;

    // normalize by the bilayer thickness
    inten /= 2.0*(length_head+length_tail);

    return inten;
}