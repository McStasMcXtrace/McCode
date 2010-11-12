int iand_ (a,b)
int *a;
int *b;
{
    static int res;
    
    res = (*a & *b);
/*    printf ("iand result: %d\n",res); */

    return (res);
}
