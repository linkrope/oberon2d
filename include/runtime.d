module runtime;

/**
 * Returns the quotient for a floored division.
 */
long DIV(long D, long d)
{
    long q = D / d;
    const r = D % d;

    if ((r > 0 && d < 0) || (r < 0 && d > 0))
        --q;
    return q;
}

///
unittest
{
    assert(DIV(8, 3) == 2);
    assert(DIV(8, -3) == -3);
    assert(DIV(-8, 3) == -3);
    assert(DIV(-8, -3) == 2);
}

/**
 * Returns the remainder for a floored division.
 */
long MOD(long D, long d)
{
    long r = D % d;

    if ((r > 0 && d < 0) || (r < 0 && d > 0))
        r += d;
    return r;
}

///
unittest
{
    assert(MOD(8, 3) == 2);
    assert(MOD(8, -3) == -1);
    assert(MOD(-8, 3) == 1);
    assert(MOD(-8, -3) == -2);
}
