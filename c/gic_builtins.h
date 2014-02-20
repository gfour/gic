/** @file gic_builtins.h

   The built-in functions of the GIC runtime.

*/

/** Big integer multiplication using libgmp.
    \param a The first big integer.
    \param b The second big integer.
    \return The product of a and b.
*/
Susp mulI(Susp a, Susp b);
/** Converts C strings to GIC lists.
    \param str The C string to convert.
    \param chars The length of the C string.
    \return A GIC string (i.e. a lazy list of characters).
*/
Susp strToList(char *str, int chars, TP_ T0);
