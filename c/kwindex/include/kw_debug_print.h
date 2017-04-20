/* kw_debug_print.h -- Andrew Cashner, 2017/04/20
 * Allow printf statements that only show if DEBUG is defined
 */

#ifndef KW_DEBUG_PRINT_H
#define KW_DEBUG_PRINT_H

#ifdef DEBUG
#define DEBUG_PRINT(x) printf x
#else
#define DEBUG_PRINT(x) {};
#endif


#endif /* KW_DEBUG_PRINT_H */
