
#include <stdlib.h>
#include <stdint.h>
#include "sqlite3.h"

int test1()
{
    sqlite3* db = NULL;
    int rc = sqlite3_open(":memory:", &db);
    if (rc != 0) return __LINE__ * 1000 + rc;

    sqlite3_stmt* stmt = NULL;
    rc = sqlite3_prepare_v2(db, "SELECT 47 + 24;", -1, &stmt, NULL);
    if (rc != 0) return __LINE__ * 1000 + rc;

    rc = sqlite3_step(stmt);
    if (rc != 0) return __LINE__ * 1000 + rc;

    int result = sqlite3_column_int(stmt, 0);
    rc = sqlite3_finalize(stmt);
    if (rc != 0) return __LINE__ * 1000 + rc;

    rc = sqlite3_close_v2(db);
    if (rc != 0) return __LINE__ * 1000 + rc;

    return result;
}

