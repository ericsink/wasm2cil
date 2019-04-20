
#include <stdlib.h>
#include <stdint.h>
#include "sqlite3.h"

int error(int line, int rc)
{
    int x = line * 10000 + rc;
    return -x;
}

#define ERROR(rc) error(__LINE__, rc)

int test1()
{
    sqlite3* db = NULL;
    int rc = sqlite3_open(":memory:", &db);
    if (rc != 0) return ERROR(rc);

    sqlite3_stmt* stmt = NULL;
    rc = sqlite3_prepare_v2(db, "SELECT 47 + 24;", -1, &stmt, NULL);
    if (rc != 0) return ERROR(rc);

    rc = sqlite3_step(stmt);
    if (rc != SQLITE_ROW) return ERROR(rc);

    int result = sqlite3_column_int(stmt, 0);

    rc = sqlite3_finalize(stmt);
    if (rc != 0) return ERROR(rc);

    rc = sqlite3_close_v2(db);
    if (rc != 0) return ERROR(rc);

    return result;
}

int test_db(char* filename)
{
    sqlite3* db = NULL;
    int rc = sqlite3_open(filename, &db);
    if (rc != 0) return ERROR(rc);

    rc = sqlite3_exec(db, "CREATE TABLE foo (x int);", NULL, NULL, NULL);
    if (rc != 0) return ERROR(rc);

    {
        sqlite3_stmt* stmt = NULL;

        rc = sqlite3_prepare_v2(db, "INSERT INTO foo (x) VALUES (?);", -1, &stmt, NULL);
        if (rc != 0) return ERROR(rc);

        for (int i=0; i<42; i++)
        {
            rc = sqlite3_reset(stmt);
            if (rc != SQLITE_OK) return ERROR(rc);

            rc = sqlite3_bind_int(stmt, 1, i);
            if (rc != SQLITE_OK) return ERROR(rc);

            rc = sqlite3_step(stmt);
            if (rc != SQLITE_DONE) return ERROR(rc);
        }

        rc = sqlite3_finalize(stmt);
        if (rc != 0) return ERROR(rc);
    }

    int result;

    {
        sqlite3_stmt* stmt = NULL;

        rc = sqlite3_prepare_v2(db, "SELECT sum(x) FROM foo;", -1, &stmt, NULL);
        if (rc != 0) return ERROR(rc);

        rc = sqlite3_step(stmt);
        if (rc != SQLITE_ROW) return ERROR(rc);

        result = sqlite3_column_int(stmt, 0);

        rc = sqlite3_finalize(stmt);
        if (rc != 0) return ERROR(rc);
    }

    rc = sqlite3_close_v2(db);
    if (rc != 0) return ERROR(rc);

    return result;
}

int test2()
{
    return test_db(":memory:");
}

int test3()
{
    return test_db("test3.sqlite");
}

