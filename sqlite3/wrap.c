
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/time.h>
#include "sqlite3.h"

long get_ms(void)
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    long s = (long) (tv.tv_sec);
    long us = (long) (tv.tv_usec);
    long ms = us / 1000;
    long total = s * 1000 + ms;
    return total;
}

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

int test_more(char* filename, int count, int first, int last)
{
    sqlite3* db = NULL;
    int rc = sqlite3_open(filename, &db);
    if (rc != 0) return ERROR(rc);

    rc = sqlite3_exec(db, "CREATE TABLE foo (x int, y int);", NULL, NULL, NULL);
    if (rc != 0) return ERROR(rc);

    {
        sqlite3_stmt* stmt = NULL;

        rc = sqlite3_prepare_v2(db, "INSERT INTO foo (x,y) VALUES (?,?);", -1, &stmt, NULL);
        if (rc != 0) return ERROR(rc);

        for (int i=0; i<count; i++)
        {
            rc = sqlite3_reset(stmt);
            if (rc != SQLITE_OK) return ERROR(rc);

            rc = sqlite3_bind_int(stmt, 1, i);
            if (rc != SQLITE_OK) return ERROR(rc);

            rc = sqlite3_bind_int(stmt, 2, i * i);
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

        rc = sqlite3_prepare_v2(db, "SELECT sum(y) FROM foo WHERE x >= ? AND x <= ?;", -1, &stmt, NULL);
        if (rc != 0) return ERROR(rc);

        rc = sqlite3_bind_int(stmt, 1, first);
        if (rc != SQLITE_OK) return ERROR(rc);

        rc = sqlite3_bind_int(stmt, 2, last);
        if (rc != SQLITE_OK) return ERROR(rc);

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

int main(int argc, char** argv)
{
    long t1 = get_ms();
    int rc = test_more("t4.sqlite", 1000, 256, 384);
    long t2 = get_ms();
    fprintf(stderr, "elapsed: %d ms\n", (int) (t2 - t1));
    printf("rc: %d\n", rc);
    return rc;
}

