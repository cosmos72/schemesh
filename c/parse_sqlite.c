/**
 * Copyright (C) 2025 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#include <sqlite3.h>
#include <string.h> /* memcpy(), strcmp() */

#include "writer.h"

#define TABLENAME_MAX 1024

static int validate_table_name(const char* s) {
  char   ch;
  size_t i;
  for (i = 0; (ch = s[i]); i++) {
    if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_' ||
        (i != 0 && ch >= '0' && ch <= '9')) {
      continue;
    }
    return 1;
  }
  return i <= TABLENAME_MAX ? 0 : 1;
}

static int usage(const char* argv0) {
  fprintf(stderr, "Usage: %s DB [TABLE]\n       %s DB --sql QUERY\n", argv0, argv0);
  return 1;
}

int main(int argc, char** argv) {
  writer        w    = {stdout, 0, {0}};
  sqlite3*      db   = NULL;
  sqlite3_stmt* stmt = NULL;
  int           err  = 0;
  int           cols;

  if (argc < 2 || argc > 4) {
    err = usage(argv[0]);
    goto quit;
  }

  if (sqlite3_open(argv[1], &db) != SQLITE_OK) {
    fprintf(stderr, "parse_sqlite open error: %s\n", sqlite3_errmsg(db));
    err = 1;
    goto quit;
  }

  if (argc == 3 && validate_table_name(argv[2]) != 0) {
    fprintf(stderr, "parse_sqlite invalid table name: %s\n", argv[2]);
    err = 1;
    goto quit;
  } else if (argc == 4 && strcmp(argv[2], "--sql") != 0) {
    err = usage(argv[0]);
    goto quit;
  }

  if (argc == 2) {
    const char sql[] = "SELECT name FROM sqlite_master WHERE type = 'table';";

    err = sqlite3_prepare_v2(db, sql, sizeof(sql) - 1, &stmt, NULL);
  } else if (argc == 3) {
    char sql[TABLENAME_MAX + 16];
    int  len = snprintf(sql, sizeof(sql), "SELECT * FROM %s;", argv[2]);
    err      = sqlite3_prepare_v2(db, sql, len, &stmt, NULL);
  } else {
    err = sqlite3_prepare_v2(db, argv[3], -1, &stmt, NULL);
  }

  if (err != SQLITE_OK) {
    fprintf(stderr, "parse_sqlite sql error: %s\n", sqlite3_errmsg(db));
    err = 1;
    goto quit;
  }

  setvbuf(w.f, NULL, _IOFBF, WBUF_MAX);

  cols = sqlite3_column_count(stmt);

  while (sqlite3_step(stmt) == SQLITE_ROW) {

    w_put_char(&w, '{');

    for (int i = 0; i < cols; i++) {
      if (i != 0) {
        w_put_char(&w, ',');
      }
      w_put_quoted_escaped_chars(&w, sqlite3_column_name(stmt, i));
      w_put_char(&w, ':');

      int type = sqlite3_column_type(stmt, i);

      switch (type) {

        case SQLITE_INTEGER:
          w_put_int64(&w, sqlite3_column_int64(stmt, i));
          break;

        case SQLITE_FLOAT:
          w_put_double(&w, sqlite3_column_double(stmt, i));
          break;

        case SQLITE_TEXT:
          w_put_quoted_escaped_chars(&w, (const char*)sqlite3_column_text(stmt, i));
          break;

        case SQLITE_NULL:
        default:
          w_put_literal(&w, "null");
          break;

        case SQLITE_BLOB:
          w_put_base64(&w, sqlite3_column_blob(stmt, i), sqlite3_column_bytes(stmt, i));
          break;
      }
    }
    w_put_literal(&w, "}\n");
  }

  w_flush(&w);

quit:
  if (stmt) {
    sqlite3_finalize(stmt);
  }
  if (db) {
    sqlite3_close(db);
  }
  return err;
}
