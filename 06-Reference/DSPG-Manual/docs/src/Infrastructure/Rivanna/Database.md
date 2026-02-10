# Database

DSPG projects will be using the `sdad` database in the `postgis1` Postgres server which is hosted in Rivanna. To connect to the database from

!!! note

    This assumes you have set up your credentials in the `~/.bashrc`. See Rivanna/Configuration for how to do so.

## From R

```R
library(RPostgreSQL)
conn <- dbConnect(drv = PostgreSQL(), dbname = "sdad")
dbDisconnect(conn = conn)
```

## From Julia

```julia
using LibPQ
conn = LibPQ.Connection("dbname = sdad");
close(conn)
```

## From Python

```
import os, psycopg2
conn = psycopg2.connect(dbname = "sdad")
cur = conn.cursor()
cur.close()
conn.close()
```

## pgAdmin

If you wish to interact with the Postgres database through the PgAdmin interface. Install [pgAdmin](https://www.pgadmin.org/download/) in your local machine.

- Connect to the UVA VPN (`UVA Anywhere` or `UVA More Secure Network`)
- Add a new server in pgAdmin
- Use the following information
```
host = postgis1
port = 5432
Maintenance database = sdad
Username = {db_usr}
password = {db_pwd}
```
and under SSH Tunnel
```
Use SSH tunneling = Yes
Tunnel Host = rivanna.hpc.virginia.edu
Username = {computing_id}
Authentication = Password
```
