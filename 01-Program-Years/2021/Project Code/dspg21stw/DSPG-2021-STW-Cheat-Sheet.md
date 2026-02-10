This document puts working with Github/Rivanna/PGAdmin into my own
words.

### Github/Rivanna Repos

Github saves versions of your code, and makes it easy to collaborate and
share code.

The Rivanna repos is for if you need to save small data. You will have
to create a "symlink" to connect them. Only one person creates the
symlink and then everyone can access it. Big data (aka WIOA or BGT) goes
in the database.

    system(command = "ln -s /project/class/bii_sdad_dspg/uva_2021/dspg21stw/ /sfs/qumulo/qhome/sm9dv/dspg21stw/data/")

Never save raw BGT data to GitHub or locally in the data folder.

### Github

All github code will be executed in the terminal.

Your local repository is in Rivanna - where you code in R.

Your remote repository is Github online.

##### Branches

The **main** (sometimes called master) branch will have all of the
merged code.

Your "personal" branch will track all of your code and will eventually
be merged into the main branch. Typically you will name your branch your
actually name (ie sarah) or the task you are working on (ie
data\_upload). I would recommend your name but that is up to your team.

Code for creating your branch:

    git checkout -b name_of_branch

##### "Saving" your code

As you write code, you should "push" it to your branch, which is like
saving a file.

**git add** tells Git that you want to include your code/changes in the
next commit. You can add all changes by using a period "." or only add a
specific file (ie bgt\_analysis.R).

**git commit** saves changes to your local repository. You should write
a short, informative note about what those changes are.

**git push** sends your local changes to the remote repository.

    git add .
    git commit -m "a short message about what your code is"
    git push origin your_branch

If you want to merge code from your branch to the main branch so that
everyone is working from the same code, you will have to issue a pull
request on GitHub online. Your fellow will be in charge of merging your
branch to the main branch if they see fit. The fellow should check the
box that will delete the branch remotely.

Next, you will have to get the new version of the main branch AND delete
your personal branch remotely.

    git checkout main
    git pull origin main
    git branch -d your_branch
    git fetch --prune

Then you must checkout a personal branch again so that you are not
writing directly to the main branch.

You can find this information on the [Git Reference
Sheet](https://teams.microsoft.com/l/file/A31B72A4-9378-4A44-A09D-BAB5C713E909?tenantId=7b3480c7-3707-4873-8b77-e216733a65ac&fileType=pdf&objectUrl=https%3A%2F%2Fmyuva.sharepoint.com%2Fsites%2FDSPG2021%2FShared%20Documents%2FSTW%2FGit%20Reference%20Sheet.pdf&baseUrl=https%3A%2F%2Fmyuva.sharepoint.com%2Fsites%2FDSPG2021&serviceName=teams&threadId=19:e9bb8194e2ba42c58986eff6eec46ca1@thread.tacv2&groupId=e6b09c3a-3569-475a-8028-721ab0ae77c4)
in Teams.

I would suggest planning 30 minutes for everyone to merge their code and
pull down the new version of the main branch so everyone is working from
the same code the following week.

### PGADMIN

If you want to view the actual database, sign in here:
[PGADMIN](https://pgadmin-bii.uvadcos.io/).

You will need to know the following passwords (write them down somewhere
:) )

Login username/password

SSH Tunnel Password

Database server password

You will be accessing the following **schemas**: bgt\_job and WIOA.

You probably will not be coding in the actual database, rather accessing
what is in the database through Rivanna. Sometimes it is helpful to look
through the data on the database though.

Under bgt\_job and "tables", you will see the six tables listed. There
is also something called **materialized views**. These are
"pre-computed" data from a query. For example, main\_va\_2019 was
pre-computed to contain all of the main table data for VA in 2019. If
you were to run this each time you needed it, it would take FOREVER.
That is why we use a materialized view. If there is specific data you
will need to filter from the main or cert tables often, materialized
view is the way to go.

##### Accessing through Rivanna

When working with these big datasets, you will want to make sure you
have enough cores and storage for your session. I would suggest bumping
up the number of cores to 40 and memory to at least 120. If your session
keeps crashing, consider bumping up your memory or creating a
materialized view.

Now we will go over the commands used in R in Rivanna to access this
data.

First, in your directory, in the .Renviron file in your HOME directory,
you will need to store the following:

DB\_USR

DB\_PWD

DB\_HOST

DB\_NAME

DB\_PORT

By adding these to your .Renviron outside of your github folder, you
will safely store your login information away from Github.

When you get data from the database, you will need three pieces of code:

1.  DB Connect

2.  Code for getting the data

3.  DB Disconnect

This is code you will want to copy. You will change the code for getting
the data section to whatever code you need.

    # DB Connect
    conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                                   dbname = Sys.getenv(x = "DB_NAME"),
                                   host = Sys.getenv(x = "DB_HOST"),
                                   port = Sys.getenv(x = "DB_PORT"),
                                   user = Sys.getenv(x = "DB_USR"),
                                   password = Sys.getenv(x = "DB_PWD"))

    # Code for getting the first 30 rows of the main_2019 data
    tbl <- RPostgreSQL::dbGetQuery(conn = conn, 
                                   statement = "SELECT * 
                                   FROM bgt_job.main_2019 
                                   LIMIT 30")
    # DB Disconnect
    RPostgreSQL::dbDisconnect(conn)

When dealing with the database you will use SQL. If you are unfamiliar
with SQL, I would recommend this tutorial: [SQL
Bolt](https://sqlbolt.com/lesson/select_queries_introduction).

Since you are working with the certification data, the next code chunk
shows show to get all of the certificates for 2019 in Virginia. If you
will need this data often, I would suggest making it a materialized
view.

    # DB Connect
    conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                                   dbname = Sys.getenv(x = "DB_NAME"),
                                   host = Sys.getenv(x = "DB_HOST"),
                                   port = Sys.getenv(x = "DB_PORT"),
                                   user = Sys.getenv(x = "DB_USR"),
                                   password = Sys.getenv(x = "DB_PWD"))

    certs <- RPostgreSQL::dbGetQuery(
      conn = conn,
      statement = "SELECT A.id, B.certification
              FROM bgt_job.main_2019 A
              LEFT JOIN bgt_job.cert B ON A.id = B.id
              LIMIT 30")

    # DB Disconnect
    RPostgreSQL::dbDisconnect(conn)

    ## [1] TRUE
