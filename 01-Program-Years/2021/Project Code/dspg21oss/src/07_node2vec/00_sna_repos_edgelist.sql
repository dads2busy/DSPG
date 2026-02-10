-- first, make repos_subset_final in /dspg21oss/05_scrape_readmes/

CREATE MATERIALIZED VIEW gh_2007_2020.repos_subnet_final AS (
	select * 
	from gh_2007_2020.repos_subset_final
	limit 500 
);

CREATE MATERIALIZED VIEW gh_2007_2020.commits_per_user_subnet AS (

WITH C AS (
	-- first, we inner join the refined commits table with the subset of the data
	-- from the dspg summer project with valid readme/pop stats data (157k subset)
	select a.slug, a.login, EXTRACT(YEAR FROM a.committed_date)::int AS year
	from gh.commits_dd_nmrc_jbsc A
	inner join gh_2007_2020.repos_subnet_final B
	on a.slug = b.slug
	-- removing all of the null users 
	where a.login is not NULL and a.login != 'null'
  --limit 100
), D AS (
	-- then we group by to quantify the commit count 
	SELECT slug, year, login, COUNT(*) AS commits
	FROM C
	GROUP BY slug, year, login
)

select slug, login, year, commits
from D 
--limit 100 

);

CREATE MATERIALIZED VIEW gh_2007_2020.sna_repos_subnet_edges AS (

WITH F AS (
	-- goal is to create a repo-repo edgelist with # ctrs as weight
	-- so join login-slug combos by year and join with same table 
	SELECT D.login, D.year, D.slug AS slug1, E.slug AS slug2
	FROM gh_2007_2020.commits_per_user_subnet D
	INNER JOIN gh_2007_2020.commits_per_user_subnet AS E 
	ON D.year = E.year AND D.login = E.login
	-- then remove duplicate rows of A-B, B-A and loops (A-A, B-B)
	WHERE D.slug < E.slug AND D.slug != E.slug
	-- and cut down table to only join certain years
	AND D.YEAR BETWEEN 2008 AND 2019 AND E.YEAR BETWEEN 2008 AND 2019
	--limit 100
)

SELECT slug1, slug2, COUNT(*) AS weight
FROM F
GROUP BY slug1, slug2
ORDER BY weight DESC
--limit 100

);

GRANT ALL PRIVILEGES ON gh_2007_2020.sna_repos_157k_edges TO ncses_oss;