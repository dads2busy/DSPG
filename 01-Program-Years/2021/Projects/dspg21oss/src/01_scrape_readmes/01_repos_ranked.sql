-- this just ranks the repos we want to scrape 
create table gh_2007_2020.repos_ranked as 
    select * 
    from gh_2007_2020.repos
    order by commits desc; 
    
-- and grants permission to everyone on the team 
create index slug_ranked_idx ON gh_2007_2020.repos_ranked (slug);
alter table gh_2007_2020.repos_ranked owner to ncses_oss; 