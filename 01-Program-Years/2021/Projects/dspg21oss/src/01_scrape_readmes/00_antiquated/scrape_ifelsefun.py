github_username = os.environ.get("GITHUB_USERNAME")
github_token = os.environ.get("GITHUB_TOKEN")

# can only make 2500 calls per hour 
# because the function calls twice each time 
PER_HOUR = 3600

@sleep_and_retry
@limits(calls=2500, period=PER_HOUR)
def scrape_readmes(slug):
    
    # define url based on the slug 
    url = f'https://api.github.com/repos/{slug}/readme'
    response = requests.get(url, auth=(github_username, github_token))
    
    response_code = response.status_code
    if response_code == 404: 
        print(f"404 error on {slug}")
        readme_string = "404 ERROR - NO README"
        now = datetime.now()
        current_time = now.strftime("%Y-%m-%d %H:%M:%S")
        return slug, readme_string, current_time, "Done"
    
    elif response_code == 403:
        now = datetime.now()
        current_time = now.strftime("%Y-%m-%d %H:%M:%S")
        return print(f"403 error on {slug} at {current_time}")
        
    elif response_code != 200:
        raise Exception(response.status_code, response.text)
        return 
    
    elif response_code == 200:
        html_content = response.content
        soup = BeautifulSoup(html_content, 'html.parser')
        site_json=json.loads(soup.text)
        readme_link = site_json['download_url']
    
        # then scrape the readme 
        readme_response = requests.get(readme_link, auth=(github_username, github_token))
        readme_response_code = readme_response.status_code
        if readme_response_code != 200:
            raise Exception(readme_response_code.status_code, readme_response_code.text)
            return 
    
        # pull the content out of the readme 
        readme_content = readme_response.content
        readme_soup = BeautifulSoup(readme_content, 'html.parser')
        readme_string = str(readme_soup)
    
        #give us the the timing and status 
        now = datetime.now()
        current_time = now.strftime("%Y-%m-%d %H:%M:%S")
        print(readme_string)
        return slug, readme_string, current_time, "Done"
    

if __name__ == "__main__":
    print("Started scraping")
    scrape_readmes(slug = 'brandonleekramer/diversity')
    print("Finished scraping")