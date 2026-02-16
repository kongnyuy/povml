import requests

headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.11",
    "Accept-Language": "en-US,en;q=0.9",
}

def fetch_wikipedia_summary(region_name):
    url = f"https://en.wikipedia.org/api/rest_v1/page/summary/{region_name}"
    response = requests.get(url)
    if response.status_code == 200:
        data = response.json()
        return data.get("extract", "No summary available.")
    return "No Wikipedia information found."


#https://en.wikipedia.org/w/api.php?action=query&titles=Centre_Region_(Cameroon)&prop=extracts&explaintext&format=json

def fetch_wikipedia_cameroon_summary(region_name):
    url = f"https://en.wikipedia.org/w/api.php?action=query&titles={region_name}_Region_(Cameroon)&prop=extracts&explaintext&format=json"    
    response = requests.get(url, headers=headers)
    
    if response.status_code == 200:
        data = response.json()
        pages = data.get('query', {}).get('pages', {})
        extract = ''
        if pages:
            page = next(iter(pages.values()))
            extract = page.get('extract', '')
        else:
            extract = ''
        return extract

        return data.get("extract", "No summary available.")
    return "No Wikipedia information found."



def fetch_worldbank_data(country_code="CM"): 
    url = f"https://api.worldbank.org/v2/country/{country_code}/indicator/SP.POP.TOTL?format=json"
    response = requests.get(url)
    if response.status_code == 200:
        data = response.json()
        if len(data) > 1 and data[1]:
            latest = data[1][0]
            return latest["value"]
    return None
