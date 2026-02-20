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

        # return data.get("extract", "No summary available.")
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

def _has_subdiv(alev):
    if alev == "subdivision":
        return True
    else:
        return False

def _has_div(alev):
    if alev == "subdivision" or alev == "division":
        return True
    else:
        return False

def _has_region(alev):
    if alev == "subdivision" or alev == "division" or alev == "region":
        return True
    else:
        return False


def area_info(region_name, ctx_admin_level, admin_db):
    if region_name not in admin_db[ctx_admin_level].values:
        return None
    d = admin_db[admin_db[ctx_admin_level] == region_name]

    res = {
        "Area label": d[ctx_admin_level],
        "Head count ratio": d["Head_Count"],
        "Poverty gap": d["Poverty_Gap"],
        "Gini coefficient": d["Gini"],
    }

    if _has_subdiv(ctx_admin_level):
        res["Subdivision"] = d["subdivision"]
    if _has_div(ctx_admin_level):
        res["Division"] = d["division"]
    if _has_region(ctx_admin_level):        
        res["Region"] = d["region"]
    
    return res

    