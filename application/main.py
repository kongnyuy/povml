import streamlit as st
import pandas as pd
import json
import folium
from streamlit_folium import st_folium
import geopandas as gpd
from lets_plot import *
from shapely.geometry import Point
import utils as utils
import pathlib
import os
from pathlib import Path

LetsPlot.setup_html()

cwd_folder = Path.cwd().name
path_prefix = ""

if cwd_folder != "application":
    path_prefix = "application/"


# -------------------------------------------------
# PAGE CONFIG
# -------------------------------------------------
st.set_page_config(layout="wide")

# -------------------------------------------------
# LOAD DATA
# -------------------------------------------------
@st.cache_data
def load_data():    
    df = pd.read_csv(pathlib.Path(path_prefix + "data/cm.subdiv.agg.csv"))
    return df

@st.cache_data
def load_data_aggregate(filename_with_ext=pathlib.Path(path_prefix + "data/cm.subdiv.agg.csv")):
    df = pd.read_csv(f"data/{filename_with_ext}")
    return df

@st.cache_data
def load_data_ebp(admin_level="region"):
    res = None
    if admin_level == "region":
        ebp = pd.read_csv(pathlib.Path(path_prefix + "data/cm.region.ebp.ind.csv"))
        ebp_mse = pd.read_csv(pathlib.Path(path_prefix + "data/cm.region.ebp.mse.csv"))
        res = {"ebp": ebp, "mse": ebp_mse}
    elif admin_level == "division":
        ebp = pd.read_csv(pathlib.Path(path_prefix + "data/cm.div.ebp.ind.csv"))
        ebp_mse = pd.read_csv(pathlib.Path(path_prefix + "data/cm.div.ebp.mse.csv"))
        res = {"ebp": ebp, "mse": ebp_mse}
    elif admin_level == "subdivision":
        ebp = pd.read_csv(pathlib.Path(path_prefix + "data/cm.subdiv.ebp.ind.csv"))
        ebp_mse = pd.read_csv(pathlib.Path(path_prefix + "data/cm.subdiv.ebp.mse.csv"))
        res = {"ebp": ebp, "mse": ebp_mse}
    return res


@st.cache_data
def load_data_agg(admin_level="region"):
    res = None
    if admin_level == "region":
        res = pd.read_csv(pathlib.Path(path_prefix + "data/cm.region.agg.csv"))
    elif admin_level == "division":
        res = pd.read_csv(pathlib.Path(path_prefix + "data/cm.div.agg.csv"))
    elif admin_level == "subdivision":
        res = pd.read_csv(pathlib.Path(path_prefix + "data/cm.subdiv.agg.csv"))   
    elif admin_level == "country":
        res = pd.read_csv(pathlib.Path(path_prefix + "data/cm.country.agg.csv"))  
    return res




# -------------------------------------------------
# LOAD GEOJSON
# -------------------------------------------------

@st.cache_data
def load_geojson():
    with open(pathlib.Path(path_prefix + "maps/cm.subdivision.geojson")) as f:
        geojson = json.load(f)
    return geojson


@st.cache_data
def load_geojson_cm(admin_level="region"):
    geojson = None
    if admin_level == "region":
        with open(pathlib.Path(path_prefix + "maps/cm.region.geojson")) as f:
            geojson = json.load(f)
    elif admin_level == "division":
        with open(pathlib.Path(path_prefix + "maps/cm.division.geojson")) as f:
            geojson = json.load(f)
    elif admin_level == "subdivision":
        # with open(pathlib.Path(path_prefix + "maps/cm.subdivision.geojson")) as f:
        with open(pathlib.Path(path_prefix + "maps/cm.subdivision_cond.geojson")) as f:
            geojson = json.load(f)
    return geojson

@st.cache_data
def load_geojson_gdf(admin_level="region"):
    if admin_level == "region":        
        gdf = gpd.read_file(pathlib.Path(path_prefix + "maps/cm.region.geojson"))
    elif admin_level == "division":        
        gdf = gpd.read_file(pathlib.Path(path_prefix + "maps/cm.division.geojson"))
    elif admin_level == "subdivision":        
        # gdf = gpd.read_file(pathlib.Path(path_prefix + "maps/cm.subdivision.geojson"))
        gdf = gpd.read_file(pathlib.Path(path_prefix + "maps/cm.subdivision_cond.geojson"))
    return gdf

def get_admin_db(admin_level):
    return load_geojson_gdf(admin_level=admin_level)



inds = {"Headcount ratio": "Head_Count", 
            "Poverty gap": "Poverty_Gap",
            "Gini coefficient": "Gini",
            "Wealth score mean": "Mean"
            }


df = load_data()
geojson = load_geojson()



# -------------------------------------------------
# SIDEBAR - MASTER SECTION
# -------------------------------------------------
st.sidebar.title("Survey Dates")

survey_dates = ["DHS 2018","DHS 2011", "DHS 2004","DHS 1998", "DHS 1991"]
selected_date = st.sidebar.selectbox("Select Survey Date", survey_dates)

if selected_date != "DHS 2018":
    st.error(f"No analysis done for survey: {selected_date}")

else:

    # -------------------------------------------------
    # MAIN DETAIL SECTION
    # -------------------------------------------------
    MAX_WEALTHSCORE_MEAN = 400000
    st.title(f"Analysis report:  {selected_date}")

    @st.dialog(title="Subdivision estimates Plot", width="large")
    def subdiv_plot_dialog(subdiv_data  = None, _ind = "Head_Count"):
        if subdiv_data is None:
            subdiv_data = load_data_agg(admin_level="subdivision")
        
        thress = st.slider(f"Filter by {indicator}. which areas have valus less than", value=float(1) if _ind != "Mean" else float(MAX_WEALTHSCORE_MEAN), step= 0.1 if _ind != "Mean" else float(100), min_value=0.0, max_value=1.0 if _ind != "Mean" else float(MAX_WEALTHSCORE_MEAN))     
        subdiv_disp_data = subdiv_data[subdiv_data[indicator] < thress] 
        st.bar_chart(subdiv_disp_data, x="subdivision", y=_ind, color=_ind, horizontal=True)
        



    main_tab_agg, main_tab_ebp, main_tab_agg_maps = st.tabs(["Aggregated Estimates", "EBP Estimates", "Aggregated Maps"])


    def to_indicator_selector(val):
        return inds[val]


    with main_tab_agg:
        st.subheader("Aggregates by administrative level")
        indicator = st.radio("Select indicator to display", list(inds.keys()))
        indicator = to_indicator_selector(indicator)

        slider_ind_filter_thresshold = st.slider(f"Filter by {indicator}. which areas have valus less than", value=float(1) if indicator != "Mean" else float(MAX_WEALTHSCORE_MEAN), step= 0.1 if indicator != "Mean" else float(100), min_value=0.0, max_value=1.0 if indicator != "Mean" else float(MAX_WEALTHSCORE_MEAN))
        tab_country, tab_region, tab_division, tab_subdivision = st.tabs(["Country", "Region", "Division", "Subdivision"])
        with tab_country:
            st.table(load_data_agg(admin_level="country"))  
        with tab_region:      
            region_data = load_data_agg(admin_level="region")
            # slider_ind_filter_thresshold = st.slider(f"Filter by {indicator}. which regions have valus less than", value=float(1), step= 0.1, min_value=0.0, max_value=1.0)     
            disp_data = region_data[region_data[indicator] < slider_ind_filter_thresshold]
            st.bar_chart(disp_data, x="region", y=indicator, color=indicator)            
            st.table(disp_data)
        with tab_division:        
            div_data = load_data_agg(admin_level="division")            
            div_disp_data = div_data[div_data[indicator] < slider_ind_filter_thresshold]    
            st.bar_chart(div_disp_data, x="division", y=indicator, color=indicator) 
            st.table(div_disp_data)
        with tab_subdivision:
            subdiv_data = load_data_agg(admin_level="subdivision")                        
            btn_show_plot = st.button("Show Distribution of Poverty indicators by Subdivision plot")
            if btn_show_plot:
                subdiv_plot_dialog(subdiv_data=subdiv_data, _ind=indicator)   
            subdiv_disp_data = subdiv_data[subdiv_data[indicator] < slider_ind_filter_thresshold]         
            st.table(subdiv_disp_data)


    with main_tab_ebp:
        st.subheader("EBP Estimates by administrative level")

        # application context
        admin_levels = ["region", "division", "subdivision"]
        ctx_current_admin_level = st.selectbox("Select Admin Level", admin_levels)
        ctx_geojson = load_geojson_cm(admin_level=ctx_current_admin_level)
        ctx_data_ebp = load_data_ebp(admin_level=ctx_current_admin_level)        




        tab_overview, tab_ebp, tab_mse = st.tabs(["Overview", "EBP Estimates", "MSE Estimates"])    

        # =================================================
        # TAB 1: OVERVIEW
        # =================================================
        with tab_overview:

            col_map, col_info = st.columns([1.5, 1.5])

            with col_map:
                st.subheader("Interactive Map")

                # Base map
                m = folium.Map(location=[0, 0], zoom_start=6)

                def style_function(feature):
                    return {
                        "fillColor": "#3186cc",
                        "color": "black",
                        "weight": 1,
                        "fillOpacity": 0.5,
                    }
                
                gdf = load_geojson_gdf(admin_level=ctx_current_admin_level)

                geo = folium.GeoJson(
                    gdf,
                    name=f"{ctx_current_admin_level}",
                    style_function=style_function,
                    highlight_function=lambda feature: {
                        "fillColor": "yellow",
                        "color": "black",
                        "weight": 3,
                        "fillOpacity": 0.7,
                    },
                tooltip=folium.GeoJsonTooltip(fields=[ctx_current_admin_level]),
                ).add_to(m)

                map_data = st_folium(m, width=600, height=600)

            with col_info:
                st.subheader("Area Information")

                if map_data and map_data.get("last_clicked"):
                    clicked = map_data["last_clicked"]
                    st.write("Coordinates:", clicked)
                    lat = clicked["lat"]
                    lon = clicked["lng"]
                    point = Point(lon, lat)

                    # Find the polygon that contains the clicked point
                    clicked_region = gdf[gdf.contains(point)]
                    if not clicked_region.empty:
                        region_name = clicked_region.iloc[0][ctx_current_admin_level]
                        st.write(f"Clicked on {ctx_current_admin_level}:", region_name)
                        # wiki_summary = utils.fetch_wikipedia_cameroon_summary(region_name)
                        # st.markdown(f"## {region_name}")    
                        # st.write(wiki_summary)
                        admin_db = get_admin_db(ctx_current_admin_level)

                        area_info = utils.area_info(region_name, ctx_current_admin_level, admin_db)
                        if area_info == None:
                            st.write("No area information found")
                        else:
                            st.write(area_info)                            


                    else:
                        st.write("Clicked outside regions")
                else:
                    st.info("Click on an area in the map to see details.")

        # =================================================
        # TAB 2: ESTIMATES
        # =================================================
        with tab_ebp:
            st.table(ctx_data_ebp["ebp"])


        with tab_mse:
            st.table(ctx_data_ebp["mse"])    

    with main_tab_agg_maps:
        st.subheader("Aggregated Maps")
        # display_indicator = st.selectbox("Select indicator to display", ["Head_Count", "Mean", "Poverty_Gap","Gini"])
        display_indicator = to_indicator_selector(st.selectbox("Select indicator to display", list(inds.keys())))
        HEIGHT = 1000
        WIDTH = 1000
        col1, col2,col3 = st.columns(3)
        

        # st.caption("Headcount ratio")
        with col1:
            gdf = gpd.read_file(pathlib.Path(path_prefix + "maps/cm.subdivision.geojson"))
            geojson_dict = json.loads(gdf.to_json())
            p2 = ggplot() + geom_map(aes(fill=display_indicator) ,map=gdf, alpha=0.5,tooltips=layer_tooltips().line('Subdivision: @subdivision').format('Mean', '.2f').line('Mean value: @Mean').format('Head_Count', '.2f').line('Headcount ration: @Head_Count'),color='white')
            st.components.v1.html(p2.to_html(), height=HEIGHT,width=WIDTH)
        with col2:
            gdf = gpd.read_file(pathlib.Path(path_prefix + "maps/cm.division.geojson"))
            geojson_dict = json.loads(gdf.to_json())
            p2 = ggplot() + geom_map(aes(fill=display_indicator) ,map=gdf, alpha=0.5,tooltips=layer_tooltips().line('Subdivision: @division').format('Mean', '.2f').line('Mean value: @Mean').format('Head_Count', '.2f').line('Headcount ration: @Head_Count'),color='white')
            st.components.v1.html(p2.to_html(), height=HEIGHT, width=WIDTH)
        with col3:
            gdf = gpd.read_file(pathlib.Path(path_prefix + "maps/cm.region.geojson"))
            geojson_dict = json.loads(gdf.to_json())
            p2 = ggplot() + geom_map(aes(fill=display_indicator) ,map=gdf, alpha=0.5,tooltips=layer_tooltips().line('Subdivision: @region').format('Mean', '.2f').line('Mean value: @Mean').format('Head_Count', '.2f').line('Headcount ration: @Head_Count'),color='white')
            st.components.v1.html(p2.to_html(), height=HEIGHT, width=WIDTH)    