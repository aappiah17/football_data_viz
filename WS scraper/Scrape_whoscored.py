import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from selenium import webdriver
#import main
#import visuals
import seaborn as sns
from webdriver_manager.chrome import ChromeDriverManager
import lxml
import pip
import time
import ast
#pip.main(['install','mplsoccer'])
#pip.main(['install','tqdm'])
#pip install tqdm

import time
import pandas as pd
import json
from bs4 import BeautifulSoup as soup
import re
from collections import OrderedDict
import datetime
from datetime import datetime as dt
import itertools
import numpy as np
import pip

pip.main(['install', 'mplsoccer'])
import mplsoccer

import pip

pip.main(['install', 'webdriver_manager'])
from webdriver_manager.chrome import ChromeDriverManager

try:
	from tqdm import trange
except ModuleNotFoundError:
	pass

from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.support.ui import Select

TRANSLATE_DICT = {'Jan': 'Jan',
                 'Feb': 'Feb',
                 'Mac': 'Mar',
                 'Apr': 'Apr',
                 'Mei': 'May',
                 'Jun': 'Jun',
                 'Jul': 'Jul',
                 'Ago': 'Aug',
                 'Sep': 'Sep',
                 'Okt': 'Oct',
                 'Nov': 'Nov',
                 'Des': 'Dec',
                 'Jan': 'Jan',
                 'Feb': 'Feb',
                 'Mar': 'Mar',
                 'Apr': 'Apr',
                 'May': 'May',
                 'Jun': 'Jun',
                 'Jul': 'Jul',
                 'Aug': 'Aug',
                 'Sep': 'Sep',
                 'Oct': 'Oct',
                 'Nov': 'Nov',
                 'Dec': 'Dec'}

main_url = 'https://whoscored.com/'


def getLeagueUrls(minimize_window=True):
	driver = webdriver.Chrome(ChromeDriverManager().install())

	if minimize_window:
		driver.minimize_window()

	driver.get(main_url)
	league_names = []
	league_urls = []
	for i in range(21):
		league_name = driver.find_element_by_xpath('//*[@id="popular-tournaments-list"]/li[' + str(i + 1) + ']/a').text
		league_link = driver.find_element_by_xpath(
			'//*[@id="popular-tournaments-list"]/li[' + str(i + 1) + ']/a').get_attribute('href')
		league_names.append(league_name)
		league_urls.append(league_link)

	for link in league_urls:
		if 'Russia' in link:
			r_index = league_urls.index(link)

	league_names[r_index] = 'Russian Premier League'

	leagues = {}
	for name, link in zip(league_names, league_urls):
		leagues[name] = link
	driver.close()
	return leagues


def getMatchUrls(comp_urls, competition, season, maximize_window=True):
	driver = webdriver.Chrome(ChromeDriverManager().install())

	if maximize_window:
		driver.maximize_window()

	comp_url = comp_urls[competition]
	driver.get(comp_url)
	time.sleep(5)

	seasons = driver.find_element_by_xpath('//*[@id="seasons"]').get_attribute('innerHTML').split(sep='\n')
	seasons = [i for i in seasons if i]

	for i in range(1, len(seasons) + 1):
		if driver.find_element_by_xpath('//*[@id="seasons"]/option[' + str(i) + ']').text == season:
			driver.find_element_by_xpath('//*[@id="seasons"]/option[' + str(i) + ']').click()

			time.sleep(5)
			try:
				stages = driver.find_element_by_xpath('//*[@id="stages"]').get_attribute('innerHTML').split(sep='\n')
				stages = [i for i in stages if i]

				all_urls = []

				for i in range(1, len(stages) + 1):
					if competition == 'Champions League' or competition == 'Europa League':
						if 'Group Stages' in driver.find_element_by_xpath('//*[@id="stages"]/option[' + str(
								i) + ']').text or 'Final Stage' in driver.find_element_by_xpath(
							'//*[@id="stages"]/option[' + str(i) + ']').text:
							driver.find_element_by_xpath('//*[@id="stages"]/option[' + str(i) + ']').click()
							time.sleep(5)

							driver.execute_script("window.scrollTo(0, 400)")

							match_urls = getFixtureData(driver)

							match_urls = getSortedData(match_urls)

							match_urls2 = [url for url in match_urls if
										   '?' not in url['date'] and '\n' not in url['date']]

							all_urls += match_urls2
						else:
							continue

					elif competition == 'Major League Soccer':
						if 'Grp. ' not in driver.find_element_by_xpath('//*[@id="stages"]/option[' + str(i) + ']').text:
							driver.find_element_by_xpath('//*[@id="stages"]/option[' + str(i) + ']').click()
							time.sleep(5)

							driver.execute_script("window.scrollTo(0, 400)")

							match_urls = getFixtureData(driver)

							match_urls = getSortedData(match_urls)

							match_urls2 = [url for url in match_urls if
										   '?' not in url['date'] and '\n' not in url['date']]

							all_urls += match_urls2
						else:
							continue

					else:
						driver.find_element_by_xpath('//*[@id="stages"]/option[' + str(i) + ']').click()
						time.sleep(5)

						driver.execute_script("window.scrollTo(0, 400)")

						match_urls = getFixtureData(driver)

						match_urls = getSortedData(match_urls)

						match_urls2 = [url for url in match_urls if '?' not in url['date'] and '\n' not in url['date']]

						all_urls += match_urls2

			except NoSuchElementException:
				all_urls = []

				driver.execute_script("window.scrollTo(0, 400)")

				match_urls = getFixtureData(driver)

				match_urls = getSortedData(match_urls)

				match_urls2 = [url for url in match_urls if '?' not in url['date'] and '\n' not in url['date']]

				all_urls += match_urls2

			remove_dup = [dict(t) for t in {tuple(sorted(d.items())) for d in all_urls}]
			all_urls = getSortedData(remove_dup)

			driver.close()

			return all_urls

	season_names = [re.search(r'\>(.*?)\<', season).group(1) for season in seasons]
	driver.close()
	print('Seasons available: {}'.format(season_names))
	raise ('Season Not Found.')


def getEuroMatchUrls(comp_url, season, weeks, maximize_window=True):
	driver = webdriver.Chrome(ChromeDriverManager().install())
	if maximize_window:
		driver.maximize_window()

	# teams = []
	driver.get(comp_url)
	time.sleep(5)

	seasons = driver.find_element_by_xpath('//*[@id="seasons"]').get_attribute('innerHTML').split(sep='\n')
	seasons = [i for i in seasons if i]

	for i in range(1, len(seasons) + 1):
		if driver.find_element_by_xpath('//*[@id="seasons"]/option[' + str(i) + ']').text == season:
			season = driver.find_element_by_xpath('//*[@id="seasons"]/option[' + str(i) + ']').click()
	time.sleep(3)
	date_config_btn = driver.find_element_by_xpath('//*[@id="date-config-toggle-button"]').click()

	match_urls = getFixtureData(driver, weeks)

	match_urls = getSortedData(match_urls)

	driver.close()
	return match_urls


def getTeamUrls(team, match_urls):
	team_data = []
	for fixture in match_urls:
		if fixture['home'] == team or fixture['away'] == team:
			team_data.append(fixture)
	team_data = [a[0] for a in itertools.groupby(team_data)]

	return team_data


def getMatchesData(match_urls, minimize_window=True):
	matches = []

	driver = webdriver.Chrome(ChromeDriverManager().install())
	if minimize_window:
		driver.minimize_window()

	try:
		for i in trange(len(match_urls), desc='Getting Match Data'):
			match_data = getMatchData(driver, main_url + match_urls[i]['url'], display=False, close_window=False)
			matches.append(match_data)
	except NameError:
		print('Recommended: \'pip install tqdm\' for a progress bar while the data gets scraped....')
		for i in range(len(match_urls)):
			match_data = getMatchData(driver, main_url + match_urls[i]['url'], display=False, close_window=False)
			matches.append(match_data)

	driver.close()

	return matches


def getFixtureData(driver):
	matches_ls = []
	while True:
		table_rows = driver.find_elements_by_class_name('divtable-row')
		if len(table_rows) == 0:
			break
		for row in table_rows:
			match_dict = {}
			element = soup(row.get_attribute('innerHTML'), features='lxml')
			link_tag = element.find("a", {"class": "result-1 rc"})
			if type(link_tag) is type(None):
				if type(element.find('span', {'class': 'status-1 rc'})) is type(None):
					date = row.text.split(', ')[-1]
			if type(link_tag) is not type(None):
				match_dict['date'] = date
				match_dict['time'] = element.find('div', {
					'class': 'col12-lg-1 col12-m-1 col12-s-0 col12-xs-0 time divtable-data'}).text
				match_dict['home'] = element.find_all("a", {"class": "team-link"})[0].text
				match_dict['away'] = element.find_all("a", {"class": "team-link"})[1].text
				match_dict['score'] = element.find("a", {"class": "result-1 rc"}).text
				match_dict['url'] = link_tag.get("href")
			matches_ls.append(match_dict)

		prev_month = driver.find_element_by_xpath('//*[@id="date-controller"]/a[1]').click()
		time.sleep(2)
		if driver.find_element_by_xpath('//*[@id="date-controller"]/a[1]').get_attribute(
				'title') == 'No data for previous week':
			table_rows = driver.find_elements_by_class_name('divtable-row')
			for row in table_rows:
				match_dict = {}
				element = soup(row.get_attribute('innerHTML'), features='lxml')
				link_tag = element.find("a", {"class": "result-1 rc"})
				if type(link_tag) is type(None):
					if type(element.find('span', {'class': 'status-1 rc'})) is type(None):
						date = row.text.split(', ')[-1]
				if type(link_tag) is not type(None):
					match_dict['date'] = date
					match_dict['time'] = element.find('div', {
						'class': 'col12-lg-1 col12-m-1 col12-s-0 col12-xs-0 time divtable-data'}).text
					match_dict['home'] = element.find_all("a", {"class": "team-link"})[0].text
					match_dict['away'] = element.find_all("a", {"class": "team-link"})[1].text
					match_dict['score'] = element.find("a", {"class": "result-1 rc"}).text
					match_dict['url'] = link_tag.get("href")
				matches_ls.append(match_dict)
			break

	matches_ls = list(filter(None, matches_ls))

	return matches_ls


def translateDate(data):
	for match in data:
		date = match['date'].split()
		match['date'] = ' '.join([TRANSLATE_DICT[date[0]], date[1], date[2]])

	return data


def getSortedData(data):
	try:
		data = sorted(data, key=lambda i: dt.strptime(i['date'], '%b %d %Y'))
		return data
	except ValueError:
		data = translateDate(data)
		data = sorted(data, key=lambda i: dt.strptime(i['date'], '%b %d %Y'))
		return data


# some gibberish

def getMatchData(driver, url, display=True, close_window=True):
	driver.get(url)

	# get script data from page source
	script_content = driver.find_element_by_xpath('//*[@id="layout-wrapper"]/script[1]').get_attribute('innerHTML')

	# clean script content
	script_content = re.sub(r"[\n\t]*", "", script_content)
	script_content = script_content[script_content.index("matchId"):script_content.rindex("}")]

	# this will give script content in list form
	script_content_list = list(filter(None, script_content.strip().split(',            ')))
	metadata = script_content_list.pop(1)

	# string format to json format
	match_data = json.loads(metadata[metadata.index('{'):])
	keys = [item[:item.index(':')].strip() for item in script_content_list]
	values = [item[item.index(':') + 1:].strip() for item in script_content_list]
	for key, val in zip(keys, values):
		match_data[key] = json.loads(val)

	# get other details about the match
	region = driver.find_element_by_xpath('//*[@id="breadcrumb-nav"]/span[1]').text
	league = driver.find_element_by_xpath('//*[@id="breadcrumb-nav"]/a').text.split(' - ')[0]
	season = driver.find_element_by_xpath('//*[@id="breadcrumb-nav"]/a').text.split(' - ')[1]
	if len(driver.find_element_by_xpath('//*[@id="breadcrumb-nav"]/a').text.split(' - ')) == 2:
		competition_type = 'League'
		competition_stage = ''
	elif len(driver.find_element_by_xpath('//*[@id="breadcrumb-nav"]/a').text.split(' - ')) == 3:
		competition_type = 'Knock Out'
		competition_stage = driver.find_element_by_xpath('//*[@id="breadcrumb-nav"]/a').text.split(' - ')[-1]
	else:
		print('Getting more than 3 types of information about the competition.')

	match_data['region'] = region
	match_data['league'] = league
	match_data['season'] = season
	match_data['competitionType'] = competition_type
	match_data['competitionStage'] = competition_stage

	# sort match_data dictionary alphabetically
	match_data = OrderedDict(sorted(match_data.items()))
	match_data = dict(match_data)
	if display:
		print('Region: {}, League: {}, Season: {}, Match Id: {}'.format(region, league, season, match_data['matchId']))

	if close_window:
		driver.close()

	return match_data


def createEventsDF(data):
	events = data['events']
	for event in events:
		event.update({'matchId': data['matchId'],
					  'startDate': data['startDate'],
					  'startTime': data['startTime'],
					  'score': data['score'],
					  'ftScore': data['ftScore'],
					  'htScore': data['htScore'],
					  'etScore': data['etScore'],
					  'venueName': data['venueName'],
					  'maxMinute': data['maxMinute'],
					  })
	events_df = pd.DataFrame(events)

	# clean period column
	events_df['period'] = pd.json_normalize(events_df['period'])['displayName']

	# clean type column
	events_df['type'] = pd.json_normalize(events_df['type'])['displayName']

	# clean outcomeType column
	events_df['outcomeType'] = pd.json_normalize(events_df['outcomeType'])['displayName']

	# clean outcomeType column
	try:
		x = events_df['cardType'].fillna({i: {} for i in events_df.index})
		events_df['cardType'] = pd.json_normalize(x)['displayName'].fillna(False)
	except KeyError:
		events_df['cardType'] = False

	# clean satisfiedEventTypes column
	eventTypeDict = data['matchCentreEventTypeJson']
	for i in range(len(events_df)):
		row = events_df.loc[i, 'satisfiedEventsTypes'].copy()
		events_df['satisfiedEventsTypes'].loc[i] = [
			list(eventTypeDict.keys())[list(eventTypeDict.values()).index(event)] for event in row]

	# clean qualifiers column
	try:
		for i in events_df.index:
			row = events_df.loc[i, 'qualifiers'].copy()
			if len(row) != 0:
				for irow in range(len(row)):
					row[irow]['type'] = row[irow]['type']['displayName']
	except TypeError:
		pass

	# clean isShot column
	if 'isShot' in events_df.columns:
		events_df['isShot'] = events_df['isShot'].replace(np.nan, False)
	else:
		events_df['isShot'] = False

	# clean isGoal column
	if 'isGoal' in events_df.columns:
		events_df['isGoal'] = events_df['isGoal'].replace(np.nan, False)
	else:
		events_df['isGoal'] = False

	# add player name column
	events_df.loc[events_df.playerId.notna(), 'playerId'] = events_df.loc[
		events_df.playerId.notna(), 'playerId'].astype(int).astype(str)
	player_name_col = events_df.loc[:, 'playerId'].map(data['playerIdNameDictionary'])
	events_df.insert(loc=events_df.columns.get_loc("playerId") + 1, column='playerName', value=player_name_col)

	# add home/away column
	h_a_col = events_df['teamId'].map({data['home']['teamId']: 'h', data['away']['teamId']: 'a'})
	events_df.insert(loc=events_df.columns.get_loc("teamId") + 1, column='h_a', value=h_a_col)
	# add team name column
	team_name_col = events_df['teamId'].map({data['home']['teamId']: data['home']['name'],
											 data['away']['teamId']: data['away']['name']})
	events_df.insert(loc=events_df.columns.get_loc("teamId") + 2, column='team_name', value=team_name_col)

	# adding shot body part column
	events_df['shotBodyType'] = np.nan
	for i in events_df.loc[events_df.isShot == True].index:
		for j in events_df.loc[events_df.isShot == True].qualifiers.loc[i]:
			if j['type'] == 'RightFoot' or j['type'] == 'LeftFoot' or j['type'] == 'Head' or j[
				'type'] == 'OtherBodyPart':
				events_df['shotBodyType'].loc[i] = j['type']

	# adding shot situation column
	events_df['situation'] = np.nan
	for i in events_df.loc[events_df.isShot == True].index:
		for j in events_df.loc[events_df.isShot == True].qualifiers.loc[i]:
			if j['type'] == 'FromCorner' or j['type'] == 'SetPiece' or j['type'] == 'DirectFreekick' or j[
				'type'] == 'Penalty':
				events_df['situation'].loc[i] = j['type']
			if j['type'] == 'ThrowinSetPiece':
				events_df['situation'].loc[i] = 'SetPiece'
			if j['type'] == 'RegularPlay' or j['type'] == 'FastBreak':
				events_df['situation'].loc[i] = 'OpenPlay'

			# adding other event types columns
	event_types = list(data['matchCentreEventTypeJson'].keys())
	for event_type in event_types:
		events_df[event_type] = pd.Series([event_type in row for row in list(events_df['satisfiedEventsTypes'])])

	return events_df


def createMatchesDF(data):
	columns_req_ls = ['matchId', 'attendance', 'venueName', 'startTime', 'startDate',
					  'score', 'home', 'away', 'referee']
	matches_df = pd.DataFrame(columns=columns_req_ls)
	if type(data) == dict:
		matches_dict = dict([(key, val) for key, val in data.items() if key in columns_req_ls])
		matches_df = matches_df.append(matches_dict, ignore_index=True)
	else:
		for match in data:
			matches_dict = dict([(key, val) for key, val in match.items() if key in columns_req_ls])
			matches_df = matches_df.append(matches_dict, ignore_index=True)

	matches_df = matches_df.set_index('matchId')
	return matches_df


def load_EPV_grid(fname='EPV_grid.csv'):
	""" load_EPV_grid(fname='EPV_grid.csv')

    # load pregenerated EPV surface from file.

    Parameters
    -----------
        fname: filename & path of EPV grid (default is 'EPV_grid.csv' in the curernt directory)

    Returns
    -----------
        EPV: The EPV surface (default is a (32,50) grid)

    """
	epv = np.loadtxt(fname, delimiter=',')
	return epv


def get_EPV_at_location(position, EPV, attack_direction, field_dimen=(106., 68.)):
	""" get_EPV_at_location

    Returns the EPV value at a given (x,y) location

    Parameters
    -----------
        position: Tuple containing the (x,y) pitch position
        EPV: tuple Expected Possession value grid (loaded using load_EPV_grid() )
        attack_direction: Sets the attack direction (1: left->right, -1: right->left)
        field_dimen: tuple containing the length and width of the pitch in meters. Default is (106,68)

    Returrns
    -----------
        EPV value at input position

    """

	x, y = position
	if abs(x) > field_dimen[0] / 2. or abs(y) > field_dimen[1] / 2.:
		return 0.0  # Position is off the field, EPV is zero
	else:
		if attack_direction == -1:
			EPV = np.fliplr(EPV)
		ny, nx = EPV.shape
		dx = field_dimen[0] / float(nx)
		dy = field_dimen[1] / float(ny)
		ix = (x + field_dimen[0] / 2. - 0.0001) / dx
		iy = (y + field_dimen[1] / 2. - 0.0001) / dy
		return EPV[int(iy), int(ix)]


def to_metric_coordinates_from_whoscored(data, field_dimen=(106., 68.)):
	'''
    Convert positions from Whoscored units to meters (with origin at centre circle)
    '''
	x_columns = [c for c in data.columns if c[-1].lower() == 'x'][:2]
	y_columns = [c for c in data.columns if c[-1].lower() == 'y'][:2]
	x_columns_mod = [c + '_metrica' for c in x_columns]
	y_columns_mod = [c + '_metrica' for c in y_columns]
	data[x_columns_mod] = (data[x_columns] / 100 * 106) - 53
	data[y_columns_mod] = (data[y_columns] / 100 * 68) - 34
	return data


def addEpvToDataFrame(data):
	# loading EPV data
	EPV = load_EPV_grid('EPV_grid.csv')

	# converting opta coordinates to metric coordinates
	data = to_metric_coordinates_from_whoscored(data)

	# calculating EPV for events
	EPV_difference = []
	for i in data.index:
		if data.loc[i, 'type'] == 'Pass' and data.loc[i, 'outcomeType'] == 'Successful':
			start_pos = (data.loc[i, 'x_metrica'], data.loc[i, 'y_metrica'])
			start_epv = get_EPV_at_location(start_pos, EPV, attack_direction=1)

			end_pos = (data.loc[i, 'endX_metrica'], data.loc[i, 'endY_metrica'])
			end_epv = get_EPV_at_location(end_pos, EPV, attack_direction=1)

			diff = end_epv - start_epv
			EPV_difference.append(diff)

		else:
			EPV_difference.append(np.nan)

	data = data.assign(EPV_difference=EPV_difference)

	# dump useless columns
	drop_cols = ['x_metrica', 'endX_metrica', 'y_metrica',
				 'endY_metrica']
	data.drop(drop_cols, axis=1, inplace=True)
	data.rename(columns={'EPV_difference': 'EPV'}, inplace=True)

	return data



#import importlib #this can update main assuming you make any changes to it and need to reload
#importlib.reload(main)
# Get single match event data

def get_match_data(matchurl):
	driver = webdriver.Chrome(ChromeDriverManager().install())
	url = matchurl
	min_dribble_length: float = 0.0
	max_dribble_length: float = 100.0
	max_dribble_duration: float = 20.0
	# Games data
	game = getMatchData(driver, url, close_window=True)

	# Events dataframe
	game_df = createEventsDF(game)
	game_df['time_seconds']=game_df['minute']*60+game_df['second']
	next_actions = game_df.shift(-1)
	same_team = game_df.teamId == next_actions.teamId
	dx = game_df.endX - next_actions.x
	dy = game_df.endY- next_actions.y
	far_enough=dy** 2 + dy** 2 >= min_dribble_length** 2
	not_too_far = dx ** 2+ dy** 2<= max_dribble_length ** 2
	dt = next_actions.time_seconds - game_df.time_seconds
	same_phase = dt < max_dribble_duration
	same_period = game_df.period == next_actions.period
	dribble_idx = same_team & far_enough & not_too_far & same_phase & same_period
	dribbles = pd.DataFrame()
	prev = game_df[dribble_idx]
	nex= next_actions[dribble_idx]
	dribbles['matchId' ] = nex.matchId
	dribbles['period' ] = nex.period
	for cols in ['expandedMinute']:
		dribbles[cols] = nex[cols]
	for cols in ['passKey','assist']:
		dribbles[ cols] = [True for _ in range (len (dribbles))]
	dribbles['isTouch'] = [True for _ in range (len (dribbles))]
	morecols = ['playerId', 'team_name','playerName','h_a']
	for cols in morecols:
		dribbles[cols] = nex[cols]
	dribbles['eventId'] = prev.eventId + 0.1
	dribbles['time_seconds' ] = (prev.time_seconds + nex.time_seconds) / 2
	dribbles['teamId'] = nex.teamId
	dribbles['x'] = prev.endX
	dribbles['y']= prev.endY
	dribbles['endX'] = nex.x
	dribbles['endY'] = nex.y
	dribbles['type'] = ['Carry' for _ in range(len(dribbles))]
	dribbles['outcomeType'] = ['Successful' for _ in range(len(dribbles))]
	dribbles['qualifiers']=[{} for _ in range(len(dribbles))]
	game_df=pd.concat([game_df,dribbles], ignore_index=True,sort=False)
	game_df=game_df.sort_values(['matchId','period','eventId']).reset_index(drop=True)
	game_df['eventId']=range(len(game_df))
	#add EPV
	#game_df_1=main.addEpvToDataFrame(game_df)
	#Add xT
	xT_grid = pd.read_csv("https://raw.githubusercontent.com/mckayjohns/xT/main/xT_Grid.csv",header=None)
	xT=np.array(xT_grid)
	xT_rows, xT_cols = xT.shape

	game_df['x1_bin'] = pd.cut(game_df['x'], bins=xT_cols, labels=False)
	game_df['y1_bin'] = pd.cut(game_df['y'], bins=xT_rows, labels=False)
	game_df['x2_bin'] = pd.cut(game_df['endX'], bins=xT_cols, labels=False)
	game_df['y2_bin'] = pd.cut(game_df['endY'], bins=xT_rows, labels=False)

	game_df['start_zone_value'] = np.nan
	game_df['end_zone_value'] = np.nan
	game_df['xT'] = np.nan

	for i in range(game_df.shape[0]):
		if (game_df.loc[i, 'outcomeType'] == "Successful") & (game_df.loc[i, 'type'] == 'Pass') or (game_df.loc[i, 'type'] == 'Carry') :

			# Values to match at xT
			x1, y1 = int(game_df.loc[i, "x1_bin"]), int(game_df.loc[i, "y1_bin"])
			x2, y2 = int(game_df.loc[i, "x2_bin"]), int(game_df.loc[i, "y2_bin"])

			# matching values
			game_df.loc[i, "start_zone_value"] = xT[y1, x1]
			game_df.loc[i, "end_zone_value"] = xT[y2, x2]
			game_df.loc[i, "xT"] = game_df.loc[i, "end_zone_value"] - game_df.loc[i, "start_zone_value"]
		else:
			pass
	game_df=game_df.drop(['x1_bin','x2_bin','y1_bin','y2_bin'],axis=1)
	return (game_df)


def get_match_data(matchurl):
	driver = webdriver.Chrome(ChromeDriverManager().install())
	url = matchurl
	min_dribble_length: float = 0.0
	max_dribble_length: float = 100.0
	max_dribble_duration: float = 20.0
	# Games data
	game = getMatchData(driver, url, close_window=True)

	# Events dataframe
	game_df = createEventsDF(game)
	game_df['time_seconds']=game_df['minute']*60+game_df['second']
	next_actions = game_df.shift(-1)
	same_team = game_df.teamId == next_actions.teamId
	dx = game_df.endX - next_actions.x
	dy = game_df.endY- next_actions.y
	far_enough=dy** 2 + dy** 2 >= min_dribble_length** 2
	not_too_far = dx ** 2+ dy** 2<= max_dribble_length ** 2
	dt = next_actions.time_seconds - game_df.time_seconds
	same_phase = dt < max_dribble_duration
	same_period = game_df.period == next_actions.period
	dribble_idx = same_team & far_enough & not_too_far & same_phase & same_period
	dribbles = pd.DataFrame()
	prev = game_df[dribble_idx]
	nex= next_actions[dribble_idx]
	dribbles['matchId' ] = nex.matchId
	dribbles['period' ] = nex.period
	for cols in ['expandedMinute']:
		dribbles[cols] = nex[cols]
	for cols in ['passKey','assist']:
		dribbles[ cols] = [True for _ in range (len (dribbles))]
	dribbles['isTouch'] = [True for _ in range (len (dribbles))]
	morecols = ['playerId', 'team_name','playerName','h_a']
	for cols in morecols:
		dribbles[cols] = nex[cols]
	dribbles['eventId'] = prev.eventId + 0.1
	dribbles['time_seconds' ] = (prev.time_seconds + nex.time_seconds) / 2
	dribbles['teamId'] = nex.teamId
	dribbles['x'] = prev.endX
	dribbles['y']= prev.endY
	dribbles['endX'] = nex.x
	dribbles['endY'] = nex.y
	dribbles['type'] = ['Carry' for _ in range(len(dribbles))]
	dribbles['outcomeType'] = ['Successful' for _ in range(len(dribbles))]
	dribbles['qualifiers']=[{} for _ in range(len(dribbles))]
	game_df=pd.concat([game_df,dribbles], ignore_index=True,sort=False)
	game_df=game_df.sort_values(['matchId','period','eventId']).reset_index(drop=True)
	game_df['eventId']=range(len(game_df))
	#add EPV
	#game_df_1=main.addEpvToDataFrame(game_df)
	#Add xT
	xT_grid = pd.read_csv("https://raw.githubusercontent.com/mckayjohns/xT/main/xT_Grid.csv",header=None)
	xT=np.array(xT_grid)
	xT_rows, xT_cols = xT.shape

	game_df['x1_bin'] = pd.cut(game_df['x'], bins=xT_cols, labels=False)
	game_df['y1_bin'] = pd.cut(game_df['y'], bins=xT_rows, labels=False)
	game_df['x2_bin'] = pd.cut(game_df['endX'], bins=xT_cols, labels=False)
	game_df['y2_bin'] = pd.cut(game_df['endY'], bins=xT_rows, labels=False)

	game_df['start_zone_value'] = np.nan
	game_df['end_zone_value'] = np.nan
	game_df['xT'] = np.nan

	for i in range(game_df.shape[0]):
		if (game_df.loc[i, 'outcomeType'] == "Successful") & (game_df.loc[i, 'type'] == 'Pass') or (game_df.loc[i, 'type'] == 'Carry') :

			# Values to match at xT
			x1, y1 = int(game_df.loc[i, "x1_bin"]), int(game_df.loc[i, "y1_bin"])
			x2, y2 = int(game_df.loc[i, "x2_bin"]), int(game_df.loc[i, "y2_bin"])

			# matching values
			game_df.loc[i, "start_zone_value"] = xT[y1, x1]
			game_df.loc[i, "end_zone_value"] = xT[y2, x2]
			game_df.loc[i, "xT"] = game_df.loc[i, "end_zone_value"] - game_df.loc[i, "start_zone_value"]
		else:
			pass
	game_df=game_df.drop(['x1_bin','x2_bin','y1_bin','y2_bin'],axis=1)
	return (game_df)

def add_carries(game_df):
	min_dribble_length: float = 0.0
	max_dribble_length: float = 100.0
	max_dribble_duration: float = 20.0
	#game_df['time_seconds']=game_df['minute']*60+game_df['second']
	next_actions = game_df.shift(-1)
	same_game=game_df.matchId==next_actions.matchId
	same_team = game_df.teamId == next_actions.teamId
	dx = game_df.endX - next_actions.x
	dy = game_df.endY- next_actions.y
	far_enough=dy** 2 + dy** 2 >= min_dribble_length** 2
	not_too_far = dx ** 2+ dy** 2<= max_dribble_length ** 2
	dt = next_actions.time_seconds - game_df.time_seconds
	same_phase = dt < max_dribble_duration
	same_period = game_df.period == next_actions.period
	dribble_idx = same_team & far_enough & not_too_far & same_phase & same_period&same_game
	dribbles = pd.DataFrame()
	prev = game_df[dribble_idx]
	nex= next_actions[dribble_idx]
	dribbles['matchId' ] = nex.matchId
	dribbles['period' ] = nex.period
	for cols in ['expandedMinute']:
		dribbles[cols] = nex[cols]
	for cols in ['passKey','assist']:
		dribbles[ cols] = [True for _ in range (len (dribbles))]
	dribbles['isTouch'] = [True for _ in range (len (dribbles))]
	morecols = ['playerId', 'team_name','playerName','h_a']
	for cols in morecols:
		dribbles[cols] = nex[cols]
	dribbles['action_id'] = prev.eventId + 0.1
	dribbles['time_seconds' ] = (prev.time_seconds + nex.time_seconds) / 2
	dribbles['teamId'] = nex.teamId
	dribbles['playerId']= nex.playerId
	dribbles['playerName' ]= nex.playerName
	dribbles['x'] = prev.endX
	dribbles['y']= prev.endY
	dribbles['endX'] = nex.x
	dribbles['endY'] = nex.y
	dribbles['type'] = ['Carry' for _ in range(len(dribbles))]
	dribbles['outcomeType'] = ['Successful' for _ in range(len(dribbles))]
	dribbles['qualifiers']=[{} for _ in range(len(dribbles))]
	game_df=pd.concat([game_df,dribbles], ignore_index=True,sort=False)
	game_df=game_df.sort_values(['matchId','period','action_id']).reset_index(drop=True)
	game_df['action_id']=range(len(game_df))
	return game_df

def get_matches_data(comp,year):
	league_urls = getLeagueUrls()

	league_urls['Africa Cup of Nations']="https://www.whoscored.com/Regions/247/Tournaments/104/International-Africa-Cup-of-Nations"

	match_urls = getMatchUrls(comp_urls=league_urls,competition=comp,season=year)

	matches = []
	for i in range(0,len(match_urls),10):
		game_data=getMatchesData(match_urls[i:i+10])
		matches.append(game_data)
		if i % 10==0:
		 time.sleep(20)
	matches_all=sum(matches,[])
	events_ls = [createEventsDF(match) for match in matches_all]
	all_events=pd.concat(events_ls)
	return all_events

def add_XT(game_df):
	xT_grid = pd.read_csv("https://raw.githubusercontent.com/mckayjohns/xT/main/xT_Grid.csv",header=None)
	xT=np.array(xT_grid)
	xT_rows, xT_cols = xT.shape

	game_df['x1_bin'] = pd.cut(game_df['x'], bins=xT_cols, labels=False)
	game_df['y1_bin'] = pd.cut(game_df['y'], bins=xT_rows, labels=False)
	game_df['x2_bin'] = pd.cut(game_df['endX'], bins=xT_cols, labels=False)
	game_df['y2_bin'] = pd.cut(game_df['endY'], bins=xT_rows, labels=False)

	game_df['start_zone_value'] = np.nan
	game_df['end_zone_value'] = np.nan
	game_df['xT'] = np.nan

	for i in game_df.index:
		if (game_df.loc[i, 'outcomeType'] == "Successful") & (game_df.loc[i, 'type'] == 'Pass') or (game_df.loc[i, 'type'] == 'Carry') :

			# Values to match at xT
			x1, y1 = int(game_df.loc[i, "x1_bin"]), int(game_df.loc[i, "y1_bin"])
			x2, y2 = int(game_df.loc[i, "x2_bin"]), int(game_df.loc[i, "y2_bin"])

			# matching values
			game_df.loc[i, "start_zone_value"] = xT[y1, x1]
			game_df.loc[i, "end_zone_value"] = xT[y2, x2]
			game_df.loc[i, "xT"] = game_df.loc[i, "end_zone_value"] - game_df.loc[i, "start_zone_value"]
		else:
			pass
	game_df=game_df.drop(['x1_bin','x2_bin','y1_bin','y2_bin'],axis=1)
	return (game_df)

def add_carries_xT(all_events):
	all_events['time_seconds'] = all_events['minute'] * 60 + all_events['second']
	all_events=all_events.groupby(['matchId'],as_index=False).apply(add_carries)
	all_events = add_XT(all_events)
	return all_events

def add_carries_xT(all_events):
	all_events['time_seconds'] = all_events['minute'] * 60 + all_events['second']
	all_events=all_events.groupby(['matchId'],as_index=False,sort=False)
	all_events = all_events.apply(lambda x:x.sort_values(['matchId', 'period', 'time_seconds']))
	all_events = add_carries(all_events)
	all_events = add_XT(all_events)
	return all_events

def game_carries_xT(matchurl):
	driver = webdriver.Chrome(ChromeDriverManager().install())
	url = matchurl
	# Games data
	game = getMatchData(driver, url, close_window=True)
	# Events dataframe
	game_df = createEventsDF(game)
	game_df['time_seconds'] = game_df['minute'] * 60 + game_df['second']
	game_df['action_id'] = range(len(game_df))
	game_df=game_df.sort_values(['matchId','period','time_seconds'])
	game_df=add_carries(game_df)
	game_df = add_XT(game_df)
	return game_df

def player_minutes(df):
	gamedf = df[(df.period=="FirstHalf")|(df.period=="SecondHalf")|(df.period=="FirstPeriodOfExtraTime")|(df.period=="SecondPeriodOfExtraTime")]
	gamedf['expandedMinute']=np.where((gamedf['expandedMinute']>45)&(gamedf['period']=="FirstHalf"),45,gamedf['expandedMinute'])
	gamedf['expandedMinute'] = np.where((gamedf['expandedMinute'] > 90) & (gamedf['period'] == "SecondHalf"), 90,gamedf['expandedMinute'])
	gamedf['expandedMinute'] = np.where((gamedf['expandedMinute'] > 105) & (gamedf['period'] == "FirstPeriodOfExtraTime"), 105,gamedf['expandedMinute'])
	gamedf['expandedMinute'] = np.where((gamedf['expandedMinute'] > 120) & (gamedf['period'] == "SecondPeriodOfExtraTime"),120, gamedf['expandedMinute'])
	max_min = gamedf.expandedMinute.max()
	#gamedf['qualifiers']= gamedf.qualifiers.apply(lambda x:ast.literal_eval(x))
	gamedf['playerName']=gamedf['playerName'].fillna(value='')
	gamedf['playerId'] = gamedf['playerId'].fillna(value='')
	gamedf['action_id']=range(len(gamedf))
	#gamedf['redcard']=gamedf.qualifiers.apply(lambda x:int(33 in x or 32 in x))
	gamedf['redcard'] = np.where((gamedf['redCard'] ==True) | (gamedf['secondYellow'] == True), 1,0)
	playeroffs = gamedf[(gamedf.type=='SubstitutionOff')|(gamedf.redcard==1)].playerName.tolist()
	playerons= gamedf[(gamedf.type=="SubstitutionOn")].playerName.tolist()
	timeoffs = gamedf[(gamedf.type=='SubstitutionOff')|(gamedf.redcard==1)].expandedMinute.tolist()
	timeons= gamedf[(gamedf.type=="SubstitutionOn")].expandedMinute.tolist()
	allplayers=gamedf[gamedf.playerName !=''].playerName.unique()
	allplayers_id = gamedf[gamedf.playerId!=''].playerId.unique()
	time_played=[]
	for i, pl in enumerate(allplayers):
		if((pl in playerons) & (pl not in playeroffs)):
			time_played.append(max_min-timeons[playerons.index(pl)])
		elif((pl in playerons) & (pl in playeroffs)):
			time_played.append(timeoffs[playeroffs.index(pl)] - timeons[playerons.index(pl)])
		elif(pl in playeroffs):
			time_played.append(timeoffs[playeroffs.index(pl)])
		else:
			time_played.append(max_min)
	timedf=pd.DataFrame()
	timedf['playerName']=allplayers
	timedf['playerId'] = allplayers_id
	timedf['time']=time_played
	gamedf=gamedf[['playerId','team_name']]
	gamedf.drop_duplicates(subset=['playerId'],inplace=True)
	timedf=pd.merge(timedf,gamedf,on="playerId")
	return timedf

def all_player_minutes(df):
	df['playerName']=np.where((df['playerName']=="Luis Suárez")&(df['playerId']=="22221"),"Luis Alberto Suárez",df['playerName'])
	df=df.groupby(['matchId'],as_index=False).apply(player_minutes)
	return(df)


#events = get_match_data("https://www.whoscored.com/Matches/1549794/Live/England-Premier-League-2021-2022-Liverpool-Norwich")
#events=events[events["type"]!="Carry"]

#game_minutes = player_minutes(events)
#driver = webdriver.Chrome(ChromeDriverManager().install())

#events = get_match_data("https://www.whoscored.com/Matches/1549771/Live/England-Premier-League-2021-2022-Burnley-Manchester-United")

'''
events = get_match_data("https://www.whoscored.com/Matches/1549771/Live/England-Premier-League-2021-2022-Burnley-Manchester-United")
xT_grid = pd.read_csv("https://raw.githubusercontent.com/mckayjohns/xT/main/xT_Grid.csv",header=None)
xtd=np.array(xT_grid)
events['x']=events['x']*1.05
events['endX']=events['endX']*1.05
events['y']=events['y']*0.68
events['endY']=events['endY']*0.68

from scipy.interpolate import RegularGridInterpolator
x = np.linspace(0,105,12)
y = np.linspace(0,68,8)
f = RegularGridInterpolator((y, x), xtd)
def binnings(Df,f):
    Df['start_zone'] = Df[['x', 'y']].apply(lambda x: f([x[1],x[0]])[0], axis=1)
    Df['end_zone'] = Df[['endX', 'endY']].apply(lambda x: f([x[1],x[0]])[0], axis=1)
    Df['xt_value'] = Df['end_zone'] - Df['start_zone']
    return Df
check = binnings(events,f)

#league_urls = getLeagueUrls()

df=pd.read_csv("epl_22_all_events.csv")
df=df.iloc[:,1:]
#df['matchId'].describe()
df=df.groupby(['matchId'],as_index=False).apply(add_carries)
df=df.groupby(['matchId']).apply(add_XT)

df.groupby(['playerName'])['xT'].agg(sum).sort_values(ascending=False)

df=df.groupby(['matchId']).apply(add_carries)
df=add_XT(df)

df=df.reset_index()
df_1=df.apply(add_XT)

df.info()
	#all_events = all_events.groupby(['matchId']).apply(add_XT)

#test=get_matches_data("Premier League","2021/2022")
#df=test.groupby('matchId').apply(add_carries)
#test.drop('time_seconds',axis=1).to_csv('epl_22_all_events.csv')

'''