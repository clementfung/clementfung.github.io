import pandas as pd
import numpy as np
import pdb

filename = "2018-19-nbaprojections.csv"
columns = ['Name', 'Team', 'POS', 'Gs', 'MPG', 'FG%', 'FT%', 'PTS', '3PM', 'REB', 'AST', 'STL', 'BLK', 'TOV']
z_columns = ['Name', 'Team', 'POS', 'Gs', 'MPG', 'ZFG%', 'ZFT%', 'ZPTS', 'Z3PM', 'ZREB', 'ZAST', 'ZSTL', 'ZBLK', 'ZTOV', 'ZAVG', 'ZPUNT']
nine_cats = ['ZFG%', 'ZFT%', 'ZPTS', 'Z3PM', 'ZREB', 'ZAST', 'ZSTL', 'ZBLK', 'ZTOV']

# If you are punting a category, put the associated Z score HERE
punt_cats = ['ZPTS', 'ZFT%']

verb = True

def read_and_parse():

	df = pd.read_csv(filename)
	df = df[columns] # Get the 9 cat, and some extra info
	df = df.ix[0:250] # Only the top 250 players

	# For some reason a bunch of the 3PM are '-', change to zero.
	for i in range(251):
		
		if df.ix[i, 8] == '-':
			df.ix[i, 8] = 0	
			if verb:
				print df.ix[i, 0] + " setting 3s to 0."

		# Clean the '%' from the percentage columns
		df.ix[i, 5] = str(df.ix[i, 5])[0:-1]
		df.ix[i, 6] = str(df.ix[i, 6])[0:-1]

	return df

def zscore(df):

	# Make a copy that holds the z scores
	zdf = pd.DataFrame(np.zeros((251, 16)), columns=z_columns)
	zdf.ix[:, 0:5] = df.ix[:, 0:5]

	# compute for all 9 categories
	for col in range(5, 14):
		
		values = df.ix[:, col].values.astype(float)

		if verb:
			print "Average " + columns[col] + " is " + str(np.mean(values))

		# Reverse the z score for turnovers
		if columns[col] == "TOV":
			zdf.ix[:, col] = np.round((np.mean(values) - values) / np.std(values), decimals=3)
		else:
			zdf.ix[:, col] = np.round((values - np.mean(values)) / np.std(values), decimals=3)

	zdf['ZAVG'] = np.round(np.mean(zdf[nine_cats], axis=1), decimals=3)

	# Add for a punt
	puntdf = zdf[nine_cats].drop(punt_cats, axis=1)
	zdf['ZPUNT'] = np.round(np.mean(puntdf, axis=1), decimals=3)

	return zdf

def main():

	df = read_and_parse()
	zdf = zscore(df)
	
	zdf = zdf.sort_values('ZAVG', ascending=False)
	zdf.to_csv("zdf.csv")

	zdf = zdf.sort_values('ZPUNT', ascending=False)
	zdf.to_csv("punt_zdf.csv")

	print "Done!"

if __name__ == '__main__':
	main()