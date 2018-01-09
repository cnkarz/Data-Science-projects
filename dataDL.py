import urllib as UR
import os
import xml.etree.ElementTree as ET
import sys

def downloadHistoricalData(my_url) :
	urlObject = UR.urlopen(my_url)
	urlObject = urlObject.read()
	return urlObject
	
def xmlParser(my_xml) :
	tree = ET.fromstring(my_xml)
	days = tree.iter("G_NEW_DATE")
	rates = tree.iter("G_TC_5YEAR")

	
	for day in days:
		date = day.find("TIPS_CURVE_DATE")
		print date.text
		rate = day.find("LIST_G_TC_5YEAR").find("G_TC_5YEAR")
		for each in rate:
			print each.tag, each.text


		
url_realYieldCurve = "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Datasets/real_yield.xml"

my_file = downloadHistoricalData(url_realYieldCurve)

xmlParser(my_file)

sys.stdout.flush()
