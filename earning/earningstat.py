import numpy as np
import pandas as pd

from datetime import datetime, timedelta
from pandas import ExcelWriter
from itertools import product
import os
import sys
module_path = os.path.join('..')
sys.path.append(module_path)

from selenium import webdriver
browser = webdriver.Chrome(executable_path=os.path.join('D:\\', 'chromedriver.exe'))

def get_chip_earningdict(symbol, start_year=2018):
    """Return a dictionary of earning dates by each quarter."""
    earning_dict = {'symbol': symbol}
    # Open the website link
    try:
        link = 'https://www.streetinsider.com/dr/eps-ticker.php?q=%s' % (symbol)
        browser.get(link)
        # Setup the earning result dataframe
        event_df = pd.read_html(browser.page_source)[1]
        text_earningtime = browser.find_elements_by_xpath('/html/body/table[1]/tbody/tr[6]/td')[0].text
        event_df = event_df.set_index('Date')[['EPS', 'Surprise']]
        event_df = event_df.filter(regex='\d+/\d+/\d{2}', axis='index')
        event_df.index = pd.to_datetime(event_df.index)
        event_df.index = [(stamp + timedelta(hours=32)) if text_earningtime == 'After Close'
                          else (stamp + timedelta(hours=18)) for stamp in event_df.index]
        event_df.sort_index(ascending=True, inplace=True)
        # Find the next earning date
        next_earningdate = browser.find_element_by_xpath('//table[@class="etable summary"]').text.split('\n')[0]
        if '*Est' not in next_earningdate:
            pass
        else:
            next_earningdate = next_earningdate.replace('Next EPS Date ', '').replace(' *Est.', '')
            if text_earningtime == 'After Close':
                next_earningdate = pd.to_datetime(next_earningdate) + timedelta(hours=32)
            else:
                next_earningdate = pd.to_datetime(next_earningdate) + timedelta(hours=18)
            event_df = event_df.append(pd.Series({'EPS': '—', 'Surprise': '—'}, name=next_earningdate))
            # Amend the earning date dictioary
        for stamp in event_df.index[event_df.index >= datetime(start_year, 1, 1)]:
            datestr = stamp.strftime('%Y-%m-%d, %H:%M')
            try:
                qnum = (int(datestr[5:7]) - 1) // 3 + 1
                ynum = datestr[:4]
                earning_dict[str(ynum) + 'q' + str(qnum)] = datestr
            except Exception as ex:
                pass
    except Exception as ex:
        template = "An exception of type {0} occurred. Arguments:\n{1!r}"
        print(template.format(type(ex).__name__, ex.args), '\n')
        print('Cannot obtain the outputs for symbol %s.' % (symbol))

    return earning_dict, event_df


def get_table_calendar(symbollist, start_year=2018, end_year=2020, sort_index=True):
    """Obtain a table with all available past and projected earning dates of all component stocks."""
    list_year = [str(year) for year in range(start_year, end_year + 1)]
    list_season = ['q1', 'q2', 'q3', 'q4']
    list_quarterkey = [elem[0] + elem[1] for elem in product(list_year, list_season)]
    table_calendar = pd.DataFrame(columns=['symbol'] + list_quarterkey)

    for symbol in symbollist:
        try:
            earning_dict = get_chip_earningdict(symbol)[0]
            table_calendar = table_calendar.append(earning_dict, ignore_index=True)
            print('Successful appending symbol %s.' % (symbol))
        except Exception as ex:
            print('Fail to append symbol %s.' % (symbol))
    table_calendar = table_calendar.set_index('symbol')

    if sort_index:
        table_calendar = table_calendar.sort_index()
    else:
        today = datetime.today().strftime('%Y-%m-%d')
        qnum = (int(today[5:7]) - 1) // 3 + 1
        ynum = today[:4]
        table_calendar.sort_values(by=[str(ynum) + 'q' + str(qnum)], inplace=True)

    return table_calendar

def caltable_to_excel(calendar_table, name):
    calendar_path = os.path.join('D:\\', 'Trading', 'Systems', 'Monitor_Events', 'Earning')
    earning_file = os.path.join(calendar_path, 'earning_' + name + '_' + datetime.today().strftime('%Y-%m-%d') + '.xlsx')
    master_writer = ExcelWriter(earning_file, engine='xlsxwriter', datetime_format='yyyy/mm/dd', date_format='yyyy/mm/dd')
    calendar_table.to_excel(master_writer, sheet_name='calendar')
    sheet_calendar = master_writer.sheets['calendar']
    sheet_calendar.set_column('A:A', 8)
    sheet_calendar.set_column('B:Z', 16)
    sheet_calendar.freeze_panes(1, 0)
    master_writer.save()
    master_writer.close()

if __name__ == '__main__':
    df_largecap = pd.read_csv(symbollist_largecap.csv, header=0, index_col='symbol')
    symbollist_largecap = list(df_largecap.index)
    table_calendar = get_table_calendar(symbollist_largecap)
    caltable_to_excel(table_calendar, 'largecap')