'''
Gets Congressional speech text, arranged by speaker.

Produces a csv (capitolwords.csv) with the following columns:
speaker_state,speaker_raw,speaker_first,congress,title,origin_url,number,id,volume,chamber,session,speaker_last,
pages,speaker_party,date,bills,bioguide_id,order,speaking,capitolwords_url

Uses the Sunlight foundation library: http://python-sunlight.readthedocs.org/en/latest/
'''

import sys
import csv
csv.field_size_limit(sys.maxint)
import unicodedata

import sunlight
from datetime import datetime

from sunlight.services.capitolwords import CapitolWords

sunlight.config.API_KEY = 'enter_key'


def remove_accentuated(s):
    """Removes accentuated chars and lower case
    """
    s = ''.join(c for c in unicodedata.normalize('NFD', s.lower()) if unicodedata.category(c) != 'Mn')
    return s


def csv_rowcount(outfile):
    try:
        with open('capitolwords.csv') as f:
            reader = csv.reader(f)
            for i, r in enumerate(reader):
                pass
            return i - 1
    except:
        return 0

if __name__ == "__main__":
    if len(sys.argv) > 2:
        outfile = sys.argv[1]
    else:
        outfile = "capitolwords.csv"

    cw = CapitolWords()
    start_date = '1900-01-01'
    #end_date = datetime.today().strftime('%Y-%m-%d')
    sort = "date asc"
    results = cw.text(phrase=" ", start_date=start_date, sort=sort)
    if len(results) > 1:
        headers = results[0].keys()
    else:
        print("Not found")
        sys.exit()

    nrow = csv_rowcount(outfile)
    print nrow
    if nrow > 0:
        start = nrow
        f = open(outfile, "ab")
        writer = csv.DictWriter(f, fieldnames=headers)
    else:
        start = 0
        f = open(outfile, "wb")
        writer = csv.DictWriter(f, fieldnames=headers)
        writer.writeheader()
    page = int(start / 50)
    first_start = start - (page * 50)
    while True:
        print page*50
        results = cw.text(phrase=" ", start_date=start_date, sort=sort, page=page)
        for i, r in enumerate(results):
            if i < first_start:
                #print "skip i: %d" % i
                continue
            else:
                first_start = 0
            #print("process i: %d" % i)
            r['speaking'] = '\n'.join(r['speaking'])
            if r['speaker_last']:
                r['speaker_last'] = r['speaker_last'].encode('utf-8')
            if r['speaker_first']:
                r['speaker_first'] = r['speaker_first'].encode('utf-8')
            try:
                writer.writerow(r)
            except:
                print r
                raise
        if len(results) < 50:
            break
        page += 1
    f.close()