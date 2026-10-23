#!/usr/bin/env python3
"""Regenerate committed data from hash-verified archives; needs Python 3.12+ and R (xlrd for --package datasets-uci); source definitions are pinned to R 4.3.3.
Pass an archive directory (offline), or --fetch to download sources into it first.
Writes TARGET/registry.rktd and TARGET/data/<id>.rktd.gz for one package.
"""
import argparse, csv, gzip, hashlib, io, json, re, subprocess, sys, tarfile, tempfile, urllib.request, zipfile
import xml.etree.ElementTree as ET
from pathlib import Path
ROOT = Path(__file__).resolve().parents[1]
DEFAULTS = {'datasets-core': ('datasets-core/datasets/core-data', 'datasets-core/datasets/provenance'),
            'datasets-r': ('datasets-r/datasets/r-data', 'datasets-r/datasets/r-data/provenance'),
            'datasets-uci': ('datasets-uci/datasets/uci-data', 'datasets-uci/datasets/uci-data/provenance')}
cli = argparse.ArgumentParser(description=__doc__)
cli.add_argument('--fetch', action='store_true')
cli.add_argument('--package', default='datasets-core')
cli.add_argument('--target', type=Path)
cli.add_argument('--provenance', type=Path)
cli.add_argument('archive_dir', type=Path)
args = cli.parse_args()
if args.package not in DEFAULTS: sys.exit(f'No import recipes for package {args.package}')
PACKAGE = args.package
OUT = args.target or ROOT/DEFAULTS[PACKAGE][0]
PROVENANCE = args.provenance or ROOT/DEFAULTS[PACKAGE][1]
(OUT/'data').mkdir(parents=True,exist_ok=True); PROVENANCE.mkdir(parents=True,exist_ok=True)
SOURCES = {k:s for k,s in json.loads((ROOT/'scripts/sources.json').read_text()).items() if s.get('package','datasets-core')==PACKAGE}
fetch = args.fetch
cache = args.archive_dir; cache.mkdir(parents=True,exist_ok=True)
for s in SOURCES.values():
    p=cache/s['file']
    if fetch and not p.exists(): urllib.request.urlretrieve(s['url'],p)
    if hashlib.sha256(p.read_bytes()).hexdigest()!=s['sha256']: raise ValueError('Source checksum mismatch: '+str(p))
def sexp(x):
    if x is None: return 'missing'
    if isinstance(x,bool): return '#t' if x else '#f'
    if isinstance(x,str): return json.dumps(x,ensure_ascii=False)
    if isinstance(x,dict): return '#hash('+ ' '.join('('+sexp(k)+' . '+sexp(v)+')' for k,v in x.items())+')'
    if isinstance(x,(list,tuple)): return '('+' '.join(map(sexp,x))+')'
    return str(x)
def number(s):
    if s=='NA': return None
    try: return int(s)
    except ValueError:
        try: return float(s)
        except ValueError: return s
registry={}
def add(id,original,rows,source,levels={},units={},descriptions={},identifiers=[],targets=[],notes='',series=None,names=None):
    names=names or [re.sub('[^a-z0-9]+','-',s.lower()).strip('-') for s in original]
    aliases={'freq':'frequency','len':'length','supp':'supplement','solar-r':'solar-r','urbanpop':'urban-pop','sepal-length':'sepal-length'}
    names=[aliases.get(n,n) for n in names]
    if id=='diabetes': names=[n.removeprefix('x-') for n in names]
    columns=[]
    for j,(name,orig) in enumerate(zip(names,original)):
        values=[r[j] for r in rows if r[j] is not None]
        kind='identifier' if name in identifiers else 'categorical' if name in levels else 'integer' if all(isinstance(v,int) for v in values) else 'real' if all(isinstance(v,(int,float)) for v in values) else 'string'
        columns.append({'name':name,'original-name':orig,'type':kind,'levels':levels.get(name,[]),'unit':units.get(name,False),'description':descriptions.get(name,orig),'missing-count':sum(r[j] is None for r in rows)})
    text='(\n'+'\n'.join('  '+sexp(r) for r in rows)+'\n)\n'
    (OUT/'data'/f'{id}.rktd.gz').write_bytes(gzip.compress(text.encode(),9,mtime=0))
    s=SOURCES[source]
    registry[id]={'name':id,'rows':len(rows),'column-count':len(names),'columns':columns,'identifiers':identifiers,'suggested-targets':targets,'source-url':s['url'],'source-version':s.get('version') or ('R 4.3.3' if source=='R' else 'lars 1.3' if source=='lars' else 'UCI snapshot 2026-09-26 (hash pinned)'),'source-sha256':s['sha256'],'sha256':hashlib.sha256(text.encode()).hexdigest(),'license':s.get('license') or ('GPL-2.0-only OR GPL-3.0-only' if source=='R' else 'GPL-2.0-only' if source=='lars' else 'CC-BY-4.0'),'citation':citations.get(id,'R Core Team (2024). R: A Language and Environment for Statistical Computing. See bundled upstream documentation for original references.'),'notes':notes or 'Source row order and values retained; names normalized to kebab-case.','time-series':series or False,'tier':'bundled','package':PACKAGE,'fetch':False}
citations={'iris':'Fisher, R. (1936). Iris. UCI. doi:10.24432/C56C76','wine':'Aeberhard, S. and Forina, M. (1991). Wine. UCI. doi:10.24432/C5PC7J','breast-cancer':'Wolberg, W., Mangasarian, O., Street, N., and Street, W. (1993). Breast Cancer Wisconsin (Diagnostic). UCI. doi:10.24432/C5DW2B','diabetes':'Efron, Hastie, Johnstone and Tibshirani (2004). Least Angle Regression. Annals of Statistics 32(2), 407-499. doi:10.1214/009053604000000067'}
PENGUIN_SOURCE='Gorman, K. B., Williams, T. D. and Fraser, W. R. (2014). Ecological sexual dimorphism and environmental variability within a community of Antarctic penguins (genus Pygoscelis). PLoS ONE 9(3), e90081. doi:10.1371/journal.pone.0090081'
citations.update({'penguins':'Horst, A. M., Hill, A. P. and Gorman, K. B. (2020). palmerpenguins: Palmer Archipelago (Antarctica) penguin data. doi:10.5281/zenodo.3960218. Data: '+PENGUIN_SOURCE,
                  'gapminder':'Bryan, J. (2025). gapminder: Data from Gapminder. R package version 1.0.1. Data: Gapminder Foundation, https://www.gapminder.org/data/',
                  'datasaurus-dozen':'Gillespie, C., Locke, S., Davies, R. and D\'Agostino McGowan, L. (2025). datasauRus: Datasets from the Datasaurus Dozen. R package version 0.1.9. Matejka, J. and Fitzmaurice, G. (2017). Same Stats, Different Graphs. CHI 2017. doi:10.1145/3025453.3025912',
                  'titanic-passengers':'Hendricks, P. (2015). titanic: Titanic Passenger Survival Data Set. R package version 0.1.0. Data: Kaggle, Titanic: Machine Learning from Disaster, https://www.kaggle.com/c/titanic/data'})
citations['penguins-raw']=citations['penguins']
PENGUIN_UNITS={'bill-length-mm':'mm','bill-depth-mm':'mm','culmen-length-mm':'mm','culmen-depth-mm':'mm','flipper-length-mm':'mm','body-mass-g':'g','delta-15-n':'per mil (o/oo)','delta-13-c':'per mil (o/oo)'}
R_RECIPES=[
    ('penguins','palmerpenguins',dict(units=PENGUIN_UNITS,targets=['species'],
        descriptions={'species':'Penguin species','island':'Island in the Palmer Archipelago, Antarctica','bill-length-mm':'Bill (culmen) length','bill-depth-mm':'Bill (culmen) depth','flipper-length-mm':'Flipper length','body-mass-g':'Body mass','sex':'Sex','year':'Study year'},
        notes='Adult Adélie, Chinstrap and Gentoo penguins near Palmer Station, Antarctica, 2007–2009. Missing measurements and sexes retained as missing. R factors rendered as labels with levels in metadata; names normalized to kebab-case.')),
    ('penguins-raw','palmerpenguins',dict(units=PENGUIN_UNITS,targets=['species'],aliases={'delta-15-n-o-oo':'delta-15-n','delta-13-c-o-oo':'delta-13-c'},
        levels={'study-name':['PAL0708','PAL0809','PAL0910'],'species':['Adelie Penguin (Pygoscelis adeliae)','Chinstrap penguin (Pygoscelis antarctica)','Gentoo penguin (Pygoscelis papua)'],'region':['Anvers'],'island':['Biscoe','Dream','Torgersen'],'stage':['Adult, 1 Egg Stage'],'clutch-completion':['No','Yes'],'sex':['FEMALE','MALE']},
        descriptions={'study-name':'Sampling expedition','sample-number':'Sample number within the expedition','individual-id':'Individual penguin identifier','clutch-completion':'Whether the study nest was observed with a full clutch of two eggs','date-egg':'Date the study nest was observed with one egg (ISO 8601)','delta-15-n':'Ratio of stable nitrogen isotopes 15N:14N','delta-13-c':'Ratio of stable carbon isotopes 13C:12C'},
        notes='Unprocessed Palmer Station LTER records behind penguins. Character columns with few values declared categorical with sorted levels; Date Egg rendered as an ISO 8601 string; the delta columns renamed delta-15-n and delta-13-c (original names recorded). Missing values retained.')),
    ('gapminder','gapminder',dict(units={'life-exp':'years','pop':'people','gdp-percap':'US$, inflation-adjusted'},targets=['life-exp'],
        descriptions={'country':'Country','continent':'Continent','year':'Year, 1952–2007 in steps of five','life-exp':'Life expectancy at birth','pop':'Population','gdp-percap':'GDP per capita'},
        notes='Excerpt of the Gapminder data: 142 countries every five years from 1952 to 2007, a balanced country-by-year panel. Country and continent factors rendered as labels with levels in metadata.')),
    ('datasaurus-dozen','datasauRus',dict(levels={'dataset':['away','bullseye','circle','dino','dots','h_lines','high_lines','slant_down','slant_up','star','v_lines','wide_lines','x_shape']},
        descriptions={'dataset':'Which of the 13 datasets the point belongs to','x':'x coordinate','y':'y coordinate'},
        notes='Long form: the 13 Datasaurus Dozen datasets stacked, 142 points each, told apart by the dataset column. They share near-identical means, standard deviations and correlations but look nothing alike when plotted.')),
    ('titanic-passengers','titanic',dict(identifiers=['passenger-id'],targets=['survived'],levels={'sex':['female','male'],'embarked':['C','Q','S']},units={'age':'years'},
        descriptions={'passenger-id':'Passenger ID','survived':'Survival: 0 = died, 1 = survived','pclass':'Ticket class: 1 = 1st, 2 = 2nd, 3 = 3rd','sib-sp':'Siblings and spouses aboard','parch':'Parents and children aboard','ticket':'Ticket number','fare':'Passenger fare','embarked':'Port of embarkation: C = Cherbourg, Q = Queenstown, S = Southampton'},
        notes='The 891-passenger Kaggle training set (titanic_train). The 418-passenger test set is not included: it has no survival labels. The 177 missing ages are retained as missing; empty cabin (687) and embarked (2) strings are rendered as missing. Ticket stays a string column. Not to be confused with titanic, the 32-cell R contingency table.')),
]
CORE_RECIPES={
    'mtcars':dict(
        identifiers=['model'],
        units={'mpg':'miles/US gallon','disp':'cubic inches','hp':'gross horsepower','wt':'1000 lb','qsec':'seconds'},
        descriptions={'cyl':'Number of cylinders','drat':'Rear axle ratio','vs':'Engine: 0 V-shaped, 1 straight','am':'Transmission: 0 automatic, 1 manual','gear':'Forward gears','carb':'Carburetors'}),
    'us-arrests':dict(
        identifiers=['state'],
        units={'murder':'arrests per 100,000','assault':'arrests per 100,000','urban-pop':'percent','rape':'arrests per 100,000'},
        notes='1973 US arrests. Historical variable names and values retained, including the documented UrbanPop transcription issue; see upstream Rd.'),
    'faithful':dict(
        units={'eruptions':'minutes','waiting':'minutes'},
        descriptions={'eruptions':'Duration of an Old Faithful eruption','waiting':'Waiting time to the next eruption'},
        notes='Original rounded eruption durations retained; see upstream documentation.'),
    'airquality':dict(
        units={'ozone':'ppb','solar-r':'Langleys','wind':'mph','temp':'degrees Fahrenheit'},
        descriptions={'ozone':'Mean ozone, 1300–1500 hours, Roosevelt Island','solar-r':'Solar radiation, 0800–1200 hours, Central Park','wind':'Average wind speed, 0700 and 1000 hours, LaGuardia Airport','temp':'Maximum daily temperature, LaGuardia Airport','month':'Calendar month (5–9)','day':'Day of month'},
        notes='New York, May–September 1973. All 37 Ozone and 7 Solar.R missing observations retained.'),
    'plant-growth':dict(
        levels={'group':['ctrl','trt1','trt2']},
        descriptions={'weight':'Dried plant weight','group':'Control or treatment group'},
        targets=['weight']),
    'tooth-growth':dict(
        levels={'supplement':['OJ','VC']},
        units={'dose':'mg/day'},
        descriptions={'length':'Odontoblast length','supplement':'Orange juice (OJ) or ascorbic acid (VC)'},
        targets=['length']),
    'titanic':dict(
        levels={'class':['1st','2nd','3rd','Crew'],'sex':['Male','Female'],'age':['Child','Adult'],'survived':['No','Yes']},
        notes='R contingency table expanded in R array order, including all eight zero-count cells. frequency counts people; rows are cells, not individuals.'),
    'air-passengers':dict(
        units={'passengers':'thousands'},
        descriptions={'year':'Calendar year','month':'Calendar month, January=1','passengers':'International airline passenger count'},
        series={'frequency':12,'start':[1949,1],'end':[1960,12],'index':['year','month']},
        notes='Monthly international airline passengers, 1949–1960. Calendar columns derived from the R ts index.'),
    'diabetes':dict(
        targets=['response'],
        units={n:'standardized (unit L2 norm)' for n in ['age','sex','bmi','map','tc','ldl','hdl','tch','ltg','glu']},
        descriptions={'age':'Age, standardized','sex':'Sex, standardized source coding','bmi':'Body mass index, standardized','map':'Mean arterial pressure, standardized','tc':'Total cholesterol, standardized','ldl':'Low-density lipoproteins, standardized','hdl':'High-density lipoproteins, standardized','tch':'Total cholesterol / HDL, standardized','ltg':'Log triglycerides, standardized','glu':'Blood glucose, standardized','response':'Quantitative disease progression one year after baseline'},
        notes='lars standardized ten-predictor x matrix and y response; source x. name prefix removed (recorded in original-name); columns centered and scaled to unit Euclidean norm. No raw measurements or exact scikit-learn parity promised.'),
}
def import_core():
    with tempfile.TemporaryDirectory() as tmp:
        tmp=Path(tmp)
        for src in ['R','lars']:
            with tarfile.open(cache/SOURCES[src]['file']) as a: a.extractall(tmp,filter='data')
        subprocess.run(['Rscript',str(ROOT/'scripts/export-r.R'),str(tmp),str(tmp)],check=True)
        docs=PROVENANCE
        rnames={'mtcars':'mtcars','faithful':'faithful','anscombe':'anscombe','airquality':'airquality','us-arrests':'USArrests','plant-growth':'PlantGrowth','tooth-growth':'ToothGrowth','titanic':'Titanic','air-passengers':'AirPassengers'}
        for id,orig in rnames.items(): (docs/f'{id}.Rd').write_bytes((tmp/f'R-4.3.3/src/library/datasets/man/{orig}.Rd').read_bytes())
        (docs/'diabetes.Rd').write_bytes((tmp/'lars/man/diabetes.Rd').read_bytes())
        for p in sorted(tmp.glob('*.csv')):
            with p.open() as f: records=list(csv.reader(f))
            id=p.stem
            kw=CORE_RECIPES.get(id,{})
            add(id,records[0],[[number(v) for v in r] for r in records[1:]],'lars' if id=='diabetes' else 'R',**kw)
    for id,file in [('iris','bezdekIris.data'),('wine','wine.data'),('breast-cancer','wdbc.data')]:
        with zipfile.ZipFile(cache/SOURCES[id]['file']) as a:
            rows=[[number(v) for v in r] for r in csv.reader(io.StringIO(a.read(file).decode())) if r]
            for name in a.namelist():
                if name.endswith('.names'): (PROVENANCE/name).write_bytes(a.read(name))
        if id=='iris':
            add(id,['sepal length','sepal width','petal length','petal width','species'],rows,id,levels={'species':['Iris-setosa','Iris-versicolor','Iris-virginica']},units={n:'cm' for n in ['sepal-length','sepal-width','petal-length','petal-width']},targets=['species'],notes='Corrected UCI bezdekIris.data variant, including corrected observations 35 and 38. Original class label spelling retained. Source class column is named species here.')
            registry[id]['columns'][-1]['original-name']='class'
        elif id=='wine':
            names=['Alcohol','Malic acid','Ash','Alcalinity of ash','Magnesium','Total phenols','Flavanoids','Nonflavanoid phenols','Proanthocyanins','Color intensity','Hue','OD280/OD315 of diluted wines','Proline','cultivar']
            add(id,names,[r[1:]+[str(r[0])] for r in rows],id,levels={'cultivar':['1','2','3']},targets=['cultivar'],notes='Source class moved from first to last column and retained as a string label; measurement order unchanged.')
            registry[id]['columns'][-1]['original-name']='class'
        else:
            measures=['radius','texture','perimeter','area','smoothness','compactness','concavity','concave points','symmetry','fractal dimension']
            names=[m+' '+s for s in ['mean','standard error','worst'] for m in measures]+['diagnosis','id']
            add(id,names,[r[2:]+[r[1],str(r[0])] for r in rows],id,levels={'diagnosis':['M','B']},identifiers=['id'],targets=['diagnosis'],notes='UCI diagnostic dataset. Source ID and M/B diagnosis moved after 30 measurements. Worst means mean of the three largest values; source IDs retained as strings.')
def import_r():
    def kebab(s): return re.sub('[^a-z0-9]+','-',re.sub('([a-z])([A-Z])',r'\1-\2',s).lower()).strip('-')
    def typed(records):
        columns=list(zip(*records[1:]))
        numeric=[all(v=='NA' or isinstance(number(v),(int,float)) for v in c) for c in columns]
        return [[number(v) if n else None if v=='NA' else v for v,n in zip(r,numeric)] for r in records[1:]]
    with tempfile.TemporaryDirectory() as tmp:
        tmp=Path(tmp)
        for s in SOURCES.values():
            with tarfile.open(cache/s['file']) as a: a.extractall(tmp,filter='data')
        subprocess.run(['Rscript',str(ROOT/'scripts/export-r.R'),str(tmp),str(tmp),PACKAGE],check=True)
        for id,rd in [('penguins','palmerpenguins/man/penguins.Rd'),('penguins-raw','palmerpenguins/man/penguins_raw.Rd'),('gapminder','gapminder/man/gapminder.Rd'),('datasaurus-dozen','datasauRus/man/datasaurus_dozen.Rd'),('titanic-passengers','titanic/man/titanic_train.Rd')]:
            (PROVENANCE/f'{id}.Rd').write_bytes((tmp/rd).read_bytes())
        (PROVENANCE/'datasauRus-LICENSE').write_bytes((tmp/'datasauRus/LICENSE').read_bytes())
        for id,source,kw in R_RECIPES:
            with (tmp/f'{id}.csv').open(encoding='utf-8') as f: records=list(csv.reader(f))
            with (tmp/f'{id}.levels').open(encoding='utf-8') as f: factor_levels=list(csv.reader(f))[1:]
            aliases=kw.pop('aliases',{})
            names=[aliases.get(kebab(o),kebab(o)) for o in records[0]]
            levels={kebab(c):[] for c,_ in factor_levels}
            for c,l in factor_levels: levels[kebab(c)].append(l)
            levels.update(kw.pop('levels',{}))
            rows=typed(records)
            for j,n in enumerate(names):
                if n in levels and not {r[j] for r in rows if r[j] is not None}<=set(levels[n]): raise ValueError(f'{id}: {n} value outside levels')
            add(id,records[0],rows,source,names=names,levels=levels,**kw)
PIXELS=[f'pixel_{i}_{j}' for i in range(8) for j in range(8)]
NUMERIC=re.compile(r'[-+]?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?')
def kebab(s): return re.sub('[^a-z0-9]+','-',s.lower()).strip('-')
def members(source,*names,inner=None):
    with zipfile.ZipFile(cache/SOURCES[source]['file']) as a:
        if inner is None: return [a.read(n) for n in names]
        with zipfile.ZipFile(io.BytesIO(a.read(inner))) as b: return [b.read(n) for n in names]
def delimited(data,delimiter=',',skip='|'):
    lines=[l for l in data.decode('utf-8').splitlines() if l.strip() and not l.startswith(skip)]
    return [[v.strip() for v in r] for r in csv.reader(lines,delimiter=delimiter)]
def with_header(records): return records[0],records[1:]
def xlsx_records(data):
    m='{http://schemas.openxmlformats.org/spreadsheetml/2006/main}'
    with zipfile.ZipFile(io.BytesIO(data)) as a:
        shared=[''.join(t.text or '' for t in si.iter(m+'t')) for si in ET.fromstring(a.read('xl/sharedStrings.xml')).iter(m+'si')]
        sheet=ET.fromstring(a.read('xl/worksheets/sheet1.xml'))
    rows=[{re.match('[A-Z]+',c.get('r')).group():(shared[int(c.find(m+'v').text)] if c.get('t')=='s' else c.find(m+'v').text) for c in row.iter(m+'c') if c.find(m+'v') is not None} for row in sheet.iter(m+'row')]
    columns=sorted(rows[0],key=lambda k:(len(k),k))
    return [[cells.get(k,'') for k in columns] for cells in rows if cells]
def xls_records(data):
    import xlrd
    sheet=xlrd.open_workbook(file_contents=data).sheet_by_index(0)
    return [[v if isinstance(v,str) else repr(v) for v in sheet.row_values(i)] for i in range(sheet.nrows)]
def uci_typed(records,missing,strings):
    columns=[]
    for j,column in enumerate(zip(*records)):
        values=[None if v in missing else v for v in column]
        present=[v for v in values if v is not None]
        if j in strings or not all(NUMERIC.fullmatch(v) for v in present): columns.append(values); continue
        numbers=[None if v is None else float(v) for v in values]
        integral=all(v.is_integer() for v in numbers if v is not None)
        columns.append([None if v is None else int(v) if integral else v for v in numbers])
    return [list(r) for r in zip(*columns)]
def names_levels(text,pattern):
    return {kebab(n):[v.strip().strip('"') for v in vs.split(',')] for n,vs in re.findall(pattern,text,re.M)}
def optdigits():
    train,test=members('uci-optdigits','optdigits.tra','optdigits.tes')
    return PIXELS+['class','split'],[r+['train'] for r in delimited(train)]+[r+['test'] for r in delimited(test)]
def adult():
    data,test,doc=members('uci-adult','adult.data','adult.test','adult.names')
    original=re.findall(r'^([a-z-]+): ',doc.decode(),re.M)+['income','split']
    return original,[r+['train'] for r in delimited(data)]+[r[:-1]+[r[-1].removesuffix('.'),'test'] for r in delimited(test)]
def auto_mpg():
    (data,)=members('uci-auto-mpg','auto-mpg.data')
    rows=[left.split()+[name.strip().strip('"')] for left,name in (l.split('\t') for l in data.decode().splitlines() if l.strip())]
    return ['mpg','cylinders','displacement','horsepower','weight','acceleration','model year','origin','car name'],rows
def wine_quality():
    red,white=members('uci-wine-quality','winequality-red.csv','winequality-white.csv')
    (header,reds),(_,whites)=with_header(delimited(red,';')),with_header(delimited(white,';'))
    return header+['colour'],[r+['red'] for r in reds]+[r+['white'] for r in whites]
def student_performance():
    mat,por=members('uci-student-performance','student-mat.csv','student-por.csv',inner='student.zip')
    (header,maths),(_,portuguese)=with_header(delimited(mat,';')),with_header(delimited(por,';'))
    return header+['subject'],[r+['math'] for r in maths]+[r+['portuguese'] for r in portuguese]
def concrete():
    header,*rows=xls_records(*members('uci-concrete-strength','Concrete_Data.xls'))
    return [re.sub(r'\s+',' ',h).strip() for h in header],rows
def energy():
    header,*rows=xlsx_records(*members('uci-energy-efficiency','ENB2012_data.xlsx'))
    return header,rows
SPAM_CHARS={';':'semicolon','(':'paren','[':'bracket','!':'exclamation','$':'dollar','#':'hash'}
def spambase():
    data,doc=members('uci-spambase','spambase.data','spambase.names')
    return re.findall(r'^([a-z_0-9]+_[^:\s]+|capital_run_length_\w+):',doc.decode(),re.M)+['spam'],delimited(data)
def mushroom_attributes():
    doc=members('uci-mushroom','agaricus-lepiota.names')[0].decode()
    return [(n,' '.join(text.split())) for n,text in re.findall(r'^\s+\d+\. ([a-z?-]+):\s+(.*?)(?=^\s+\d+\. |\Z)',doc[doc.index('7. Attribute Information'):doc.index('8. Missing')],re.M|re.S)]
def mushroom():
    return ['class']+[n for n,_ in mushroom_attributes()],delimited(members('uci-mushroom','agaricus-lepiota.data')[0])
def uci_cite(authors,year,title,source,paper=None):
    s=SOURCES[source]
    return f"{authors} ({year}). {title}. UCI Machine Learning Repository. doi:{s['doi']}. {s['page']}"+(f' Described in: {paper}' if paper else '')
YES_NO=['no','yes']
STUDENT_YES_NO=['schoolsup','famsup','paid','activities','nursery','higher','internet','romantic']
BIKE_DESCRIPTIONS={'instant':'Record index','dteday':'Date (ISO 8601)','season':'Season: 1 winter, 2 spring, 3 summer, 4 fall','yr':'Year: 0 = 2011, 1 = 2012','mnth':'Month (1 to 12)','hr':'Hour (0 to 23)','holiday':'Whether the day is a holiday','weekday':'Day of the week','workingday':'1 if the day is neither weekend nor holiday, otherwise 0','weathersit':'Weather: 1 clear or partly cloudy; 2 mist; 3 light snow or rain; 4 heavy rain, ice, snow or fog','temp':'Normalized temperature in Celsius: (t - t_min)/(t_max - t_min), t_min = -8, t_max = +39','atemp':'Normalized feeling temperature in Celsius: (t - t_min)/(t_max - t_min), t_min = -16, t_max = +50','hum':'Normalized humidity, divided by 100','windspeed':'Normalized wind speed, divided by 67','casual':'Count of casual users','registered':'Count of registered users','cnt':'Count of total rental bikes, casual and registered'}
BIKE_PAPER='Fanaee-T, H. and Gama, J. (2013). Event labeling combining ensemble detectors and background knowledge. Progress in Artificial Intelligence, 1-15. doi:10.1007/s13748-013-0040-3'
DIGIT_DESCRIPTIONS={'digit':'Digit class 0-9'}|{f'pixel-{i}-{j}':f'Count of on pixels in 4x4 block row {i}, column {j} of the 32x32 bitmap (0-16)' for i in range(8) for j in range(8)}
UCI_DOCS={'uci-optdigits':['optdigits.names'],'uci-adult':['adult.names'],'uci-auto-mpg':['auto-mpg.names'],'uci-abalone':['abalone.names'],'uci-wine-quality':['winequality.names'],
          'uci-heart-disease':['heart-disease.names'],'uci-mushroom':['agaricus-lepiota.names'],'uci-car-evaluation':['car.names'],'uci-spambase':['spambase.names','spambase.DOCUMENTATION'],
          'uci-concrete-strength':['Concrete_Readme.txt'],'uci-bike-sharing':[('Readme.txt','bike-sharing-Readme.txt')]}
def import_uci():
    for source,docs in UCI_DOCS.items():
        for doc in docs:
            member,name=doc if isinstance(doc,tuple) else (doc,doc)
            (PROVENANCE/name).write_bytes(*members(source,member))
    (PROVENANCE/'bank-additional-names.txt').write_bytes(*members('uci-bank-marketing','bank-additional/bank-additional-names.txt',inner='bank-additional.zip'))
    (PROVENANCE/'student.txt').write_bytes(*members('uci-student-performance','student.txt',inner='student.zip'))
    adult_doc=members('uci-adult','adult.names')[0].decode()
    bank_doc=members('uci-bank-marketing','bank-additional/bank-additional-names.txt',inner='bank-additional.zip')[0].decode('utf-8-sig').replace('\r','')
    bank_levels=names_levels(bank_doc,r'^\s*\d+ - ([a-z_.]+)\s*:?[^(\n]*\(categorical: ([^)]*?)(?:;[^)]*)?\)')|{'month':['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'],'y':YES_NO}
    mushroom_codes={kebab(n):[pair.split('=')[1] for pair in re.split(r',\s*',text) if not pair.endswith('=?')] for n,text in mushroom_attributes()}
    mushroom_text={kebab(n):text for n,text in mushroom_attributes()}
    optdigits_cite=uci_cite('Alpaydin, E. and Kaynak, C.',1998,'Optical Recognition of Handwritten Digits','uci-optdigits')
    citations.update({'digits':optdigits_cite,'optdigits':optdigits_cite,
        'adult':uci_cite('Becker, B. and Kohavi, R.',1996,'Adult','uci-adult'),
        'auto-mpg':uci_cite('Quinlan, R.',1993,'Auto MPG','uci-auto-mpg'),
        'abalone':uci_cite('Nash, W., Sellers, T., Talbot, S., Cawthorn, A. and Ford, W.',1994,'Abalone','uci-abalone','Nash, W. J., Sellers, T. L., Talbot, S. R., Cawthorn, A. J. and Ford, W. B. (1994). The Population Biology of Abalone (Haliotis species) in Tasmania. I. Blacklip Abalone (H. rubra) from the North Coast and Islands of Bass Strait. Sea Fisheries Division, Technical Report No. 48.'),
        'wine-quality':uci_cite('Cortez, P., Cerdeira, A., Almeida, F., Matos, T. and Reis, J.',2009,'Wine Quality','uci-wine-quality','Cortez, P., Cerdeira, A., Almeida, F., Matos, T. and Reis, J. (2009). Modeling wine preferences by data mining from physicochemical properties. Decision Support Systems 47(4), 547-553. doi:10.1016/j.dss.2009.05.016'),
        'bank-marketing':uci_cite('Moro, S., Rita, P. and Cortez, P.',2014,'Bank Marketing','uci-bank-marketing','Moro, S., Cortez, P. and Rita, P. (2014). A data-driven approach to predict the success of bank telemarketing. Decision Support Systems. doi:10.1016/j.dss.2014.03.001'),
        'heart-disease':uci_cite('Janosi, A., Steinbrunn, W., Pfisterer, M. and Detrano, R.',1989,'Heart Disease','uci-heart-disease','Detrano, R. et al. (1989). International application of a new probability algorithm for the diagnosis of coronary artery disease. American Journal of Cardiology 64, 304-310.'),
        'mushroom':uci_cite('Schlimmer, J. (donor)',1987,'Mushroom','uci-mushroom','The Audubon Society Field Guide to North American Mushrooms (1981). G. H. Lincoff (Pres.), New York: Alfred A. Knopf.'),
        'car-evaluation':uci_cite('Bohanec, M.',1988,'Car Evaluation','uci-car-evaluation','Bohanec, M. and Rajkovic, V. (1988). Knowledge acquisition and explanation for multi-attribute decision making. 8th International Workshop on Expert Systems and their Applications, Avignon, 59-78.'),
        'spambase':uci_cite('Hopkins, M., Reeber, E., Forman, G. and Suermondt, J.',1999,'Spambase','uci-spambase'),
        'banknote-authentication':uci_cite('Lohweg, V.',2012,'Banknote Authentication','uci-banknote-authentication'),
        'seeds':uci_cite('Charytanowicz, M., Niewczas, J., Kulczycki, P., Kowalski, P. and Lukasik, S.',2010,'Seeds','uci-seeds','Charytanowicz, M., Niewczas, J., Kulczycki, P., Kowalski, P. A., Lukasik, S. and Zak, S. (2010). Complete Gradient Clustering Algorithm for Features Analysis of X-Ray Images. Information Technologies in Biomedicine. doi:10.1007/978-3-642-13105-9_2'),
        'concrete-strength':uci_cite('Yeh, I.-C.',1998,'Concrete Compressive Strength','uci-concrete-strength','Yeh, I.-C. (1998). Modeling of strength of high performance concrete using artificial neural networks. Cement and Concrete Research 28(12), 1797-1808. doi:10.1016/S0008-8846(98)00165-3. Copyright Prof. I-Cheng Yeh.'),
        'energy-efficiency':uci_cite('Tsanas, A. and Xifara, A.',2012,'Energy Efficiency','uci-energy-efficiency','Tsanas, A. and Xifara, A. (2012). Accurate quantitative estimation of energy performance of residential buildings using statistical machine learning tools. Energy and Buildings 49.'),
        'student-performance':uci_cite('Cortez, P.',2008,'Student Performance','uci-student-performance','Cortez, P. and Silva, A. (2008). Using data mining to predict secondary school student performance. Proceedings of the 5th Annual Future Business Technology Conference.'),
        'bike-sharing-daily':uci_cite('Fanaee-T, H.',2013,'Bike Sharing','uci-bike-sharing',BIKE_PAPER),
        'bike-sharing-hourly':uci_cite('Fanaee-T, H.',2013,'Bike Sharing','uci-bike-sharing',BIKE_PAPER)})
    recipes=[
        ('digits','uci-optdigits',lambda:(PIXELS+['class'],delimited(members('uci-optdigits','optdigits.tes')[0])),dict(
            names=[kebab(n) for n in PIXELS]+['digit'],targets=['digit'],descriptions=DIGIT_DESCRIPTIONS,
            notes='The 1,797-row optdigits.tes test portion, identical row for row to the data of scikit-learn load_digits (checked against scikit-learn 1.8.0): 64 pixel counts (0-16) of an 8x8 grid in row-major order, then the digit class. Pixel names follow scikit-learn (pixel_ROW_COL; the source names no attributes). The full 5,620-row set is optdigits.')),
        ('optdigits','uci-optdigits',optdigits,dict(
            names=[kebab(n) for n in PIXELS]+['digit','split'],targets=['digit'],levels={'split':['train','test']},descriptions=DIGIT_DESCRIPTIONS|{'split':'Source file: train (optdigits.tra, 30 writers) or test (optdigits.tes, 13 other writers)'},
            notes='All 5,620 rows: the 3,823 optdigits.tra rows, then the 1,797 optdigits.tes rows, told apart by split. Writers of the two splits do not overlap. Pixel names follow scikit-learn; digits is the test portion alone.')),
        ('adult','uci-adult',adult,dict(
            missing={'?'},targets=['income'],levels=names_levels(adult_doc,r'^([a-z-]+): ((?!continuous)[^\n]*)\.$')|{'income':['<=50K','>50K'],'split':['train','test']},
            descriptions={'fnlwgt':'Final sampling weight','education-num':'Education, as a number','income':'Whether income exceeds $50K/yr','split':'Source file: train (adult.data) or test (adult.test)'},
            notes='1994 US Census extract: the 32,561 adult.data rows, then the 16,281 adult.test rows, told apart by split. The trailing period on test income labels is removed. ? is rendered as missing (workclass, occupation, native-country).')),
        ('auto-mpg','uci-auto-mpg',auto_mpg,dict(
            missing={'?'},identifiers=['car-name'],targets=['mpg'],
            descriptions={'cylinders':'Number of cylinders','model-year':'Model year (19xx)','origin':'Origin code (1, 2 or 3)','car-name':'Car name; not unique across rows'},
            notes='auto-mpg.data: the StatLib cars with the 8 unknown-mpg rows removed. The 6 ? horsepower values are rendered as missing. Integral values written with a decimal point are stored as integers.')),
        ('abalone','uci-abalone',lambda:(['Sex','Length','Diameter','Height','Whole weight','Shucked weight','Viscera weight','Shell weight','Rings'],delimited(members('uci-abalone','abalone.data')[0])),dict(
            levels={'sex':['M','F','I']},targets=['rings'],units={'length':'mm / 200','diameter':'mm / 200','height':'mm / 200','whole-weight':'grams / 200','shucked-weight':'grams / 200','viscera-weight':'grams / 200','shell-weight':'grams / 200'},
            descriptions={'sex':'M, F or I (infant)','length':'Longest shell measurement','diameter':'Perpendicular to length','height':'With meat in shell','whole-weight':'Whole abalone','shucked-weight':'Weight of meat','viscera-weight':'Gut weight (after bleeding)','shell-weight':'After being dried','rings':'Rings; +1.5 gives the age in years'},
            notes='Continuous measurements as distributed, scaled by 1/200 by the donors; rows with missing values were removed upstream.')),
        ('wine-quality','uci-wine-quality',wine_quality,dict(
            levels={'colour':['red','white']},targets=['quality'],descriptions={'quality':'Sensory quality score between 0 and 10','colour':'Red or white vinho verde'},
            notes='Red and white vinho verde in one table: the 1,599 red rows, then the 4,898 white rows, told apart by colour.')),
        ('bank-marketing','uci-bank-marketing',lambda:with_header(delimited(members('uci-bank-marketing','bank-additional/bank-additional-full.csv',inner='bank-additional.zip')[0],';')),dict(
            missing={'unknown'},levels={kebab(k):[l for l in v if l!='unknown'] for k,v in bank_levels.items()},targets=['y'],units={'duration':'seconds'},
            descriptions={'duration':'Last contact duration; known only after the call, so for benchmarking only','pdays':'Days since last contact in a previous campaign; 999 = not previously contacted','y':'Whether the client subscribed a term deposit'},
            notes='bank-additional-full.csv: 41,188 calls, May 2008 to November 2010, ordered by date, with five social and economic context attributes; the file analysed by Moro et al. (2014), chosen over the older bank-full.csv (45,211 rows, no context attributes). The documented unknown label is rendered as missing; pdays 999 is kept.')),
        ('heart-disease','uci-heart-disease',lambda:(['age','sex','cp','trestbps','chol','fbs','restecg','thalach','exang','oldpeak','slope','ca','thal','num'],delimited(members('uci-heart-disease','processed.cleveland.data')[0])),dict(
            missing={'?'},targets=['num'],units={'age':'years','trestbps':'mm Hg','chol':'mg/dl'},
            descriptions={'age':'Age','sex':'1 = male, 0 = female','cp':'Chest pain type: 1 typical angina, 2 atypical angina, 3 non-anginal pain, 4 asymptomatic','trestbps':'Resting blood pressure on admission','chol':'Serum cholesterol','fbs':'Fasting blood sugar > 120 mg/dl: 1 true, 0 false','restecg':'Resting electrocardiographic results: 0 normal, 1 ST-T wave abnormality, 2 left ventricular hypertrophy','thalach':'Maximum heart rate achieved','exang':'Exercise induced angina: 1 yes, 0 no','oldpeak':'ST depression induced by exercise relative to rest','slope':'Slope of the peak exercise ST segment: 1 upsloping, 2 flat, 3 downsloping','ca':'Number of major vessels (0-3) colored by fluoroscopy','thal':'3 normal, 6 fixed defect, 7 reversible defect','num':'Angiographic disease status: 0 absence, 1-4 presence'},
            notes='processed.cleveland.data: the 303 Cleveland Clinic patients and the 14 attributes used in published experiments. ? is rendered as missing (ca, thal). Coded attributes stay numeric with their codes in the descriptions; integral values written with a decimal point are stored as integers.')),
        ('mushroom','uci-mushroom',mushroom,dict(
            missing={'?'},levels={'class':['e','p']}|mushroom_codes,targets=['class'],descriptions={'class':'edible=e, poisonous=p'}|mushroom_text,
            notes='Hypothetical samples of 23 gilled mushroom species, keeping the source one-letter codes (decoded in each description) and the source column order with class first. bruises? is renamed bruises. The 2,480 ? stalk-root values are rendered as missing.')),
        ('car-evaluation','uci-car-evaluation',lambda:(['buying','maint','doors','persons','lug_boot','safety','class'],delimited(members('uci-car-evaluation','car.data')[0])),dict(
            levels={'buying':['vhigh','high','med','low'],'maint':['vhigh','high','med','low'],'doors':['2','3','4','5more'],'persons':['2','4','more'],'lug-boot':['small','med','big'],'safety':['low','med','high'],'class':['unacc','acc','good','vgood']},targets=['class'],
            descriptions={'buying':'Buying price','maint':'Price of the maintenance','doors':'Number of doors','persons':'Capacity in terms of persons to carry','lug-boot':'Size of luggage boot','safety':'Estimated safety of the car','class':'Car acceptability'},
            notes='All 1,728 combinations of the six attributes with their acceptability class.')),
        ('spambase','uci-spambase',spambase,dict(
            names_fn=lambda n:'char-freq-'+SPAM_CHARS[n[-1]] if n.startswith('char_freq_') else kebab(n),targets=['spam'],descriptions={'spam':'1 = spam, 0 = not spam'},
            notes='4,601 e-mails: 48 word and 6 character frequencies (percent), three capital-run-length statistics and the spam class. Character columns are named after the character (char_freq_; is char-freq-semicolon, and so on).')),
        ('banknote-authentication','uci-banknote-authentication',lambda:(['variance','skewness','curtosis','entropy','class'],delimited(members('uci-banknote-authentication','data_banknote_authentication.txt')[0])),dict(
            targets=['class'],descriptions={'variance':'Variance of the wavelet transformed image','skewness':'Skewness of the wavelet transformed image','curtosis':'Curtosis of the wavelet transformed image','entropy':'Entropy of the image','class':'Class (0 or 1)'},
            notes='Wavelet features of images of genuine and forged banknote-like specimens. The archive carries no documentation file; names and descriptions follow the UCI dataset page.')),
        ('seeds','uci-seeds',lambda:(['area','perimeter','compactness','length of kernel','width of kernel','asymmetry coefficient','length of kernel groove','class'],[l.split() for l in members('uci-seeds','seeds_dataset.txt')[0].decode().splitlines() if l.strip()]),dict(
            targets=['class'],descriptions={'area':'Area A','perimeter':'Perimeter P','compactness':'Compactness C = 4*pi*A/P^2','class':'Wheat variety (1, 2 or 3): Kama, Rosa and Canadian, 70 kernels each'},
            notes='X-ray measurements of 210 wheat kernels. The source file separates some fields with two tabs; fields are split on whitespace. The archive carries no documentation file; names follow the UCI dataset page.')),
        ('concrete-strength','uci-concrete-strength',concrete,dict(
            names=['cement','blast-furnace-slag','fly-ash','water','superplasticizer','coarse-aggregate','fine-aggregate','age','concrete-compressive-strength'],targets=['concrete-compressive-strength'],
            units={n:'kg/m^3 of mixture' for n in ['cement','blast-furnace-slag','fly-ash','water','superplasticizer','coarse-aggregate','fine-aggregate']}|{'age':'days','concrete-compressive-strength':'MPa'},
            notes='Concrete_Data.xls in raw, unscaled form; cell values as stored in the workbook. Column names shortened from the workbook headers (kept as original names).')),
        ('energy-efficiency','uci-energy-efficiency',energy,dict(
            names=['relative-compactness','surface-area','wall-area','roof-area','overall-height','orientation','glazing-area','glazing-area-distribution','heating-load','cooling-load'],targets=['heating-load','cooling-load'],
            descriptions={'relative-compactness':'Relative compactness','surface-area':'Surface area','wall-area':'Wall area','roof-area':'Roof area','overall-height':'Overall height','orientation':'Orientation','glazing-area':'Glazing area','glazing-area-distribution':'Glazing area distribution','heating-load':'Heating load','cooling-load':'Cooling load'},
            notes='768 simulated building shapes from ENB2012_data.xlsx with two responses. Columns X1-X8, Y1, Y2 named from the documentation; cell values as stored in the workbook.')),
        ('student-performance','uci-student-performance',student_performance,dict(
            levels={'school':['GP','MS'],'sex':['F','M'],'address':['U','R'],'famsize':['LE3','GT3'],'pstatus':['T','A'],'mjob':['teacher','health','services','at_home','other'],'fjob':['teacher','health','services','at_home','other'],'reason':['home','reputation','course','other'],'guardian':['mother','father','other'],'subject':['math','portuguese']}|{n:YES_NO for n in STUDENT_YES_NO},
            targets=['g3'],descriptions={'g1':'First period grade (0-20)','g2':'Second period grade (0-20)','g3':'Final grade (0-20)','subject':'Course: math (student-mat.csv) or portuguese (student-por.csv)'},
            notes='Two Portuguese secondary schools in one table: the 395 mathematics rows, then the 649 Portuguese-language rows, told apart by subject. Some students appear in both courses; the source has no student identifier.')),
        ('bike-sharing-daily','uci-bike-sharing',lambda:with_header(delimited(members('uci-bike-sharing','day.csv')[0])),dict(
            identifiers=['instant'],targets=['cnt'],descriptions=BIKE_DESCRIPTIONS,notes='Capital Bikeshare (Washington, D.C.) rentals per day, 2011-2012, with weather and calendar attributes (day.csv).')),
        ('bike-sharing-hourly','uci-bike-sharing',lambda:with_header(delimited(members('uci-bike-sharing','hour.csv')[0])),dict(
            identifiers=['instant'],targets=['cnt'],descriptions=BIKE_DESCRIPTIONS,notes='Capital Bikeshare (Washington, D.C.) rentals per hour, 2011-2012, with weather and calendar attributes (hour.csv); hours without rentals are absent.')),
    ]
    for id,source,read,kw in recipes:
        original,records=read()
        names_fn=kw.pop('names_fn',kebab)
        names=kw.pop('names',None) or [names_fn(n) for n in original]
        levels=kw.pop('levels',{})
        rows=uci_typed(records,kw.pop('missing',set()),{j for j,n in enumerate(names) if n in levels})
        if {len(r) for r in rows}!={len(names)}: raise ValueError(f'{id}: ragged rows')
        for j,n in enumerate(names):
            if n not in levels: continue
            present={r[j] for r in rows if r[j] is not None}
            if not present<=set(levels[n]): raise ValueError(f'{id}: {n} values {present-set(levels[n])} outside levels')
            levels[n]=[l for l in levels[n] if l in present]
        add(id,original,rows,source,names=names,levels=levels,**kw)
{'datasets-core':import_core,'datasets-r':import_r,'datasets-uci':import_uci}[PACKAGE]()
(OUT/'registry.rktd').write_text(sexp(dict(sorted(registry.items())))+'\n')
for stale in (OUT/'data').iterdir():
    if stale.name not in {f'{id}.rktd.gz' for id in registry}: stale.unlink()
print('Imported',len(registry),'datasets')
