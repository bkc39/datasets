#!/usr/bin/env python3
"""Regenerate committed data from hash-verified archives; needs Python 3.12+ and R; source definitions are pinned to R 4.3.3.
Pass an archive directory (offline), or --fetch to download sources into it first.
Writes TARGET/registry.rktd and TARGET/data/<id>.rktd.gz for one package.
"""
import argparse, csv, gzip, hashlib, io, json, re, subprocess, sys, tarfile, tempfile, urllib.request, zipfile
from pathlib import Path
ROOT = Path(__file__).resolve().parents[1]
DEFAULTS = {'datasets-core': ('datasets-core/datasets/core-data', 'datasets-core/datasets/provenance'),
            'datasets-r': ('datasets-r/datasets/r-data', 'datasets-r/datasets/r-data/provenance')}
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
                  'datasaurus-dozen':'Davies, R., Locke, S. and D\'Agostino McGowan, L. (2025). datasauRus: Datasets from the Datasaurus Dozen. R package version 0.1.9. Matejka, J. and Fitzmaurice, G. (2017). Same Stats, Different Graphs. CHI 2017. doi:10.1145/3025453.3025912',
                  'titanic-passengers':'Hendricks, P. (2015). titanic: Titanic Passenger Survival Data Set. R package version 0.1.0. Data: Kaggle, Titanic: Machine Learning from Disaster, https://www.kaggle.com/c/titanic/data'})
citations['penguins-raw']=citations['penguins']
PENGUIN_UNITS={'bill-length-mm':'mm','bill-depth-mm':'mm','culmen-length-mm':'mm','culmen-depth-mm':'mm','flipper-length-mm':'mm','body-mass-g':'g','delta-15-n':'per mil (o/oo)','delta-13-c':'per mil (o/oo)'}
R_RECIPES=[
    ('penguins','palmerpenguins',dict(units=PENGUIN_UNITS,targets=['species'],
        descriptions={'species':'Penguin species','island':'Island in the Palmer Archipelago, Antarctica','bill-length-mm':'Bill (culmen) length','bill-depth-mm':'Bill (culmen) depth','flipper-length-mm':'Flipper length','body-mass-g':'Body mass','sex':'Sex','year':'Study year'},
        notes='Adult Adélie, Chinstrap and Gentoo penguins near Palmer Station, Antarctica, 2007–2009. Missing measurements and sexes retained as missing. R factors rendered as labels with levels in metadata; names normalized to kebab-case.')),
    ('penguins-raw','palmerpenguins',dict(units=PENGUIN_UNITS,targets=['species'],aliases={'delta-15-n-o-oo':'delta-15-n','delta-13-c-o-oo':'delta-13-c'},
        levels={'species':['Adelie Penguin (Pygoscelis adeliae)','Chinstrap penguin (Pygoscelis antarctica)','Gentoo penguin (Pygoscelis papua)'],'region':['Anvers'],'island':['Biscoe','Dream','Torgersen'],'stage':['Adult, 1 Egg Stage'],'clutch-completion':['No','Yes'],'sex':['FEMALE','MALE']},
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
            id=p.stem; kw={}
            if id=='mtcars': kw=dict(identifiers=['model'],units={'mpg':'miles/US gallon','disp':'cubic inches','hp':'gross horsepower','wt':'1000 lb','qsec':'seconds'},descriptions={'cyl':'Number of cylinders','drat':'Rear axle ratio','vs':'Engine: 0 V-shaped, 1 straight','am':'Transmission: 0 automatic, 1 manual','gear':'Forward gears','carb':'Carburetors'})
            if id=='us-arrests': kw=dict(identifiers=['state'],units={'murder':'arrests per 100,000','assault':'arrests per 100,000','urban-pop':'percent','rape':'arrests per 100,000'},notes='1973 US arrests. Historical variable names and values retained, including the documented UrbanPop transcription issue; see upstream Rd.')
            if id=='faithful': kw=dict(units={'eruptions':'minutes','waiting':'minutes'},descriptions={'eruptions':'Duration of an Old Faithful eruption','waiting':'Waiting time to the next eruption'},notes='Original rounded eruption durations retained; see upstream documentation.')
            if id=='airquality': kw=dict(units={'ozone':'ppb','solar-r':'Langleys','wind':'mph','temp':'degrees Fahrenheit'},descriptions={'ozone':'Mean ozone, 1300–1500 hours, Roosevelt Island','solar-r':'Solar radiation, 0800–1200 hours, Central Park','wind':'Average wind speed, 0700 and 1000 hours, LaGuardia Airport','temp':'Maximum daily temperature, LaGuardia Airport','month':'Calendar month (5–9)','day':'Day of month'},notes='New York, May–September 1973. All 37 Ozone and 7 Solar.R missing observations retained.')
            if id=='plant-growth': kw=dict(levels={'group':['ctrl','trt1','trt2']},descriptions={'weight':'Dried plant weight','group':'Control or treatment group'},targets=['weight'])
            if id=='tooth-growth': kw=dict(levels={'supplement':['OJ','VC']},units={'dose':'mg/day'},descriptions={'length':'Odontoblast length','supplement':'Orange juice (OJ) or ascorbic acid (VC)'},targets=['length'])
            if id=='titanic': kw=dict(levels={'class':['1st','2nd','3rd','Crew'],'sex':['Male','Female'],'age':['Child','Adult'],'survived':['No','Yes']},notes='R contingency table expanded in R array order, including all eight zero-count cells. frequency counts people; rows are cells, not individuals.')
            if id=='air-passengers': kw=dict(units={'passengers':'thousands'},descriptions={'year':'Calendar year','month':'Calendar month, January=1','passengers':'International airline passenger count'},series={'frequency':12,'start':[1949,1],'end':[1960,12],'index':['year','month']},notes='Monthly international airline passengers, 1949–1960. Calendar columns derived from the R ts index.')
            if id=='diabetes': kw=dict(targets=['response'],units={n:'standardized (unit L2 norm)' for n in ['age','sex','bmi','map','tc','ldl','hdl','tch','ltg','glu']},descriptions={'age':'Age, standardized','sex':'Sex, standardized source coding','bmi':'Body mass index, standardized','map':'Mean arterial pressure, standardized','tc':'Total cholesterol, standardized','ldl':'Low-density lipoproteins, standardized','hdl':'High-density lipoproteins, standardized','tch':'Total cholesterol / HDL, standardized','ltg':'Log triglycerides, standardized','glu':'Blood glucose, standardized','response':'Quantitative disease progression one year after baseline'},notes='lars standardized ten-predictor x matrix and y response; source x. name prefix removed (recorded in original-name); columns centered and scaled to unit Euclidean norm. No raw measurements or exact scikit-learn parity promised.')
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
{'datasets-core':import_core,'datasets-r':import_r}[PACKAGE]()
(OUT/'registry.rktd').write_text(sexp(dict(sorted(registry.items())))+'\n')
for stale in (OUT/'data').iterdir():
    if stale.name not in {f'{id}.rktd.gz' for id in registry}: stale.unlink()
print('Imported',len(registry),'datasets')
