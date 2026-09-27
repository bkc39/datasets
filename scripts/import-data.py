#!/usr/bin/env python3
"""Regenerate committed data from hash-verified archives; needs Python 3.12+ and R; source definitions are pinned to R 4.3.3.
Pass an archive directory (offline), or --fetch DIRECTORY to download sources first.
"""
import csv, hashlib, io, json, re, subprocess, sys, tarfile, tempfile, urllib.request, zipfile
from pathlib import Path
ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT/'datasets-core/datasets/private'
SOURCES = json.loads((ROOT/'scripts/sources.json').read_text())
fetch = '--fetch' in sys.argv
cache = Path(sys.argv[-1]); cache.mkdir(parents=True,exist_ok=True)
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
def add(id,original,rows,source,levels={},units={},descriptions={},identifiers=[],targets=[],notes='',series=None):
    names=[re.sub('[^a-z0-9]+','-',s.lower()).strip('-') for s in original]
    aliases={'freq':'frequency','len':'length','supp':'supplement','solar-r':'solar-r','urbanpop':'urban-pop','sepal-length':'sepal-length'}
    names=[aliases.get(n,n) for n in names]
    if id=='diabetes': names=[n.removeprefix('x-') for n in names]
    columns=[]
    for j,(name,orig) in enumerate(zip(names,original)):
        values=[r[j] for r in rows if r[j] is not None]
        kind='identifier' if name in identifiers else 'categorical' if name in levels else 'integer' if all(isinstance(v,int) for v in values) else 'real' if all(isinstance(v,(int,float)) for v in values) else 'string'
        columns.append({'name':name,'original-name':orig,'type':kind,'levels':levels.get(name,[]),'unit':units.get(name,False),'description':descriptions.get(name,orig),'missing-count':sum(r[j] is None for r in rows)})
    text='(\n'+'\n'.join('  '+sexp(r) for r in rows)+'\n)\n'
    (OUT/'data'/f'{id}.rktd').write_text(text)
    s=SOURCES[source]
    registry[id]={'name':id,'rows':len(rows),'column-count':len(names),'columns':columns,'identifiers':identifiers,'suggested-targets':targets,'source-url':s['url'],'source-version':'R 4.3.3' if source=='R' else 'lars 1.3' if source=='lars' else 'UCI snapshot 2026-09-26 (hash pinned)','source-sha256':s['sha256'],'sha256':hashlib.sha256(text.encode()).hexdigest(),'license':'GPL-2.0-only OR GPL-3.0-only' if source=='R' else 'GPL-2.0-only' if source=='lars' else 'CC-BY-4.0','citation':citations.get(id,'R Core Team (2024). R: A Language and Environment for Statistical Computing. See bundled upstream documentation for original references.'),'notes':notes or 'Source row order and values retained; names normalized to kebab-case.','time-series':series or False}
citations={'iris':'Fisher, R. (1936). Iris. UCI. doi:10.24432/C56C76','wine':'Aeberhard, S. and Forina, M. (1991). Wine. UCI. doi:10.24432/C5PC7J','breast-cancer':'Wolberg, W., Mangasarian, O., Street, N., and Street, W. (1993). Breast Cancer Wisconsin (Diagnostic). UCI. doi:10.24432/C5DW2B','diabetes':'Efron, Hastie, Johnstone and Tibshirani (2004). Least Angle Regression. Annals of Statistics 32(2), 407-499. doi:10.1214/009053604000000067'}
with tempfile.TemporaryDirectory() as tmp:
    tmp=Path(tmp)
    for src in ['R','lars']:
        with tarfile.open(cache/SOURCES[src]['file']) as a: a.extractall(tmp,filter='data')
    subprocess.run(['Rscript',str(ROOT/'scripts/export-r.R'),str(tmp),str(tmp)],check=True)
    docs=OUT.parent/'provenance'; docs.mkdir(exist_ok=True)
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
            if name.endswith('.names'): (OUT.parent/'provenance'/name).write_bytes(a.read(name))
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
(OUT/'registry.rktd').write_text(sexp(dict(sorted(registry.items())))+'\n')
print('Imported',len(registry),'datasets')
