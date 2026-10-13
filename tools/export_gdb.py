#!/usr/bin/env python3
"""Convierte tablas ANLA del ZIP de resultados a FileGDB, sin geometrías."""
import argparse
import csv
import json
from pathlib import Path
import shutil
import tempfile
import zipfile

def read_csv(path):
    with path.open(encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f))

def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument("input", type=Path, help="ZIP de resultados o carpeta descomprimida")
    p.add_argument("output", type=Path, help="Carpeta nueva terminada en .gdb")
    p.add_argument("--allow-draft", action="store_true", help="Permitir borrador con pendientes; nunca implica conformidad ANLA")
    args=p.parse_args()
    if args.output.suffix.lower()!=".gdb": p.error("output debe terminar en .gdb")
    if args.output.exists(): p.error("La salida ya existe; use un nombre nuevo")
    try:
        from osgeo import ogr, gdal
    except ImportError:
        p.error("Se requiere Python con GDAL: ejecutar en el entorno Python de QGIS/OSGeo4W o instalar python3-gdal")
    gdal.UseExceptions()
    driver=gdal.GetDriverByName("OpenFileGDB")
    if driver is None: p.error("GDAL no tiene OpenFileGDB; use GDAL >= 3.6 con escritura")
    root=Path(__file__).resolve().parents[1]/"app"/"data"
    schema=json.loads((root/"anla_schema.json").read_text())
    domains=json.loads((root/"anla_domains.json").read_text())
    with tempfile.TemporaryDirectory() as tmp:
        if args.input.is_dir(): source=args.input
        else:
            source=Path(tmp)
            # Extraer solo nombres esperados; evita rutas arbitrarias de ZIP.
            allowed={"MuestreoFaunaTB.csv","MuestreoFaunaResultadosTB.csv","Validacion_ANLA.csv","parametros.json"}
            with zipfile.ZipFile(args.input) as z:
                for name in allowed:
                    if name in z.namelist(): (source/name).write_bytes(z.read(name))
        required=["MuestreoFaunaTB.csv","MuestreoFaunaResultadosTB.csv","Validacion_ANLA.csv"]
        for n in required:
            if not (source/n).is_file(): p.error(f"Falta {n}")
        issues=read_csv(source/"Validacion_ANLA.csv")
        if issues and not args.allow_draft:
            p.error(f"Hay {len(issues)} pendientes ANLA. Revise las tablas; para una GDB de borrador use --allow-draft")
        args.output.parent.mkdir(parents=True,exist_ok=True)
        types={"String":ogr.OFTString,"Double":ogr.OFTReal,"Integer":ogr.OFTInteger,"SmallInteger":ogr.OFTInteger,"Date":ogr.OFTDate}
        ds=driver.Create(str(args.output),0,0,0,gdal.GDT_Unknown)
        if ds is None: raise RuntimeError("No se pudo crear la GDB")
        try:
            used={f["domain"] for fields in schema.values() for f in fields if f["domain"] in domains}
            for n in used:
                dom=ogr.CreateCodedFieldDomain(n,n,ogr.OFTReal,ogr.OFSTNone,{str(int(v["code"])):v["label"] for v in domains[n]})
                if not ds.AddFieldDomain(dom): raise RuntimeError(f"No se pudo añadir el dominio {n}")
            counts={}
            for table,fields in schema.items():
                layer=ds.CreateLayer(table,geom_type=ogr.wkbNone)
                if layer is None: raise RuntimeError(f"No se pudo crear {table}")
                for f in fields:
                    field_type=types[f["type"]]
                    fd=ogr.FieldDefn(f["field"],field_type)
                    if field_type==ogr.OFTString: fd.SetWidth(int(f["size"]))
                    if f["type"]=="SmallInteger": fd.SetSubType(ogr.OFSTInt16)
                    if f["domain"] in used:
                        # Dominios categóricos del diccionario: reales e enteros requieren tipos compatibles.
                        dv=domains[f["domain"]]
                        dn=f["domain"]
                        if field_type==ogr.OFTInteger:
                            dn=dn+("_Int16" if f["type"]=="SmallInteger" else "_Integer")
                            if ds.GetFieldDomain(dn) is None:
                                dom=ogr.CreateCodedFieldDomain(dn,dn,ogr.OFTInteger,ogr.OFSTInt16 if f["type"]=="SmallInteger" else ogr.OFSTNone,{str(int(v["code"])):v["label"] for v in dv})
                                if not ds.AddFieldDomain(dom): raise RuntimeError(f"No se pudo añadir {dn}")
                        fd.SetDomainName(dn)
                    if layer.CreateField(fd)!=0: raise RuntimeError(f"No se pudo crear {f['field']}")
                rows=read_csv(source/(table+".csv"))
                for ri,row in enumerate(rows,1):
                    feat=ogr.Feature(layer.GetLayerDefn())
                    for f in fields:
                        value=row.get(f["field"],"").strip()
                        if not value: continue
                        if f["type"]=="String":
                            if len(value)>int(f["size"]): raise ValueError(f"{table} fila {ri}: {f['field']} supera longitud")
                            feat.SetField(f["field"],value)
                        elif f["type"]=="Date":
                            from datetime import date
                            dt=date.fromisoformat(value)
                            feat.SetField(f["field"],dt.year,dt.month,dt.day,0,0,0,0)
                        else:
                            number=float(value)
                            if f["type"] in ("Integer","SmallInteger") and not number.is_integer(): raise ValueError("Código no entero")
                            if f["domain"] in domains and number not in {v["code"] for v in domains[f["domain"]]}: raise ValueError(f"Código fuera de dominio: {f['field']}")
                            feat.SetField(f["field"],int(number) if f["type"] in ("Integer","SmallInteger") else number)
                    if layer.CreateFeature(feat)!=0: raise RuntimeError(f"Falló {table} fila {ri}")
                    feat=None
                counts[table]=len(rows)
            ds=None
            check=gdal.OpenEx(str(args.output),gdal.OF_VECTOR)
            if check is None: raise RuntimeError("No se pudo reabrir la GDB")
            for table,n in counts.items():
                if check.GetLayerByName(table).GetFeatureCount()!=n: raise RuntimeError(f"Conteo incorrecto: {table}")
            check=None
        except Exception:
            ds=None
            shutil.rmtree(args.output,ignore_errors=True)
            raise
        report={"estado":"BORRADOR" if issues else "Tablas convertidas; capas y relaciones pendientes", "registros":counts,"pendientes":issues,"geometrias":False}
        args.output.with_suffix(".validation.json").write_text(json.dumps(report,ensure_ascii=False,indent=2))
        print(json.dumps({"output":str(args.output),"registros":counts,"pendientes":len(issues)},ensure_ascii=False))
if __name__=="__main__": main()
