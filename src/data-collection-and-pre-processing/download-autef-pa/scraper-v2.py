import requests
from multiprocessing import Pool, Manager
from itertools import repeat
from random import randint
from time import sleep
import pandas as pd
import traceback
import pickle
import os

#Mude isso para aumentar ou diminuir a paralelização
P_COUNT = 6

class Scraper():
    def __init__(self):
        self.df = pd.DataFrame()
        if not os.path.exists("gf"):
            os.mkdir("gf")
        if not os.path.exists("autef"):
            os.mkdir("autef")
        if not os.path.exists("pmf"):
            os.mkdir("pmf")
        try:
            with open("last_pages.obj", 'rb') as f:
                self.lp = pickle.load(f)
        except:
            self.lp = {
                "autef": [1],
                "pmf": [1],
                "gf":[1],
                "autef_p": [1],
                "gf_p":[1]
            }
        try:
            with open("last_page_xlsx.obj", 'rb') as f:
                self.lpx = pickle.load(f)
        except:
            self.lpx = []
        self.q = Manager().Queue()

    def get_id(self, i, url, prods, d_i, d_f):
        headers = {
            "Accept": "application/json, text/plain, */*",
            "Accept-Language": "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
            "Connection": "keep-alive",
            "Referer": "http://portaldatransparencia.semas.pa.gov.br/",
            "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
        }
        df_ts = []
        querystring = {"page":"1", 'data_inicio': d_i, "data_fim": d_f}

        res = requests.request("GET", url,  headers=headers, params=querystring)
        try:
            for r in res.json()["data"]:
                if "guia" not in url:
                    if prods:
                        df_ts += r["produtos"]
                else:
                    if prods or prods == "only":
                        df_ts += self.get_gfprod(r['id'], d_i, d_f)

            if "autef" in url:
                for id in res.json()["data"]:
                    self.get_zip(id["titulo"], 'autef')
                if prods:
                    self.df = pd.concat([self.df, pd.DataFrame.from_dict(df_ts)])
                    self.df.drop_duplicates(inplace=True)
                    self.df.to_csv("produtos.csv", encoding="utf-8", index=False)
                    self.lp['autef_p'].append(i)
                else:
                    self.lp['autef'].append(i)
                
            elif "pmf" in url:
                for id in res.json()["data"]:
                    self.get_zip(id["titulo"], 'pmf')
                self.lp['pmf'].append(i)

            elif "guia-florestal" in url:
                if prods != "only" or prods == False:
                    for id in res.json()["data"]:
                        self.get_zip(id["id"], 'gf')
                    self.lp['gf'].append(i)
                elif prods or prods == "only":
                    self.df = pd.DataFrame.from_dict(df_ts)
                    self.df.drop_duplicates(inplace=True)
                    self.df.to_csv("gf_produtos.csv", mode = 'a', encoding="utf-8", index=False)
                    self.lp['gf_p'].append(i)

            with open("last_pages.obj", 'wb') as f:
                    pickle.dump(self.lp, f)
            
        except: pass

    def get_gfprod(self, id, d_i, d_f):
        dfts = []
        querystring = {"page":"1","id":f"{id}", 'data_inicio': d_i, "data_fim": d_f}
        r = requests.get("http://portaldatransparencia.semas.pa.gov.br/portal-da-transparencia-api/api/v1/guia-florestal/especies", params=querystring)
        try:
            for r in r.json()["data"]:
                r.pop("tipoEspecieArea")
                dfts += [r]
        except:
            sleep(5)
        return dfts
        
    def get_ids(self, url, prods, d_i, d_f):
        try:
            querystring = {"page":"1", 'data_inicio': d_i, "data_fim": d_f}
            r = requests.request("GET", url,  headers={"User-Agent":"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"}, params=querystring)
            l_page = int(r.json()["last_page"])+1
            #Mude o valor de l_page para trocar a página final que o programa irá terminar
            #l_page = 10
            if "guia" in url:
                if prods == True:
                    i = [x for x in list(range(0, l_page)) if x not in self.lp['gf_p']]
                else:
                    i = [x for x in list(range(0, l_page)) if x not in self.lp['gf']]
            elif "pmf" in url:
                i = [x for x in list(range(0, l_page)) if x not in self.lp['pmf']]
            elif "autef" in url:
                if prods == True:
                    i = [x for x in list(range(0, l_page)) if x not in self.lp['autef_p']]
                else:
                    i = [x for x in list(range(0, l_page)) if x not in self.lp['autef']]
            with Pool(P_COUNT) as p:
                res = p.starmap(self.get_id, zip(i, repeat(url), repeat(prods), repeat(d_i), repeat(d_f)), chunksize=int(len(i)/4))
                for r in res:
                    pass
        except: print(traceback.format_exc())

    def get_zip(self, id,folder):
        try:
            if not folder == "gf":
                url = f"http://177.74.62.130/arquivos-intranet/BuscarShapeTitulo/{id}"
                ext = "zip"
            else:
                url = f"https://monitoramento.semas.pa.gov.br/sisflora2/sisflora.api/Gf/VisualizarPdf/{id}"
                ext = "pdf"

            headers = {
                "Accept": "application/json, text/plain, */*",
                "Accept-Language": "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
                "Connection": "keep-alive",
                "Origin": "http://portaldatransparencia.semas.pa.gov.br",
                "Referer": "http://portaldatransparencia.semas.pa.gov.br/",
                "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
            }
            res = requests.request("GET", url, headers=headers)
            if not os.path.exists(f"{folder}/{id}.{ext}") and res.status_code != 500:
                with open(f"{folder}/{id}.{ext}", 'wb') as f:
                    f.write(res.content)
            if res.status_code == 500:
                sleep(5)
                self.get_zip(id,folder)
        except: pass

    def get_xlsx_info(self, i, url, d_i, d_f):
        headers = {
                "Accept": "application/json, text/plain, */*",
                "Accept-Language": "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
                "Connection": "keep-alive",
                "Referer": "http://portaldatransparencia.semas.pa.gov.br/",
                "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
            }

        xlsx = []
        querystring = {"page":"1", 'data_inicio': d_i, "data_fim": d_f}
        
        r = requests.request("GET", url,  headers=headers, params=querystring)
        try:
            for r in r.json()["data"]:
                try:
                    info = {
                        "No gf": r['numero'],
                        "Data Obtenção": r['data_emissao'],
                        "Situação": r['situacao'],
                        "CPF/CNPJ Procedência": r['origem_cpf_cnpj'],
                        "Nome/Razão Social Procedência": r['origem_nome'],
                        "CEPROF Procedência": r['origem_ceprof'],
                        "Município/UF Procedência": r['origem_municipio'] + ' / ' + r['origem_estado'],
                        "CPF/CNPJ Destino": r['destino_cpf_cnpj'],
                        "Nome/Razão Social Destino": r['destino_nome'],
                        "CEPROF Destino": r['destino_ceprof'],
                        "Município/UF Destino": r['destino_municipio'] + ' / ' + r['destino_estado'],
                        "Placa Veículo": r['placa']
                    }
                except:
                    pass
                xlsx.append(info)
                self.q.put(xlsx)

                self.lpx.append(i)
                with open("last_page_xlsx.obj", 'wb') as f:
                    pickle.dump(self.lpx, f)
        except: 
            sleep(10)
            self.get_xlsx_info(i, url, d_i, d_f)
        return None
        
    def get_xlsx(self, url, d_i, d_f):
        querystring = {"page":"1", 'data_inicio': d_i, "data_fim": d_f}
        r = requests.request("GET", url,  headers={"User-Agent":"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"}, params=querystring)
        l_page = int(r.json()["last_page"])+1
        #Mude o valor de l_page para trocar a página final que o programa irá terminar
        #l_page = 10
        with Pool(P_COUNT) as p:
                i = [x for x in list(range(0, l_page)) if x not in self.lpx]
                res = p.starmap(self.get_xlsx_info, zip(i, repeat(url), repeat(d_i), repeat(d_f)), chunksize=int(len(i)/4))
                for r in res:
                    pass
        
def save_xlsx(df, q):
    while True:
        xlsx = q.get()
        if xlsx != None:
            df = pd.DataFrame.from_dict(xlsx)
            df.drop_duplicates(inplace=True)
            df.to_csv("gf_xlsx.csv", mode='a',encoding="utf-8", index=False)

def main():
    s = Scraper()
    choice = None
    while choice != 0:
        choice = input("GOSTARIA DE FILTRAR O ANO? SE SIM ESCREVA O ALCANCE DOS ANOS, EXEMPLO: 2022-2023. CASO CONTRARIO DEIXE EM BRANCO: ")
        if choice != "":
            datas = choice.split("-")
            d_inicio = datas[0] + "-1-1"
            d_fim = datas[1] + "-1-1"
        else:
            d_inicio = "0"
            d_fim = "0"
        choice = input("QUAL INFORMAÇÃO DESEJA COLETAR? ESCOLHAS: gf, autef, pmf \n")
        s.df = pd.DataFrame()
        if choice == "autef":
            choice = input("Pegar também os produtos ou apenas as ações? (y/n): ")
            if choice == "y" or choice == "Y":
                prod = True
                try:
                    s.df = pd.read_csv("produtos.csv")
                except:
                    pass
            else:
                prod = False
            print("COLETANDO AUTEF")
            s.get_ids("http://portaldatransparencia.semas.pa.gov.br/portal-da-transparencia-api/api/v1/autef/", prod, d_inicio, d_fim)
            if not s.df.empty:
                s.df.to_csv("produtos.csv", index=False)
    
            with open("last_pages.obj", 'wb') as f:
                    if prod == True:
                        s.lp["autef_p"] = [1]
                    else:
                        s.lp["autef"] = [1]
                    pickle.dump(s.lp, f)

        elif choice == "pmf":
            print("COLETANDO PMF")
            prod = False
            s.get_ids("http://portaldatransparencia.semas.pa.gov.br/portal-da-transparencia-api/api/v1/pmf/", prod, d_inicio, d_fim)
        
            with open("last_pages.obj", 'wb') as f:
                    s.lp["pmf"] = [1]
                    pickle.dump(s.lp, f)

        elif choice == "gf":
            choice = input("Qual informação deseja coletar? (1- Ações; 2 - Xlsx; 3 - Produtos): ")

            if int(choice) == 1:
                choice = input("Pegar também os produtos ou apenas as ações? (y/n): ")
                if choice == "y" or choice == "Y":
                    prod = True
                    try:
                        s.df = pd.read_csv("gf_produtos.csv")
                    except:
                        pass
                else:
                    prod = False

                print("COLETANDO GF")
                s.get_ids("http://portaldatransparencia.semas.pa.gov.br/portal-da-transparencia-api/api/v1/guia-florestal/", prod, d_inicio, d_fim)
                if not s.df.empty:
                    s.df.to_csv("gf_produtos.csv", index=False)

                with open("last_pages.obj", 'wb') as f:
                    if prod == True:
                        s.lp["gf_p"] = [1]
                    else:
                        s.lp["gf"] = [1]
                    pickle.dump(s.lp, f)

            elif int(choice) == 2:
                print("COLETANDO GF XLSX")
                p = Pool(1)
                p.apply_async(save_xlsx, (s.df, s.q))
                s.get_xlsx("http://portaldatransparencia.semas.pa.gov.br/portal-da-transparencia-api/api/v1/guia-florestal/", d_inicio, d_fim)

                with open("last_page_xlsx.obj", 'wb') as f:
                    pickle.dump(s.lpx, f)
            
            elif int(choice) == 3:
                print("COLETANDO GF PRODUTOS")
                s.get_ids("http://portaldatransparencia.semas.pa.gov.br/portal-da-transparencia-api/api/v1/guia-florestal/", "only", d_inicio, d_fim)
                with open("last_pages.obj", 'wb') as f:
                    s.lp["gf_p"] = [1]
                    pickle.dump(s.lp, f)
        else:
            choice = 0

if __name__=="__main__":
    main()