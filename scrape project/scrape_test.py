# -*- coding: utf-8 -*-
"""
Created on Thu Oct 30 16:01:56 2025

@author: rorya
"""

import pandas as pd
from bs4 import BeautifulSoup
import requests

# %%

url = "https://en.wikipedia.org/wiki/List_of_best-selling_game_consoles"

# Obtener el permiso para acceder
headers = {"User-Agent": "MyResearchBot/1.0 (contact: raaldrettes@colmex.mx)"}

# Poner la página y jalar su código
page = requests.get(url, headers=headers)

# Que lea el html
soup = BeautifulSoup(page.text, 'html')


"""
    Parece que soup lo que hace es crear una entidad de página que puedes mani-
pular. Puedes escoger partes de esa página. Abajo encontré la tabla que quería
y después le saqué los títulos

Para ubicarnos:
    - table para tablas
    - th para headers
    - tr filas
    - td datos

al usar find_all() con estos obtenemos listas de cada una de estas, los elemen-
tos de los q está conformado son parte del elemento en ¿string?
"""


# %%

# Buscar las tablas y agarrar la q interesa
table = soup.find_all("table")[0]

# Buscar los títulos
titles = table.find_all('th')

table_titles = [title.text.strip() for title in titles][0:5]


# %%

# Ahora hacer la tabla

df = pd.DataFrame(columns=table_titles)
df


# %%

# Obtenemos las filas
rows = table.find_all('tr')

# De cada fila extraemos el dato
for row in rows[1:]:
    # Extraemos los datos
    row_i = row.find_all('td')
    # Extraemos los datos que están en formato 'th'
    f_elem_r = row.find_all('th')
    # Hacemos list comprehension y sacamos cada fila en lista
    f_elem = [f.text.strip() for f in f_elem_r]
    full_row = [r.text.strip() for r in row_i]
    # Los juntamos pq no se mezclan
    end_row = f_elem + full_row
    # Quitamos el último
    end_row = end_row[0:5]
    
    # Añadimos al dataframe
    n = len(df)
    df.loc[n] = end_row

# %%

df.to_csv(r'C:\Users\rorya\Desktop\COLMEX\TESIS\po\Scripts\scrape project\scrape_test\test.csv',
          index=False)    





# %%

print(table)


