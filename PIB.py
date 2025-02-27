# ╔════════════════════════════════════════════════════════════════════╗
# ║                     Tabelas do PIB                                 ║
# ║    Esse código faz o download e trata a tabela 5932 do             ║
# ║    PIB, extraindo séries temporais para as variáveis e             ║
# ║    setores definidos, simplificando as subdivisões dos             ║
# ║    setores do lado da oferta quando disponível o total.            ║
# ║                                                                    ║
# ║    Variáveis:                                                      ║
# ║      - Taxa trimestral (em relação ao mesmo período do ano)        ║
# ║      - Taxa acumulada em quatro trimestres                         ║
# ║      - Taxa acumulada ao longo do ano                              ║
# ║      - Taxa trimestre contra trimestre imediatamente anterior      ║
# ║                                                                    ║
# ║    Setores de DEMANDA:                                             ║
# ║      • PIB a preços de mercado                                     ║
# ║      • Despesa de consumo das famílias                             ║
# ║      • Despesa de consumo da administração pública                 ║
# ║      • Formação bruta de capital fixo                              ║
# ║                                                                    ║
# ║    Setores de OFERTA:                                              ║
# ║      • Agropecuária - total                                        ║
# ║      • Indústria - total                                           ║
# ║      • Indústrias extrativas                                       ║
# ║      • Indústrias de transformação                                 ║
# ║      • Eletricidade e gás, água, esgoto, atividades de gestão      ║
# ║        de resíduos                                                 ║
# ║      • Construção                                                  ║
# ║      • Serviços - total                                            ║
# ║      • Comércio                                                    ║
# ║      • Transporte, armazenagem e correio                           ║
# ║      • Informação e comunicação                                    ║
# ║      • Atividades financeiras, de seguros e serviços relacionados  ║
# ║      • Atividades imobiliárias                                     ║
# ║      • Outras atividades de serviços                               ║
# ║      • Administração, saúde e educação públicas e seguridade social║
# ║      • Valor adicionado a preços básicos                           ║
# ║      • Impostos líquidos sobre produtos                            ║
# ║                                                                    ║
# ║      Author: Lucas Gabriel Martins de Oliveira                     ║
# ║      Data:   2025-02-27                                            ║
# ╚════════════════════════════════════════════════════════════════════╝

import requests
import pandas as pd
import pprint
import re

# URL da API do IBGE
link = "https://apisidra.ibge.gov.br/values/t/5932/n1/all/v/all/p/all/c11255/all/d/v6561%201,v6562%201,v6563%201,v6564%201"
response = requests.get(link)

if response.status_code == 200:
    data = response.json()
    # O primeiro elemento contém os nomes das colunas
    header = data[0]
    df = pd.DataFrame(data[1:], columns=header)
    print("DataFrame inicial:")
    print(df.head())
else:
    print("Erro na requisição, status code:", response.status_code)

# Função para converter a string de trimestre em um objeto pd.Period
def convert_trimestre(trimestre_str):
    match = re.search(r'(\d)º trimestre (\d+)', trimestre_str)
    if match:
        quarter = int(match.group(1))
        year = int(match.group(2))
        return pd.Period(year=year, quarter=quarter, freq='Q')
    else:
        return pd.NaT

# Cria a coluna 'Periodo' convertendo os trimestres
df['Periodo'] = df['D3N'].apply(convert_trimestre)

# Remove linhas onde a conversão falhou
df = df.dropna(subset=['Periodo'])

# Converte a coluna de valores para numérico
df['Valor'] = pd.to_numeric(df['V'], errors='coerce')

# Extrai as combinações únicas de variável e setor
unique_series = df[['D2N', 'D4N']].drop_duplicates()
# Opcional: salve para CSV se necessário
unique_series.to_csv('unique_series.csv', index=False)

# Cria o dicionário original de séries temporais (chave = "Variável - Setor")
ts_dict = {}
for index, row in unique_series.iterrows():
    variable = row['D2N']
    sector = row['D4N']
    
    df_subset = df[(df['D2N'] == variable) & (df['D4N'] == sector)].copy()
    df_subset = df_subset.sort_values(by='Periodo')
    
    ts = pd.Series(data=df_subset['Valor'].values, index=df_subset['Periodo'])
    key = f"{variable} - {sector}"
    ts_dict[key] = ts

# Definição dos setores por lado:
demand_side = [
    "PIB a preços de mercado",
    "Despesa de consumo das famílias",
    "Despesa de consumo da administração pública",
    "Formação bruta de capital fixo"
]

offer_side = [
    "Agropecuária - total",
    "Indústria - total",
    "Indústrias extrativas",
    "Indústrias de transformação",
    "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos",
    "Construção",
    "Serviços - total",
    "Comércio",
    "Transporte, armazenagem e correio",
    "Informação e comunicação",
    "Atividades financeiras, de seguros e serviços relacionados",
    "Atividades imobiliárias",
    "Outras atividades de serviços",
    "Administração, saúde e educação públicas e seguridade social",
    "Valor adicionado a preços básicos",
    "Impostos líquidos sobre produtos"
]

# Simplifica as séries conforme as regras:
# 1. Se for demanda, mantemos a série.
# 2. Se for oferta e o setor tiver subdivisões, mantemos apenas a série com "total" se existir;
#    caso não exista, mantemos as subdivisões.
simplified_ts_dict = {}

# Primeiro, inclui todas as séries de demanda
for key, series in ts_dict.items():
    variable, sector = key.split(" - ", 1)
    if sector in demand_side:
        simplified_ts_dict[key] = series

# Para o lado da oferta, agrupa as séries por variável
offer_group = {}
for key, series in ts_dict.items():
    variable, sector = key.split(" - ", 1)
    if sector in offer_side:
        offer_group.setdefault(variable, {})[sector] = series

# Para cada variável do lado da oferta, verifica se há uma série "total"
for variable, subdict in offer_group.items():
    # Verifica se existe um setor com a palavra "total"
    total_sector = None
    for sector in subdict.keys():
        if "total" in sector.lower():
            total_sector = sector
            break
    if total_sector:
        key = f"{variable} - {total_sector}"
        simplified_ts_dict[key] = subdict[total_sector]
    else:
        # Se não houver total, mantém todas as subdivisões
        for sector, series in subdict.items():
            key = f"{variable} - {sector}"
            simplified_ts_dict[key] = series

# Exibe as chaves das séries simplificadas
print("\nChaves das séries temporais simplificadas:")
for key in simplified_ts_dict.keys():
    print(key)

# Exemplo: imprime as primeiras linhas de algumas séries simplificadas
for key, series in simplified_ts_dict.items():
    print("\nTime Series:", key)
    print(series.head())

