import os
import pandas as pd
import pdfplumber
import matplotlib.pyplot as plt
import requests
import openpyxl

# =============================================================================
# Configurações Iniciais: diretórios e caminhos
# =============================================================================
DOWNLOAD_DIR = "download"
RESULT_DIR = "result"

# Cria os diretórios caso não existam
for folder in [DOWNLOAD_DIR, RESULT_DIR]:
    if not os.path.exists(folder):
        os.mkdir(folder)

download_path = DOWNLOAD_DIR
save_path = RESULT_DIR

# Caminhos dos arquivos PDF
diesel_file_path = os.path.join(download_path, "Tabelas de Preços - Diesel S500 e S10 01-02-25.pdf")
gasoline_file_path = os.path.join(download_path, "Tabelas de Preços - Gasolina 01-02-25.pdf")

# =============================================================================
# Funções Auxiliares para Processamento dos PDFs
# =============================================================================
def extract_tables_from_pdf(pdf_path):
    """Extrai tabelas de um PDF e define a primeira linha como nome das colunas."""
    tables = []
    with pdfplumber.open(pdf_path) as pdf:
        for i, page in enumerate(pdf.pages, start=1):
            extracted_tables = page.extract_tables()
            for j, table in enumerate(extracted_tables, start=1):
                df = pd.DataFrame(table)
                # Remove linhas vazias
                df.dropna(how='all', inplace=True)
                # Certifica-se de que o DataFrame não está vazio antes de processar
                if not df.empty and len(df) > 1:
                    df.columns = df.iloc[0]  # Usa a primeira linha como cabeçalho
                    df = df[1:].reset_index(drop=True)  # Remove a primeira linha
                    # Garante nomes de colunas únicos
                    df.columns = [f"Column_{k}" if not col or col in df.columns[:k] else col 
                                  for k, col in enumerate(df.columns)]
                    tables.append(df)
                print(f"{pdf_path} - Página {i}, Tabela {j} extraída")
    return tables

# =============================================================================
# Extração e Processamento dos Dados dos PDFs
# =============================================================================
diesel_tables = extract_tables_from_pdf(diesel_file_path)
gasoline_tables = extract_tables_from_pdf(gasoline_file_path)

diesel_df = pd.concat(diesel_tables, ignore_index=True) if diesel_tables else pd.DataFrame()
gasoline_df = pd.concat(gasoline_tables, ignore_index=True) if gasoline_tables else pd.DataFrame()

# Reformata os DataFrames (wide-to-long)
diesel_df = pd.melt(diesel_df, id_vars=['LOCAL', 'MODALIDADE\nDE VENDA'], value_name='PRECO')
diesel_df = diesel_df.rename(columns={'MODALIDADE\nDE VENDA': 'MODALIDADE DE VENDA', 'variable': 'DATA'})
gasoline_df = pd.melt(gasoline_df, id_vars=['LOCAL', 'MODALIDADE\nDE VENDA'], value_name='PRECO')
gasoline_df = gasoline_df.rename(columns={'MODALIDADE\nDE VENDA': 'MODALIDADE DE VENDA', 'variable': 'DATA'})

# Remove valores vazios
diesel_df.dropna(subset=['PRECO', 'LOCAL'], inplace=True)
diesel_df = diesel_df[diesel_df['PRECO'] != '']
gasoline_df.dropna(subset=['PRECO', 'LOCAL'], inplace=True)
gasoline_df = gasoline_df[gasoline_df['PRECO'] != '']

# Converte a coluna 'PRECO' para float (formato numérico brasileiro)
def convert_price(price):
    try:
        return float(price.replace('.', '').replace(',', '.'))
    except ValueError:
        return None

diesel_df['PRECO'] = diesel_df['PRECO'].apply(convert_price)
gasoline_df['PRECO'] = gasoline_df['PRECO'].apply(convert_price)

# Converte a coluna 'DATA' para formato de data (%d.%m.%Y)
def convert_date(date):
    try:
        return pd.to_datetime(date, format="%d.%m.%Y").strftime("%Y-%m-%d")
    except ValueError:
        return None

diesel_df['DATA'] = diesel_df['DATA'].apply(convert_date)
gasoline_df['DATA'] = gasoline_df['DATA'].apply(convert_date)

# =============================================================================
# Carregamento dos Dados Refinados (Excel) e Criação das Séries Temporais
# =============================================================================
# Carrega o arquivo Excel (Precos_Combustiveis.xlsx) que deve estar no diretório 'result'
output_path = os.path.join(save_path, "Precos_Combustiveis.xlsx")
diesel_df = pd.read_excel(output_path, sheet_name="Diesel")
gasoline_df = pd.read_excel(output_path, sheet_name="Gasolina")

# Converte a coluna 'DATA' para datetime
diesel_df['DATA'] = pd.to_datetime(diesel_df['DATA'])
gasoline_df['DATA'] = pd.to_datetime(gasoline_df['DATA'])

# =============================================================================
# Função para Criar Séries Temporais (sem plots individuais)
# =============================================================================
def create_series(df, fuel_type):
    series_dict = {}
    for modality in df['MODALIDADE DE VENDA'].unique():
        modality_df = df[df['MODALIDADE DE VENDA'] == modality]
        # Agrega por data para obter a média do preço por dia
        modality_df = modality_df.groupby('DATA')['PRECO'].mean().reset_index()
        # Ajusta a escala dividindo o preço por 1000
        modality_df['PRECO'] = modality_df['PRECO'] / 1000
        series_dict[f"{fuel_type}_{modality}"] = modality_df
        # Os plots individuais foram removidos para deixar apenas os plots dos dados merged.
    return series_dict

# Cria as séries para Diesel e Gasolina (sem gerar plots individuais)
diesel_series = create_series(diesel_df, "Diesel")
gasoline_series = create_series(gasoline_df, "Gasolina")
all_series = {**diesel_series, **gasoline_series}

# =============================================================================
# Dicionário das Variáveis de Cada Série
# =============================================================================
entregas_dict = {
    "EXA": "Ex-Ponto “A” - Entrega por duto a serviço da COMPRADORA, interligado ao Ponto “A” ou Flange de Interconexão dos Ativos, ambos dentro da Área Operacional da Unidade Fornecedora.",
    "LPA": "Livre para o Armazém - Entrega por duto, ou trecho de duto, a serviço da PETROBRAS, interligado a Flange de Interconexão dos Ativos fora da Área Operacional da Unidade Fornecedora.",
    "LCT": "Livre no Compartimento de Carga do Veículo Recebedor - Entrega diretamente no Veículo Recebedor estacionado junto à plataforma da Estação de Carregamento a serviço da PETROBRAS.",
    "LPC": "Livre para Carregamento - Entrega diretamente na Estação de Carregamento a serviço da COMPRADORA.",
    "ETM": "Entrega no Tanque de Destino Marítimo - Entrega no terminal de destino a partir de navio a serviço da PETROBRAS, com operação de descarga contratada pela PETROBRAS.",
    "LTM": "Livre no Terminal Marítimo - Entrega no terminal de destino a partir de navio a serviço da PETROBRAS, com operação de descarga contratada pela COMPRADORA.",
    "ETT": "Entregue no Terminal Terrestre - Entrega diretamente na Estação de Descarregamento a serviço da COMPRADORA a partir da descarga do Veículo de Entrega, ou diretamente no tanque recebedor, quando a entrega se der por duto a serviço da PETROBRAS.",
    "ETD": "Entrega no Tanque de Destino - Entrega do produto se dá mediante troca de propriedade no tanque da Base Recebedora.",
    "LPD": "Livre para Descarga - A entrega será feita por Veículo de Entrega estacionado na plataforma de descarga do terminal de destino a serviço da COMPRADORA."
}

# =============================================================================
# Download e Processamento dos Dados PPI (ANP)
# =============================================================================
ppi_url = "https://www.gov.br/anp/pt-br/assuntos/precos-e-defesa-da-concorrencia/precos/arq-ppi/ppi.xlsx"
ppi_file_path = os.path.join(DOWNLOAD_DIR, "ppi.xlsx")

response = requests.get(ppi_url)
if response.status_code == 200:
    with open(ppi_file_path, 'wb') as file:
        file.write(response.content)
    print("Download concluído com sucesso!")
else:
    print("Erro ao baixar o arquivo PPI:", response.status_code)

xls = pd.ExcelFile(ppi_file_path, engine='openpyxl')
gasolina_ppi = pd.read_excel(xls, sheet_name="Gasolina R$ semanal", skiprows=2, engine='openpyxl')
diesel_ppi = pd.read_excel(xls, sheet_name="Diesel R$ semanal", skiprows=2, engine='openpyxl')

# Configura "Data" como índice e remove colunas desnecessárias (Unnamed)
gasolina_ppi = gasolina_ppi.set_index("Data").drop(columns=[col for col in gasolina_ppi.columns if "Unnamed" in col])
diesel_ppi = diesel_ppi.set_index("Data").drop(columns=[col for col in diesel_ppi.columns if "Unnamed" in col])

# Verifica a estrutura (opcional)
print("Gasolina PPI (últimas linhas):")
print(gasolina_ppi.tail())
print("\nDiesel PPI (primeiras linhas):")
print(diesel_ppi.head())

def extract_start_date(date_str):
    """Extrai a data inicial de uma string no formato 'DD/MM/YYYY A DD/MM/YYYY'."""
    try:
        return date_str.split(" A ")[0]
    except Exception:
        return None

# Processa os dados PPI para Gasolina
gasolina_ppi["start_date"] = gasolina_ppi.index.to_series().apply(extract_start_date)
gasolina_ppi["start_date"] = pd.to_datetime(gasolina_ppi["start_date"], format="%d/%m/%Y", errors="coerce")
gasolina_ppi = gasolina_ppi.dropna(subset=["start_date"])
gasolina_ppi.sort_values("start_date", inplace=True)

# Processa os dados PPI para Diesel
diesel_ppi["start_date"] = diesel_ppi.index.to_series().apply(extract_start_date)
diesel_ppi["start_date"] = pd.to_datetime(diesel_ppi["start_date"], format="%d/%m/%Y", errors="coerce")
diesel_ppi = diesel_ppi.dropna(subset=["start_date"])
diesel_ppi.sort_values("start_date", inplace=True)

# =============================================================================
# Merge dos Dados EXA com os Dados PPI
# =============================================================================
# Prepara as séries EXA (assumindo que a chave "Gasolina_EXA" e "Diesel_EXA" existam em all_series)
gasolina_exa = all_series["Gasolina_EXA"].copy()
diesel_exa = all_series["Diesel_EXA"].copy()
gasolina_exa["DATA"] = pd.to_datetime(gasolina_exa["DATA"])
diesel_exa["DATA"] = pd.to_datetime(diesel_exa["DATA"])
gasolina_exa.sort_values("DATA", inplace=True)
diesel_exa.sort_values("DATA", inplace=True)

merged_gasolina = pd.merge_asof(
    gasolina_ppi,
    gasolina_exa,
    left_on="start_date",
    right_on="DATA",
    direction="backward"
)
merged_diesel = pd.merge_asof(
    diesel_ppi,
    diesel_exa,
    left_on="start_date",
    right_on="DATA",
    direction="backward"
)

print("Merged Gasolina Data (first 5 rows):")
print(merged_gasolina.head())
print("\nMerged Diesel Data (first 5 rows):")
print(merged_diesel.head())

# Remove colunas duplicadas que terminem com '.1'
merged_gasolina = merged_gasolina.loc[:, ~merged_gasolina.columns.str.endswith(".1")]
merged_diesel = merged_diesel.loc[:, ~merged_diesel.columns.str.endswith(".1")]

# Configura 'start_date' como o índice e remove a coluna 'DATA'
merged_gasolina.set_index("start_date", inplace=True)
merged_diesel.set_index("start_date", inplace=True)
merged_gasolina.drop(columns=["DATA"], inplace=True)
merged_diesel.drop(columns=["DATA"], inplace=True)

# Filtra os dados a partir de 1º de abril de 2023 e reamostra semanalmente
merged_gasolina.index = pd.to_datetime(merged_gasolina.index)
merged_diesel.index = pd.to_datetime(merged_diesel.index)
merged_gasolina = merged_gasolina.loc[merged_gasolina.index >= "2023-04-01"]
merged_diesel = merged_diesel.loc[merged_diesel.index >= "2023-04-01"]
merged_gasolina = merged_gasolina.resample("W").ffill()
merged_diesel = merged_diesel.resample("W").ffill()

print("\nMerged Gasolina (após filtro e resample):")
print(merged_gasolina.head())
print("\nMerged Diesel (após filtro e resample):")
print(merged_diesel.head())

# =============================================================================
# Plot dos Dados Merged
# =============================================================================
def plot_merged_data(merged_df, fuel_type):
    """
    Plota os dados mergeados para cada coluna numérica.
    """
    plt.figure(figsize=(14, 7))
    numeric_cols = merged_df.select_dtypes(include=['number']).columns
    for col in numeric_cols:
        plt.plot(merged_df.index, merged_df[col], label=col)
    plt.title(f"Merged {fuel_type} Data (2020 and Beyond) - Line Plot")
    plt.xlabel("Date")
    plt.ylabel("Values")
    plt.grid(True)
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.show()

# Plota os dados merged para Gasolina e Diesel
plot_merged_data(merged_gasolina, "Gasolina")
plot_merged_data(merged_diesel, "Diesel")


# plot defasagens
def plot_defasagem_by_ppi_final(merged_df, fuel_type):
    """
    Para cada coluna de PPI (colunas numéricas exceto 'PRECO'),
    calcula a defasagem percentual em relação ao Preço Petrobras usando:
    
         defasagem (%) = ((PRECO - PPI) / PRECO) * 100
         
    Plota todas as séries no mesmo gráfico (sem legendas). 
    Em seguida, dentre as últimas observações de cada série, identifica-se
    aquela cujo valor tem o menor módulo (ou seja, está mais próximo de zero)
    e anota somente esse valor na última data.
    
    Parâmetros:
      - merged_df: DataFrame com a coluna 'PRECO' e as demais colunas numéricas representando os PPIs.
      - fuel_type: String com o tipo de combustível (ex.: "Gasolina" ou "Diesel").
    """
    plt.style.use("seaborn-darkgrid")
    plt.figure(figsize=(14, 7))
    
    # Seleciona as colunas numéricas que correspondem aos PPIs (exceto 'PRECO')
    numeric_cols = merged_df.select_dtypes(include=['number']).columns
    ppi_cols = [col for col in numeric_cols if col != 'PRECO']
    
    if not ppi_cols:
        print("Nenhuma coluna de PPI encontrada!")
        return
    
    # Dicionário para armazenar a série de defasagem de cada coluna
    defasagem_dict = {}
    # Dicionário para armazenar o valor final (última observação) de cada série
    final_values = {}
    
    # Para cada coluna de PPI, calcula a série de defasagem e plota
    for col in ppi_cols:
        # Calcula a série: defasagem = ((PRECO - PPI) / PRECO) * 100
        defasagem_ts = (merged_df['PRECO'] - merged_df[col]) / merged_df['PRECO'] * 100
        defasagem_dict[col] = defasagem_ts
        final_values[col] = defasagem_ts.iloc[-1]
        plt.plot(merged_df.index, defasagem_ts, linewidth=2)
    
    # Entre as últimas observações de cada série, identifica aquela cujo valor absoluto é o menor
    chosen_col = min(final_values, key=lambda c: abs(final_values[c]))
    chosen_value = final_values[chosen_col]
    
    # Define a posição do label: na última data da série escolhida
    last_date = merged_df.index[-1]
    plt.scatter(last_date, defasagem_dict[chosen_col].iloc[-1], s=100, color="red", zorder=5)
    plt.annotate(f"{chosen_value:.2f}%", (last_date, defasagem_dict[chosen_col].iloc[-1]),
                 textcoords="offset points", xytext=(0,10), ha="center", color="red")
    
    # Linha de referência em zero
    plt.axhline(0, color="black", linestyle="--", linewidth=1)
    
    # Configurações do gráfico
    plt.title(f"Defasagem Percentual por PPI - {fuel_type}\n(Preço Petrobras vs. Cada PPI)", fontsize=16)
    plt.xlabel("Data", fontsize=14)
    plt.ylabel("Defasagem (%)", fontsize=14)
    plt.xticks(fontsize=12, rotation=45)
    plt.yticks(fontsize=12)
    # Não adicionamos legendas
    plt.tight_layout()
    plt.show()

# Exemplo de chamada (supondo que 'merged_gasolina' e 'merged_diesel' já estejam filtrados para datas a partir de 1º de abril de 2023 e reamostrados semanalmente):
plot_defasagem_by_ppi_final(merged_gasolina, "Gasolina")
plot_defasagem_by_ppi_final(merged_diesel, "Diesel")




































