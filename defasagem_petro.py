import pandas as pd
import pdfplumber
import os
import matplotlib.pyplot as plt

# Define os diretórios para download e resultado
if not os.path.exists('download'):
    os.mkdir('download')

if not os.path.exists('result'):
    os.mkdir('result')

download_path = 'download'
save_path = 'result'

# Caminhos dos arquivos PDF
diesel_file_path = os.path.join(download_path, "Tabelas de Preços - Diesel S500 e S10 01-02-25.pdf")
gasoline_file_path = os.path.join(download_path, "Tabelas de Preços - Gasolina 01-02-25.pdf")

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
                    df.columns = df.iloc[0]  # Usa a primeira linha como nome das colunas
                    df = df[1:].reset_index(drop=True)  # Remove a primeira linha
                    
                    # Garante nomes de colunas únicos
                    df.columns = [f"Column_{k}" if not col or col in df.columns[:k] else col for k, col in enumerate(df.columns)]
                    
                    tables.append(df)

                # Mensagem de log
                print(f"{pdf_path} - Página {i}, Tabela {j} extraída")
    return tables

# Extrai tabelas dos PDFs
diesel_tables = extract_tables_from_pdf(diesel_file_path)
gasoline_tables = extract_tables_from_pdf(gasoline_file_path)

# Junta as tabelas em um único DataFrame por categoria
diesel_df = pd.concat(diesel_tables, ignore_index=True) if diesel_tables else pd.DataFrame()
gasoline_df = pd.concat(gasoline_tables, ignore_index=True) if gasoline_tables else pd.DataFrame()

# Reformata os DataFrames
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
        return pd.to_datetime(date, format="%d.%m.%Y").strftime("%Y-%m-%d")  # Ajusta para o formato YYYY-MM-DD
    except ValueError:
        return None

diesel_df['DATA'] = diesel_df['DATA'].apply(convert_date)
gasoline_df['DATA'] = gasoline_df['DATA'].apply(convert_date)

# Separar séries
# Define the path to the Excel file - arquivo da anp de ppi tem que estar no
# diretório.

save_path = 'result'
output_path = os.path.join(save_path, "Precos_Combustiveis.xlsx")

# Load the Excel file
diesel_df = pd.read_excel(output_path, sheet_name="Diesel")
gasoline_df = pd.read_excel(output_path, sheet_name="Gasolina")

# Convert 'DATA' to datetime format
diesel_df['DATA'] = pd.to_datetime(diesel_df['DATA'])
gasoline_df['DATA'] = pd.to_datetime(gasoline_df['DATA'])

# Function to create separate time series per "MODALIDADE DE VENDA" and plot them
def create_series(df, fuel_type):
    series_dict = {}

    for modality in df['MODALIDADE DE VENDA'].unique():
        modality_df = df[df['MODALIDADE DE VENDA'] == modality]

        # Aggregate by date to get the average price per day
        modality_df = modality_df.groupby('DATA')['PRECO'].mean().reset_index()

        # Divide PRECO by 1000
        modality_df['PRECO'] = modality_df['PRECO'] / 1000

        # Store the series in a dictionary
        series_dict[f"{fuel_type}_{modality}"] = modality_df

        # Plot the series
        plt.figure(figsize=(10, 5))
        plt.plot(modality_df['DATA'], modality_df['PRECO'], marker='o', linestyle='-')
        plt.title(f"{fuel_type} - {modality}")
        plt.xlabel("Data")
        plt.ylabel("Preço (R$/L)")
        plt.grid(True)
        plt.xticks(rotation=45)
        plt.show()

    return series_dict

# Create and plot time series for Diesel and Gasoline
diesel_series = create_series(diesel_df, "Diesel")
gasoline_series = create_series(gasoline_df, "Gasolina")

# Combine both series dictionaries
all_series = {**diesel_series, **gasoline_series}


# Dicionário das variáveis de cada série
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

# carregar dados PPI - ANP
import requests
import pandas as pd
import openpyxl

# checar se o link não quebra
url = "https://www.gov.br/anp/pt-br/assuntos/precos-e-defesa-da-concorrencia/precos/arq-ppi/ppi.xlsx"
# Caminho local para salvar o arquivo temporariamente
file_path = r"U:/Pastas pessoais/LucasM/Python/AcompanhamentoDefasagemPetrobras/download/ppi.xlsx"
# Fazer o download do arquivo e salvar localmente
response = requests.get(url)
if response.status_code == 200:
    with open(file_path, 'wb') as file:
        file.write(response.content)
    print("Download concluído com sucesso!")
else:
    print("Erro ao baixar o arquivo:", response.status_code)
# Carregar o arquivo Excel usando openpyxl
xls = pd.ExcelFile(file_path, engine='openpyxl')

# Read the sheets, skipping first two rows and selecting "Data" as the index
gasolina_ppi = pd.read_excel(xls, sheet_name="Gasolina R$ semanal", skiprows=2, engine='openpyxl')
diesel_ppi = pd.read_excel(xls, sheet_name="Diesel R$ semanal", skiprows=2, engine='openpyxl')

# Set "Data" as index and drop unnamed columns
gasolina_ppi = gasolina_ppi.set_index("Data").drop(columns=[col for col in gasolina_ppi.columns if "Unnamed" in col])
diesel_ppi = diesel_ppi.set_index("Data").drop(columns=[col for col in diesel_ppi.columns if "Unnamed" in col])

# checar estrutura
gasolina_ppi.tail()
diesel_ppi.head()

# agora é necessário fazer o merge dos dataframes
# -----------------------------
# MERGE EXA Series with PPI Data (with null removal)
# -----------------------------

# Helper function to extract the start date from strings like "18/11/2024 A 22/11/2024"
def extract_start_date(date_str):
    try:
        return date_str.split(" A ")[0]
    except Exception:
        return None

# For Gasolina PPI: extract the start date from the index
gasolina_ppi["start_date"] = gasolina_ppi.index.to_series().apply(extract_start_date)
gasolina_ppi["start_date"] = pd.to_datetime(gasolina_ppi["start_date"], format="%d/%m/%Y", errors="coerce")

# Drop rows with null start_date values
gasolina_ppi = gasolina_ppi.dropna(subset=["start_date"])
gasolina_ppi.sort_values("start_date", inplace=True)

# For Diesel PPI: do the same
diesel_ppi["start_date"] = diesel_ppi.index.to_series().apply(extract_start_date)
diesel_ppi["start_date"] = pd.to_datetime(diesel_ppi["start_date"], format="%d/%m/%Y", errors="coerce")
diesel_ppi = diesel_ppi.dropna(subset=["start_date"])
diesel_ppi.sort_values("start_date", inplace=True)

# Prepare the EXA series from your dictionary (assumed to have columns "DATA" and "PRECO")
# Convert the EXA series date column to datetime
gasolina_exa = all_series["Gasolina_EXA"].copy()
diesel_exa = all_series["Diesel_EXA"].copy()
gasolina_exa["DATA"] = pd.to_datetime(gasolina_exa["DATA"])
diesel_exa["DATA"] = pd.to_datetime(diesel_exa["DATA"])

# Sort the EXA data by date
gasolina_exa.sort_values("DATA", inplace=True)
diesel_exa.sort_values("DATA", inplace=True)

# Merge using pd.merge_asof:
# This will, for each PPI week (using the start_date), assign the most recent EXA price available
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

# Display the first few rows of each merged DataFrame
print("Merged Gasolina Data (first 5 rows):")
print(merged_gasolina.head())
print("\nMerged Diesel Data (first 5 rows):")
print(merged_diesel.head())

# exclude all .1 columns at the final
merged_gasolina = merged_gasolina.loc[:, ~merged_gasolina.columns.str.endswith(".1")]
merged_diesel = merged_diesel.loc[:, ~merged_diesel.columns.str.endswith(".1")]

# Set 'start_date' as the index
merged_gasolina.set_index("start_date", inplace=True)
merged_diesel.set_index("start_date", inplace=True)

# Drop DATA column
merged_gasolina.drop(columns=["DATA"], inplace=True)
merged_diesel.drop(columns=["DATA"], inplace=True)

# filter for 2020 and beyond
merged_gasolina.index = pd.to_datetime(merged_gasolina.index)
merged_diesel.index = pd.to_datetime(merged_diesel.index)

# Filter data for 2020 and beyond
merged_gasolina = merged_gasolina.loc[merged_gasolina.index >= "2020-01-01"]
merged_diesel = merged_diesel.loc[merged_diesel.index >= "2020-01-01"]

# Resample weekly (if necessary)
merged_gasolina = merged_gasolina.resample("W").ffill()  # Fill missing weeks forward
merged_diesel = merged_diesel.resample("W").ffill()

# Display results
print(merged_gasolina.head())
print(merged_diesel.head())

# -------------------------------
# Visualizing Merged Gasolina Data with Lines Only
# -------------------------------
plt.figure(figsize=(14, 7))

# Automatically select numeric columns for plotting
numeric_cols_gasolina = merged_gasolina.select_dtypes(include=['number']).columns

# Plot each numeric column as a separate line (without markers)
for col in numeric_cols_gasolina:
    plt.plot(merged_gasolina.index, merged_gasolina[col], label=col)

plt.title("Merged Gasolina Data (2020 and Beyond) - Line Plot")
plt.xlabel("Date")
plt.ylabel("Values")
plt.legend()
plt.grid(True)
plt.xticks(rotation=45)
plt.tight_layout()
plt.show()

# -------------------------------
# Visualizing Merged Diesel Data with Lines Only
# -------------------------------
plt.figure(figsize=(14, 7))

# Automatically select numeric columns for plotting
numeric_cols_diesel = merged_diesel.select_dtypes(include=['number']).columns

# Plot each numeric column as a separate line (without markers)
for col in numeric_cols_diesel:
    plt.plot(merged_diesel.index, merged_diesel[col], label=col)

plt.title("Merged Diesel Data (2020 and Beyond) - Line Plot")
plt.xlabel("Date")
plt.ylabel("Values")
plt.legend()
plt.grid(True)
plt.xticks(rotation=45)
plt.tight_layout()
plt.show()


# CALCULAR E PLOTAR DEFASAGEM
def plot_avg_defasagem_refineries(merged_df, fuel_type):
    """
    Calcula e plota a defasagem percentual média para cada refinaria.
    
    Parâmetros:
      - merged_df: DataFrame com os dados mesclados que deve conter:
          • Colunas com os nomes das refinarias (ex.: "Manaus", "Itaqui", etc.)
          • A coluna "PRECO" (preço EXA)
          • Outras colunas derivadas que serão ignoradas: "defasagem_pct", "ppi_media", "Geral"
      - fuel_type: String indicando o tipo de combustível (ex.: "Diesel" ou "Gasolina")
    """
    # Definindo as colunas que NÃO representam refinarias
    exclude_cols = ['PRECO', 'defasagem_pct', 'ppi_media', 'Geral']
    # Seleciona as colunas que representam refinarias
    refinery_cols = [col for col in merged_df.columns if col not in exclude_cols]
    
    # Dicionário para armazenar a defasagem média de cada refinaria
    defasagem_dict = {}
    
    for ref in refinery_cols:
        # Cálculo da defasagem para cada linha:
        # (PRECO - valor da refinaria) / PRECO * 100
        # Assim, se o valor da refinaria for maior que o PRECO, a defasagem será negativa.
        df_temp = (merged_df['PRECO'] - merged_df[ref]) / merged_df['PRECO'] * 100
        mean_defasagem = df_temp.mean()
        # Remove espaços em branco no nome (se houver)
        defasagem_dict[ref.strip()] = mean_defasagem
    
    # Ordena as refinarias (opcional)
    refinarias = list(defasagem_dict.keys())
    valores = list(defasagem_dict.values())
    
    # Cria o gráfico de barras
    plt.figure(figsize=(12, 6))
    plt.bar(refinarias, valores, color='skyblue')
    plt.axhline(0, color='black', linestyle='--', linewidth=1)
    plt.title(f"Defasagem Percentual Média por Refinaria ({fuel_type})")
    plt.xlabel("Refinaria")
    plt.ylabel("Defasagem Média (%)")
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.show()

# Exemplo de chamada para Diesel e Gasolina:
plot_avg_defasagem_refineries(merged_diesel, "Diesel")
plot_avg_defasagem_refineries(merged_gasolina, "Gasolina")





import matplotlib.pyplot as plt

def plot_avg_defasagem_series(merged_df, fuel_type):
    """
    Calcula a defasagem percentual média (usando a média de todas as refinarias) e plota a série temporal.
    
    Parâmetros:
      - merged_df: DataFrame que deve conter:
          • Colunas com os nomes das refinarias (ex.: "Manaus", "Itaqui", "Suape", etc.)
          • A coluna "PRECO" (preço EXA)
          • Colunas derivadas (como 'defasagem_pct', 'ppi_media', 'Geral') serão ignoradas.
      - fuel_type: String indicando o tipo de combustível (ex.: "Diesel" ou "Gasolina")
    """
    # Colunas que NÃO serão consideradas como refinarias
    exclude_cols = ['PRECO', 'defasagem_pct', 'ppi_media', 'Geral']
    # Seleciona as colunas que correspondem às refinarias
    refinery_cols = [col for col in merged_df.columns if col not in exclude_cols]
    
    if not refinery_cols:
        print("Nenhuma coluna de refinaria encontrada.")
        return

    # Calcula a média dos PPIs das refinarias para cada linha (data)
    merged_df['ppi_refinery_mean'] = merged_df[refinery_cols].mean(axis=1)
    
    # Calcula a defasagem percentual média:
    # Se o PPI médio estiver acima do PRECO, o resultado será negativo.
    merged_df['defasagem_media'] = (merged_df['PRECO'] - merged_df['ppi_refinery_mean']) / merged_df['PRECO'] * 100
    
    # Cria o gráfico de série temporal
    plt.figure(figsize=(14, 7))
    plt.plot(merged_df.index, merged_df['defasagem_media'], label=f'Defasagem Média ({fuel_type})', color='blue')
    plt.axhline(0, color='black', linestyle='--', linewidth=1)
    
    # Destaca o último ponto da série
    last_date = merged_df.index[-1]
    last_value = merged_df['defasagem_media'].iloc[-1]
    plt.scatter(last_date, last_value, color='red', s=100, zorder=5, label='Último Dado')
    plt.annotate(f'{last_value:.2f}%', (last_date, last_value),
                 textcoords="offset points", xytext=(0,10), ha='center', color='red')
    
    plt.title(f"Defasagem Percentual Média (todas refinarias) - {fuel_type}")
    plt.xlabel("Data")
    plt.ylabel("Defasagem (%)")
    plt.legend()
    plt.grid(True)
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.show()

# Exemplo de chamada para Diesel e Gasolina (assegurando que o índice do DataFrame seja datetime)
plot_avg_defasagem_series(merged_diesel, "Diesel")
plot_avg_defasagem_series(merged_gasolina, "Gasolina")



