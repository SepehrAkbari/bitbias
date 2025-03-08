import os
from qiskit import QuantumCircuit
from qiskit_aer import Aer
from qiskit import transpile
import pandas as pd
from math import ceil, log2

def generateBit() -> int:
    qc = QuantumCircuit(1, 1)
    qc.h(0)
    qc.measure(0, 0)

    backend = Aer.get_backend('qasm_simulator')
    transpiled_qc = transpile(qc, backend)
    job = backend.run(transpiled_qc)
    result = job.result()

    counts = result.get_counts()
    measured_result = int(list(counts.keys())[0])

    return measured_result

def generateInt(max_val: int):
    nBits = ceil(log2(max_val + 1))
    
    while True:
        bits = [generateBit() for _ in range(nBits)]
        random_number = sum(bit * (2 ** i) for i, bit in enumerate(reversed(bits)))

        if random_number <= max_val:
            return random_number
        
def createDf(arr, max_val: int, trial_n: int, filename: str):
    for i in range(trial_n):
        n = generateInt(max_val)
        arr.append({'index': i, 'n': n})

    df = pd.DataFrame(arr)
    df.to_csv(filename, index=False)

        
def main():
    print('Starting...')

    n_datasets = 10
    max_val = 10
    trial_n = 500

    data_folder = "Data"
    os.makedirs(data_folder, exist_ok=True)

    for i in range(n_datasets):
        print(f'Generating dataset {i+21}...')
        arr = []
        filename = os.path.join(data_folder, f'randoms-{i+1}.csv')
        createDf(arr, max_val, trial_n, filename)
    
    print('\nDone!')

if __name__ == '__main__':
    main()