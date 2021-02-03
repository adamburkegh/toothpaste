# toothpaste

This project shows a technique for discovering Stochastic Petri Nets from event logs using process mining.

The paper describing the technique is "Burke, A, Leemans, SJJ and Wynn, M.T - Discovering Stochastic Process Models By Reduction and Abstraction" (forthcoming)


# Development Setup and Installation

## Haskell Stack

`stack test`

## Gradle and Java

Checkout `prom-helpers`
In prom-helpers
`./gradlew test`
`./gradlew publishToMavenLocal`

In toothpaste
`./gradlew test`

## Python

`python -m pip install -r requirements.txt`

## Graphviz

Requires installing graphviz, with the dot graphing tool to be on the PATH, for the graph rendering steps.

If you have chocolatey

`choco install graphviz`

Then run `dot -c` to initialize

(It's a [weird graphviz thing](https://stackoverflow.com/a/62549025/5729872))



# Running
`scripts/red.sh data/exercise1.xes` invokes the Toothpaste Miner prototype, outputting `var/exercise1.png` and various intermediates.

`scripts/st.sh exercise` to just invoke the miner on a DCDT file and output a process tree.

The test scaffold entry point is `ModelRunner.java`

The reporting entry point is `SPNDiscoverReporter.java`

Some scripts for running on Windows and Unix are in `scripts/`.

# Results

Result files from experiments performed on this framework are in `results`. "Weight estimation" mining was done using the [spd\_we project](https://github.com/adamburkegh/spd_we). This includes output models 


