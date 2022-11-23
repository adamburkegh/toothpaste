# toothpaste

This project shows a technique for discovering Stochastic Petri Nets from event logs using process mining.

The technique is explained formally in *Burke, A., Leemans, S. J. J., & Wynn, Moe Thandar. (2021). Discovering Stochastic Process Models By Reduction and Abstraction. International conference on applications and theory of Petri nets and concurrency*. [This blog post](https://adamburkeware.net/2021/08/02/toothpaste.html) gives a quick overview.


# Development Setup and Installation

## Build All
`./build.sh` runs the complete build.

Though it should not be necessary to run individual component builds, they are listed below to help troubleshoot if there are problems.

## Haskell Stack

`stack test`

## Gradle and Java

Checkout [`prom-helpers`](https://github.com/adamburkegh/prom-helpers)

In prom-helpers
`./gradlew test`
`./gradlew publishToMavenLocal`

In toothpaste
`./gradlew test`

## Python

`python -m pip install -r requirements.txt`

## Graphviz

Graphviz is required, with the dot graphing tool to be on the PATH, for the graph rendering steps. Without this, either a text or LaTeX (Tikz) visualization will have to be used.

If you have chocolatey

`choco install graphviz`

Then run `dot -c` to initialize

(It's a [weird graphviz thing](https://stackoverflow.com/a/62549025/5729872))



# Running
`scripts/red.sh data/exercise1.xes` invokes the Toothpaste Miner prototype, outputting `var/exercise1.png` and various intermediates.

`scripts/st.sh exercise` invokes the miner on a DCDT file and outputs a probabilistic process tree.

The test scaffold entry point is `ModelRunner.java`

The reporting entry point is `SPNDiscoverReporter.java`

Some scripts for running on Windows and Unix are in `scripts/`.

# Results

Result files from experiments performed on this framework are in `results`. "Weight estimation" mining was done using the [spd\_we project](https://github.com/adamburkegh/spd_we). This includes output model PNML files.

## Result Models

Visualizations of the models from the evaluation are in [eval-models](eval-models/).
