# The **H**askell **I**s **O**bviously **B**etter at **E**verything (HIOBE) Index

## Purpose

The HIOBE Index is a Haskell application that is specifically engineered to be
explored using the Glorious Glasgow Haskell Compiler's (GHC) profiling
capabilities and libraries/tools such as
[`ghc-debug`](https://gitlab.haskell.org/ghc/ghc-debug) and
[`eventlog2html`](https://github.com/mpickering/eventlog2html).

## Usage

Refer to
[Well-Typed's recorded YouTube livestream titled "Profiling Memory Usage with `eventlog2html` and `ghc-debug`"](https://www.youtube.com/live/nIyaC3JtlyQ?feature=share)
for a tutorial on profiling and debugging the HIOBE Index's memory usage.

Profiling workflows will be further documented in the future.

## Explanation

The [TIOBE Index](https://www.tiobe.com/tiobe-index/) is a programming language
popularity index. At the time of writing, the TIOBE Index has Haskell ranked at
#43, behind D and LabVIEW (?). This is blasphemy and we shouldn't stand for it.

The HIOBE Index is my rebuttal. It is a simple web application (in `server`)
that stores (a subset of) the
[2022 Stack Overflow Developer Survey](https://survey.stackoverflow.co/2022/)
results in a sqlite database (over 70000 rows). The database has a single table
`survey_data` that looks like this:

```sql
CREATE TABLE IF NOT EXISTS "survey_data" (
    ResponseId INTEGER PRIMARY KEY AUTOINCREMENT,
    LanguageHaveWorkedWith TEXT,
    LanguageWantToWorkWith TEXT,
    ConvertedCompYearly INTEGER
);
```

### Endpoints

We can issue requests to the following endpoints of the web server:

> `POST /survey/respond`

Submit your own response to the HIOBE Index survey.

Example request:

```bash
curl -X POST http://localhost:3001/survey/respond -d '
{
    "haveWorkedWith": ["python","java","go","erlang","lua","javascript","typescript","clojure"],
    "wantToWorkWith": ["haskell"],
    "yearlyComp": 50000
}'
```

Example response:

```json
{
    "message": "Thanks!",
    "responseId": 73858
}
```

> `GET /truth`

Serves truth.

> `GET /languages/list`

List all languages stored in the database.

> `GET /languages/count/have/:lang`

The number of survey respondants that have worked with `lang`.

> `GET /languages/count/want/:lang`

The number of survey respondants that want to work with `lang`.

> `GET /languages/hist/have`

A histogram of the languages respondants have worked with

> `GET /languages/hist/have`

A histogram of the languages respondants want to work with

### Traffic Generation

We want to make sure that the HIOBE Index makes Haskell look as good as we know
it is, so we also have an executable application that generates traffic (in
`traffic`) which automatically submits lots of honest survey responses to the
HIOBE Index, and also generates some nice request traffic.

Unfortunately, in a sick twist of fate, the HIOBE Index server application has
been having some memory issues. You are invited to profile the `hiobe-server`
application with tools such as `eventlog2html` and `ghc-debug` in order to
figure out how to fix these issues.
