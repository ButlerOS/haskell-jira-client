# md2jira - Manage Jira Backlog In Markdown

Use this tool to push your tasks written in markdown to JIRA.

## Usage

Add to a `project.md`:

```
# The epic name

The goal of this epic is to achieve big goals!

## Story name

This task is done when:

- [ ] action a
- [ ] action b

## Other story

DOD
```

Write an environment `.env`:

```
JIRA_URL=https://issues.example.com
JIRA_PROJECT=MY_PROJ
JIRA_TOKEN=Msecret
```

Install the toolchain using [ghcup](https://www.haskell.org/ghcup/) to run the tool:

```
$ export $(cat .env)
$ cabal run exe:md2jira -- project.md
```

`md2jira` creates the stories in the Jira database and it inserts the issue id in the header.
For example, after the first run, the file will look like this:

```
# MY_PROJ-42 The epic name

The goal of this epic is to achieve big goals!

## MY_PROJ-43 Story name

This task is...
```

When the title or the description is changed in the file, the Jira database is updated.
The tool perform one-way push synchronization: the Jira data is not pulled.


## Integration with etherpad

Here is a one-liner to maintain your backlog in etherpad:

```
$ curl https://etherpad.example.com/project-backlog/export/txt | md2jira > update.md \
 && curl "http://etherpad.example.com/api/1/setText?apikey=$PAD_TOKEN&padID=project-backlog" \
    --data-urlencode "text=$(cat update.md)"
```

## Contribute

Feel free to report issues and propose new features. Run the `./bin/run-tests` at the top of the project to validate your changes.

Roadmap:

- [ ] Handle story status, e.g. no `[x]` means open, one tick means in-progress, all-ticked means completed
- [ ] Support sub-task, e.g. with `###` third heading
- [ ] Manage assignment, e.g. by adding user-id after the heading
- [ ] Provide a custom editor with quill-ot to handle drag/drop and outline folding
