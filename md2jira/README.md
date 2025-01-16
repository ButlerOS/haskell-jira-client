# md2jira - Manage Jira Backlog In Markdown

Use this tool to push your backlog written in markdown to JIRA.


## Usage

**Create a `project.md` file**

List the main goals for the top level epics.

Here is an example:

```markdown
# Develop Y

The goal of this epic is...

# Operate X

# Support Z
```


**Write down what needs to be done**

- Pick a one-liner summary for the title.
- Describe the goals and the definition of done for the description.
- Write down a list of actionable item for the tasks.

The file should looks like this:

```markdown
# Develop Y

The goal of this epic is...

## Implement feature name

The goal of this story is...

- [ ] create module
- [ ] update api

## Upgrade component

The goal of this other story is...

- [ ] upgrade version

# Operate X

# Support Z
```


**Synchronize to Jira**

Run `md2jira project.md` to create the backlog in Jira.

The tool will update the file to inject the remote identifiers:

```markdown
# Develop Y {#PROJ-1}

The goal of this epic is...

## Implement feature name {#PROJ-2}

The goal of this story is...

- [ ] create module
- [ ] update api

## Upgrade component {#PROJ-3}

The goal of this other story is...

- [ ] upgrade version

# Operate X {#PROJ-4}

# Support Z {#PROJ-5}
```

An entry without an identifier will be created, otherwise it is updated.
The tool perform one-way push synchronization: the Jira data is not pulled.

> [!tip]
> Re-run `md2jira` at any points to synchronize the file content, the command is idempotent.

**Update Status**

When one of the following attribute is defined, then `md2jira` will set its value in Jira:

- **status**: one of todo/wip/done
- **points**: a number
- **assignee**: a nickname

The assignee nicknames must be defined in the file frontmater. Here is an example:

```markdown
---
users:
  tc: tdecacqu@redhat.com
---
# Develop Y {#PROJ-1}

## Implement feature name {#PROJ-2 status=done points=5 assignee=tc}

The goal of this story is to ...:

- [x] create module
- [ ] update api
```

> [!note]
> The attributes that are not defined in the markdown file are not updated in Jira.


## Install

Get the toolchain using [ghcup](https://www.haskell.org/ghcup/) and download the `md2jira` sources:

```bash
git clone https://github.com/ButlerOS/haskell-jira-client
cd haskell-jiira-client
```

Build and copy the command line to `~/.cabal/bin`

```
$ cabal install exe:md2jira
```

Write an environment `.env`:

```
JIRA_URL=https://issues.example.com
JIRA_PROJECT=MY_PROJ
JIRA_TOKEN=Msecret
```

Run the tool:

```
$ export $(cat .env) PATH="${HOME}/.cabal/bin:${PATH}"
$ md2jira --dry < project.md
```

Alternatively, run the tool directly from the sources using:

```
$ cabal run exe:md2jira -- --help
```


## Contribute

Feel free to report issues and propose new features. Run the `./bin/run-tests` at the top of the project to validate your changes.

Roadmap:

- [ ] Update the epic link when a story is moved to a different epic.
- [ ] Mark a story has completed when all the tasks are closed.
- [ ] Support sub-task, e.g. with `###` third heading.
