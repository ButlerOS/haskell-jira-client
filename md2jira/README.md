# md2jira - Manage Jira Backlog In Markdown

Use this tool to push your tasks written in markdown to JIRA.

## Suggested Workflow

**Create a `project.md` file**

List the main goals for the top level epics.

Here is an example:

```markdown
# Develop Y

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

## Implement feature name

The goal of this story is to ...:

- [ ] create module
- [ ] update api

# Operate X

# Support Z
```

**Synchronize to Jira**

Refine the entry and run `md2jira project.md` to create the issues in Jira.

The tool will update the file to inject the remote identifiers:

```markdown
# Develop Y {#PROJ-1}

## Implement feature name {#PROJ-2}

The goal of this story is to ...:

- [ ] create module
- [ ] update api

# Operate X {#PROJ-3}

# Support Z {#PROJ-4}
```

Re-run `md2jira` at any points to synchronize the file content, the command is idempotent.
An entry without an identifier will be created, otherwise it is updated.
The tool perform one-way push synchronization: the Jira data is not pulled.

Once all the tasks are done, run `md2jira` one more time to close the story.
Then the story can be removed from the file and added to a separate `archive.md`.


## Usage

Get the toolchain using [ghcup](https://www.haskell.org/ghcup/) and install the command line:

```
$ cabal install --installdir=~/.local/bin exe:md2jira
```

Write an environment `.env`:

```
JIRA_URL=https://issues.example.com
JIRA_PROJECT=MY_PROJ
JIRA_TOKEN=Msecret
```

Run the tool:

```
$ export $(cat .env)
$ md2jira project.md
```

Alternatively, run the tool from the sources using:

```
$ cabal run exe:md2jira -- --help
```


## Contribute

Feel free to report issues and propose new features. Run the `./bin/run-tests` at the top of the project to validate your changes.

Roadmap:

- [ ] Support story status
- [ ] Manage assignment, e.g. by adding user-id after the heading
- [ ] Support sub-task, e.g. with `###` third heading
