# Purehs.org

Site source for [purehs.org](http://purehs.org)

## Publishing Packages, Blog Posts, and Tutorials 

Publish packages via pull request conforming to the following configuration and documentation formats.

Make sure you upload an author bio and avatar with your first post, tutorial, or package.

Note that there is a global blog and there are global tutorials. For each package there is a blog, and for each package version there are package tutorials. It is expected that tutorials are copied over with each new version and updated where necessary.

For the most part, at least at the moment, the Pure.hs ecosystem is evergreen in the sense that most development has been commited to the meta repository [pure-platform](https://github.com/grumply/pure-platform). This has worked well for me, but if anyone else starts using and developing in pure, a new solution will need to be developed. For now, I would be happy to add any work to the meta-repository if you're interested in publishing it. Packages don't really add much time to the build unless they're big (like [pure-semantic-ui](https://github.com/grumply/pure-semantic-ui)) or incur massive dependencies (like [pure-xhr](https://github.com/grumply/pure-xhr)). Overall, I welcome any interest or contributions.

### Directory Structure

Legend:

* ISO 8601: <YYYY-mm-ddThh:mm> or <YYYY-mm-dd>
* <>: placeholder; substitute with relevant name or date or yaml, etc....
* []: optional; can be omitted

> Note: <slug> is an identifier that is semi-guaranteed to be unique by the filesystem

#### Author directory structure

```
authors/
  <author>/
    author.yaml
    author.md
```

##### author.yaml

```yaml
# author.yaml
name: <display name>
[github]: <github name>
[twitter]: <twitter name>
[email]: <email>
[company]: <company>
description: |
  A short description only used in page description. <160 character max>
excerpt: |
  A markdown excerpt from the author's bio. 
```

#### Page directory structure

```
pages/
  <slug>/
    page.yaml
    page.md
```

##### page.yaml

```yaml
# page.yaml
slug : <slug>
description: |
  A short description only used in page description. <160 character max>
```

#### Blog directory structure


```
blog/
  <slug>/
    post.yaml
    post.md
```

##### post.yaml

```yaml
# post.yaml
title: <title>
[subtitle]: <subtitle>
slug: <slug>
[series]: <series name>
[episode]: <episode number; if series introduction, omit>
published: <ISO 8601>
authors:
  - author1
  - author2
tags:
  - tag1
  - tag2
[packages]:
  - package1
  - package2
short: Short post description. Used in the dropdown menu. <50 character max>
description: |
  Post description. Used only in page meta description. <160 character max; meta>
excerpt: |
  A post excerpt in markdown for display in lists. Best kept short and compelling. 
  
  Should answer the questions for the reader:

    * What's the gist?
```

#### Tutorials directory structure

```
tutorials/
  <slug>/
    tutorial.yaml
    tutorial.md
```

##### tutorial.yaml

```yaml
# tutorial.yaml
title: <title>
[subtitle]: <subtitle>
slug: <slug>
[series]: <series name>
[episode]: <episode number; if series introduction, omit>
published: <ISO 8601>
authors:
  - author1
  - author2
tags:
  - tag1
  - tag2
[packages]:
  - package1
  - package2
short: Short tutorial description. Used in the dropdown menu. <50 character max> 
description: Tutorial description. Used only in page meta description. <160 character max; meta>
excerpt: |
  A tutorial excerpt in markdown for display in lists. Best kept short and compelling. 
  
  Should answer the questions for the reader:

    * How can this tutorial help me?
    * What's the gist?
```

#### Packages directory structure

```
packages/
  <package>/
    package.yaml
    package.md
    blog/
      <slug>/
        post.yaml
        post.md
    versions/
      <version>/
        version.yaml
        modules/
          <module>/
            module.yaml
            module.md
        tutorials/
          <slug>/
            tutorial.yaml
            tutorial.md
```

##### package.yaml

```yaml
# package.yaml
name: <package name>
author: <author name>
latest: <version>
published: <ISO 8601>
license: <license-type>
[repo]: <url>
[homepage]: <url>
collaborators: []
tags:
  - tag1
  - tag2
short: A bite-sized package description. Used in the dropdown menu. <50 character max>
description: Package description. Used only in page meta description. <160 character max; meta>
```

##### package.md

This file should inform the reader if this is the package they're looking for and a quick overview of how to use it within a couple of minutes. This will appear on the package's main page, above versions.

This answers the questions: 

  * Does this package implement a solution to my problem?
  * How does it work, simply?

##### version.yaml

```yaml
# version.yaml
version: <version>
changes: |
  Important version-specific changes. 
```

##### module.yaml

```yaml
# module.yaml
name: <module name>
description: |
  A short description to use in page description. <160 character max; meta>
excerpt: |
  A module excerpt in markdown to be shown in lists of modules. 
  
  General descriptive information: explain the purpose of the module and maybe what, in general, it exports. 
  
  Someone looking for documentation is looking for one of two things at this point: 

      * Is this the module for which I'm looking for documentation?
      * What does this module do?

  The first question is answered, simply, by the module name. The second question should be answered by the excerpt.
```

##### module.md

Module markdown format is:

* H2 represents an entity start
* Code highlighting with click-to-copy is supported in fenced code blocks
* Inline live editors are supported when code is wrapped in ```<pre data-try></pre>```. The backend will guarantee that such code is compiled and results are cached.
* Content wrapped in a ```<div class="hide">``` tag will be hidden in module pages, but not in entity pages.
* Content wrapped in ```<div class="info">``` tag will display in a blue info box.
* Content wrapped in ```<div class="warn">``` tag will display in a red warning box.

