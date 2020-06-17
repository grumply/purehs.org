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
synopsis: |
  A short description to use in page title.
description: |
  A text description to go in the page's meta description tag.
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
synopsis: |
  A short description to use in page title.
description: |
  A text description to go in the page's meta description tag.
excerpt: |
  A markdown excerpt. Likely unused since this is a page and lists of
  pages aren't likely to be shown anywhere.
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
[episode]: <series number>
published: <ISO 8601>
[edited]: <ISO 8601>
authors:
  - author1
  - author2
editors: []
tags:
  - tag1
  - tag2
synopsis: |
  A short description to use in page title.
description: |
  A text description to be used in a page meta tag.
excerpt: |
  A post excerpt in markdown for display in lists.
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
[episode]: <series number>
published: <ISO 8601>
[edited]: <ISO 8601>
authors:
  - author1
  - author2
editors: []
tags:
  - tag1
  - tag2
[packages]:
  - package1
  - package2
synopsis: |
  A short description to use in page title.
description: |
  A text description to be used in a page meta tag.
excerpt: |
  A post excerpt in markdown for display in lists.
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
synopsis: |
  A short description to use in page title.
description: |
  A text description to be used in a page meta tag.
excerpt: |
  A package excerpt in markdown to be shown in lists of packages. Keep short.
```

##### post.yaml

```yaml
# post.yaml
title: <title>
[subtitle]: <subtitle>
slug: <slug>
[episode]: <series number>
published: <ISO 8601>
[edited]: <ISO 8601>
authors:
  - author1
  - author2
editors: []
tags:
  - tag1
  - tag2
synopsis: |
  A short description to use in page title.
description: |
  A text description to be used in a page meta tag. Keep short.
excerpt: |
  A post excerpt in markdown for display in lists. Keep short.
```

##### version.yaml

```yaml
# version.yaml
version: <version>
changes: |
  Important version-specific changes and examples in markdown.
```

##### module.yaml

```yaml
# module.yaml
name: <module name>
synopsis: |
  A short description to use in page title.
description: |
  Module description to be used in page meta tags. Keep short.
excerpt: |
  A module excerpt in markdown to be shown in lists of modules. Keep somewhat short.
```

##### tutorial.yaml

```yaml
# tutorial.yaml
title: <title>
[subtitle]: <subtitle>
slug: <slug>
[episode]: <series number>
published: <ISO 8601>
[edited]: <ISO 8601>
authors:
  - author1
  - author2
editors: []
tags:
  - tag1
  - tag2
[packages]:
  - package1
  - package2
synopsis: |
  A short description to use in page title.
description: |
  A text description to be used in a page meta tag. Keep short.
excerpt: |
  A post excerpt in markdown for display in lists. Keep short.
```

##### module.md

Module markdown format is:

* H2 represents an entity start
* Code highlighting with click-to-copy is supported in fenced code blocks
* Inline live editors are supported when code is wrapped in ```<pre data-try></pre>```. The backend will guarantee that such code is compiled and results are cached.
* Content wrapped in a ```<div class="hide">``` tag will be hidden in module pages, but not in entity pages.

