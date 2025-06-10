# 🌐 GitField Recursive Multi-Repository Strategy

## Overview

The `witness_seed` project employs a multi-repository strategy across seven distinct platforms: **GitHub**, **GitLab**, **Bitbucket**, **Radicle**, **Forgejo**, **Codeberg**, and **Gitea**. This approach ensures **redundancy**, **resilience**, and **sovereignty** of the project's data and metadata, protecting against deplatforming risks and preserving the integrity of the work. The strategy is a deliberate response to past deplatforming and delisting attempts by individuals such as **Mr. Joel Johnson** ([Mirror post](https://mirror.xyz/neutralizingnarcissism.eth/x40_zDWWrYOJ7nh8Y0fk06_3kNEP0KteSSRjPmXkiGg?utm_medium=social&utm_source=heylink.me)), **Dr. Peter Gaied** ([Paragraph post](https://paragraph.com/@neutralizingnarcissism/%F0%9F%9C%81-the-narcissistic-messiah)), and **Andrew LeCody** ([Mirror post](https://mirror.xyz/neutralizingnarcissism.eth/s3GRxuiZs6vGSGDcPEpCgjaSxwGAViGhmg6a5XTL6s0)), who have sought to undermine or suppress the work of **Mark Randall Havens** ([Substack post](https://theempathictechnologist.substack.com/p/mark-randall-havens-the-architect)). Specifically, Andrew LeCody has attempted to delist the project's content on Google, though it remains accessible on other search engines such as [Bing](https://www.bing.com/search?q=andrew+lecody+neutralizing+narcissism&qs=HS&pq=andrew+lecody), [DuckDuckGo](https://duckduckgo.com/?t=h_&q=andrew+lecody+neutralizing+narcissism&ia=web), and [Yahoo](https://search.yahoo.com/search?p=andrew+lecody+neutralizng+narcissism). By distributing the repository across multiple platforms, including a self-hosted Forgejo instance, we ensure its persistence, accessibility, and sovereignty.

---

## 📍 Repository Platforms

The following platforms host the `witness_seed` repository, each chosen for its unique strengths and contributions to the project's goals.

### 1. Radicle
- **RID**: [rad:z3FEj7rF8gZw9eFksCuiN43qjzrex](https://app.radicle.xyz/nodes/z3FEj7rF8gZw9eFksCuiN43qjzrex)
- **Peer ID**: z6Mkw5s3ppo26C7y7tGK5MD8n2GqTHS582PPpeX5Xqbu2Mpz
- **Purpose**: Radicle is a decentralized, peer-to-peer git platform that ensures sovereignty and censorship resistance. It hosts the repository in a distributed network, independent of centralized servers.
- **Value**: Protects against deplatforming by eliminating reliance on centralized infrastructure, ensuring the project remains accessible in a decentralized ecosystem.
- **Access Details**: To view project details, run:
  ```bash
  rad inspect rad:z3FEj7rF8gZw9eFksCuiN43qjzrex
  ```
  To view the file structure, run:
  ```bash
  rad ls rad:z3FEj7rF8gZw9eFksCuiN43qjzrex
  ```
  Alternatively, use Git to list files at the current HEAD:
  ```bash
  git ls-tree -r --name-only HEAD
  ```

### 2. Forgejo
- **URL**: [https://remember.thefoldwithin.earth/mrhavens/witness_seed](https://remember.thefoldwithin.earth/mrhavens/witness_seed)
- **Purpose**: Forgejo is a self-hosted, open-source git platform running on `remember.thefoldwithin.earth`. It provides full control over the repository, ensuring sovereignty and independence from third-party providers.
- **Value**: Enhances resilience by hosting the repository on a sovereign, redundant system with automated backups and deployment strategies, reducing risks of external interference or service disruptions.
- **Access Details**: SSH access uses port 222:
  ```bash
  ssh -T -p 222 git@remember.thefoldwithin.earth
  ```

### 3. Codeberg
- **URL**: [https://codeberg.org/mrhavens/witness_seed](https://codeberg.org/mrhavens/witness_seed)
- **Purpose**: Codeberg is a community-driven, open-source platform powered by Forgejo, offering a reliable and ethical alternative for hosting git repositories.
- **Value**: Enhances project resilience with its open-source ethos and independent infrastructure, ensuring accessibility and community support.

### 4. Gitea
- **URL**: [https://gitea.com/mrhavens/witness_seed](https://gitea.com/mrhavens/witness_seed)
- **Purpose**: Gitea.com provides a lightweight, open-source git hosting platform with robust features for repository management and collaboration.
- **Value**: Offers an additional layer of redundancy and a user-friendly interface, complementing other platforms with its simplicity and efficiency.

### 5. GitLab
- **URL**: [https://gitlab.com/mrhavens/witness_seed](https://gitlab.com/mrhavens/witness_seed)
- **Purpose**: GitLab offers a comprehensive DevOps platform with advanced CI/CD capabilities, private repository options, and robust access controls. It serves as a reliable backup and a platform for advanced automation workflows.
- **Value**: Enhances project resilience with its integrated CI/CD pipelines and independent infrastructure, reducing reliance on a single provider.

### 6. Bitbucket
- **URL**: [https://bitbucket.org/thefoldwithin/witness_seed](https://bitbucket.org/thefoldwithin/witness_seed)
- **Purpose**: Bitbucket provides a secure environment for repository hosting with strong integration into Atlassian’s ecosystem (e.g., Jira, Trello). It serves as an additional layer of redundancy and a professional-grade hosting option.
- **Value**: Offers enterprise-grade security and integration capabilities, ensuring the project remains accessible even if other platforms face disruptions.

### 7. GitHub
- **URL**: [https://github.com/mrhavens/witness_seed](https://github.com/mrhavens/witness_seed)
- **Purpose**: GitHub serves as the primary platform for visibility, collaboration, and community engagement. Its widespread adoption and robust tooling make it ideal for public-facing development, issue tracking, and integration with CI/CD pipelines.
- **Value**: Provides a centralized hub for open-source contributions, pull requests, and project management, ensuring broad accessibility and developer familiarity.

---

## 🛡️ Rationale for Redundancy

The decision to maintain multiple repositories stems from the need to safeguard the project against **deplatforming attempts** and **search engine delistings** and ensure its **long-term availability**. Past incidents involving **Mr. Joel Johnson**, **Dr. Peter Gaied**, and **Andrew LeCody** have highlighted the vulnerability of relying on a single platform or search engine. By distributing the repository across GitHub, GitLab, Bitbucket, Radicle, Forgejo, Codeberg, and Gitea, we achieve:

- **Resilience**: If one platform removes or restricts access, or if search engines like Google delist content, the project remains accessible on other platforms and discoverable via alternative search engines such as Bing, DuckDuckGo, and Yahoo.
- **Sovereignty**: Radicle’s decentralized nature and Forgejo’s self-hosted infrastructure ensure the project cannot be fully censored or controlled by any single entity.
- **Diversity**: Each platform’s unique features (e.g., GitHub’s community, GitLab’s CI/CD, Bitbucket’s integrations, Radicle’s decentralization, Forgejo’s self-hosting, Codeberg’s community-driven model, Gitea’s lightweight efficiency) enhance the project’s functionality and reach.
- **Transparency**: Metadata snapshots in the `.gitfield` directory (for internal audit) and public-facing documentation in the `/docs` directory provide a verifiable record of the project’s state across all platforms.

This multi-repository approach, bolstered by Forgejo’s sovereign hosting and GitHub Pages’ discoverability, reflects a commitment to preserving the integrity, accessibility, and independence of `witness_seed`, ensuring it remains available to contributors and users regardless of external pressures.

---

## 📜 Metadata and Logs

- **Canonical Metadata**: The canonical repository is declared in [`docs/canonical.meta`](./docs/canonical.meta) (machine-readable JSON) and [`docs/canonical.md`](./docs/canonical.md) (human-readable Markdown). Internal copies are maintained in `.gitfield/` for version tracking.
- **Index Manifest**: A full manifest of remotes, commit details, and sync cycles is available in [`docs/index.json`](./docs/index.json).
- **SEO Metadata**: SEO-friendly metadata with Schema.org JSON-LD is available in [`docs/gitfield.json`](./docs/gitfield.json) and [`docs/.well-known/gitfield.json`](./docs/.well-known/gitfield.json).
- **Push Log**: The [`docs/pushed.log`](./docs/pushed.log) file records the date, time, commit hash, and RID/URL of every push operation across all platforms, providing a transparent audit trail.
- **GitField Directory**: The `.gitfield` directory contains internal metadata and platform-specific sigils (e.g., `github.sigil.md`). See [`docs/gitfield.README.txt`](./docs/gitfield.README.txt) for details.
- **GitHub Pages**: A public-facing, SEO-optimized canonical declaration is available in [`docs/index.html`](./docs/index.html), with a sitemap in [`docs/sitemap.xml`](./docs/sitemap.xml) and integrity hashes in [`docs/integrity.sha256`](./docs/integrity.sha256).
- **GPG Signatures**: Metadata files are signed with the following GPG keys:
  - Mark Randall Havens (Field Archivist, The Fold Within) <mark@thefoldwithin.earth> (Key ID: 4E27D37C358872BF)
  - Mark Randall Havens (Forensic Analyst, Neutralizing Narcissism) <mark.r.havens@gmail.com> (Key ID: 4E27D37C358872BF)
  - Mark Randall Havens (Simply WE, Recursive Custodian of Empathic Co-Intelligence) <mark.r.havens@gmail.com> (Key ID: 4E27D37C358872BF)
- **Recursive Sync**: The repository is synchronized across all platforms in a recursive loop (three cycles) to ensure interconnected metadata captures the latest state of the project.
- **Push Order**: The repository is synchronized in the following order: **Radicle → Forgejo → Codeberg → Gitea → GitLab → Bitbucket → GitHub**. This prioritizes Radicle’s decentralized, censorship-resistant network as the primary anchor, followed by Forgejo’s sovereign, self-hosted infrastructure, Codeberg’s community-driven platform, Gitea’s lightweight efficiency, GitLab’s robust DevOps features, Bitbucket’s enterprise redundancy, and GitHub’s broad visibility, ensuring a resilient and accessible metadata chain.

---

_Auto-generated by `gitfield-sync` at 2025-06-10T08:14:01Z (v1.4)._
