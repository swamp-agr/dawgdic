# Contribution policy

## Did you find a bug?

- Please create a new issue with description.
- It would be nice if you provide more details such as GHC version, cabal-install version (or other build tool title and version).
- Reproducible case will be appreciated.

## Do you want to contribute a new feature/fix or any other improvement?

- Please ensure that the corresponding issue exists prior opening a new pull request.
- Provide a clear title.
- Describe a feature. If it contains more code than could be read during 15 minutes or more than 150 lines of code, please describe your contribution, it will help us in review. Keep description short and consise.
- Add/re-run tests.
- Add/re-run benchmarks.

## LLM Contribution Policy

### Common

- Scope of contribution matches exactly with exactly a single feature, fix, documentation, refactoring or any other improvement. Avoid mixing two improvements in a single pull request.
- Code diffs larger than 150 LOC (in total) are not encouraged and could be rejected.
- Avoid over-generalisation, provide a clear description of what has been included in the patch, why this contribution should be accepted, what problem it solves.
- Strip LLM-suggested "improvements" that fall outside your stated scope.
- Run tests and benchmarks

Avoid:

- Mixing refactors, docs and features.
- Touching unrelated modules with styling changes.
- Submitting cleanup or idiomatic fixes unless explicitly requested.

### Metadata

- LLM usage must be disclosed. Traceability protects you, the project, and future of Haskell Open Source. Undisclosed LLM usage or any suspicion of it will be rejected.
- Provide following details:
    - Model name, version, revision/hash (if applicable), provider.
    - Full prompt(s), associated response, refinement chain (with timestamps).
    - Session(s) if more than one session of LLM interaction had taken a place.

#### How to generate

1. Export your LLM session (most platforms should support JSON/Markdown export).
2. Strip your personal identifiers (PII must not be shared).
3. (Nice-to-have) validate against schema (see below).
4. Attach as collapsible section in pull request or its comments. Otherwise, submit in separate commit (as an extension of the patch, it will be exempted from 150 LOC rule) under `.llmtraces` directory.

**Note**: We believe that transparency leads to positive outcomes in the future. By providing full interaction with LLM 
- together we can optimise LLM usage for Haskell.
- this data will be used in further academic researches.

#### Schema

Use the following spec as guidance and not as strict requirement. We are still at early stages of interaction with LLM.

```json
{
    "model": "string",
    "version": "string",
    "revision": "string|null",
    "provider": "string",
    "access_date": "YYYY-MM-DD",
    "prompt_chain": [
        {
            "timestamp": "ISO8601",
            "role": "user|assistant",
            "content": "string"
        }
    ],
}```
