
1. Add remotes to external repos to be merged.

```
git remote add -f bcichallenges https://github.com/birdcountindia/BCI-challenges
git remote add -f bcifunctions https://github.com/birdcountindia/bci-functions
git remote add -f pmp https://github.com/birdcountindia/BCI-patchmon
```

2. Create a new branch for the external repository
2. Merge the master repository's branch into the newly added repository
2. Delete remote connection so that further changes are restricted to current master repo (not original repo also)
2. Update branch to track origin remote (main branch)
2. *Make necessary changes to structure (move to subfolder) and commit*
2. Push the changes to relevant branch of master repo

```
git checkout -b bci-challenges bcichallenges/master
git pull origin main --allow-unrelated-histories
git remote remove bcichallenges
git branch bci-challenges -u origin/bci-challenges
# changes
git push origin bci-challenges

git checkout -b bci-functions bcifunctions/main
git pull origin main --allow-unrelated-histories
git remote remove bcifunctions
git branch bci-functions -u origin/bci-functions
# changes
git push origin bci-functions

git checkout -b pmp pmp/main
git pull origin main --allow-unrelated-histories
git remote remove pmp
git push origin pmp
git branch pmp -u origin/pmp
# changes
git push origin pmp
```

8. Merge individual branches into main branch (will have conflicts)

```
git checkout main
git merge --allow-unrelated-histories bci-challenges
git merge --allow-unrelated-histories bci-functions
```

### Resources

This workflow was formulated using several guides/resources including:

- https://gfscott.com/blog/merge-git-repos-and-keep-commit-history/
- https://stackdiary.com/tutorials/how-to-merge-two-git-repositories/

<!-- -->
<!-- -->
