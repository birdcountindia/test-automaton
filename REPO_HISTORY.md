1. Add remotes to external repos to be merged.

```
git remote add -f bcichallenges https://github.com/birdcountindia/BCI-challenges
git remote add -f bcifunctions https://github.com/birdcountindia/bci-functions
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
```

8. Merge individual branches into main branch (will have conflicts)

```
git checkout main
git merge --allow-unrelated-histories bci-challenges
git merge --allow-unrelated-histories bci-functions
```

