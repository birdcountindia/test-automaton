```
git remote add bcichallenges https://github.com/birdcountindia/BCI-challenges
git remote add bcifunctions https://github.com/birdcountindia/bci-functions

git fetch --all
```

- Create a new branch for the external repository
- Merge the master repository's branch into the newly added repository
- Delete remote connection so that further changes are restricted to current master repo (not original repo also)
- *Make necessary changes to structure (move to subfolder) and commit*
- Push the changes to relevant branch of master repo

```
git checkout -b bci-challenges bcichallenges/master
git pull origin main --allow-unrelated-histories
git remote remove bcichallenges
# changes
git push origin bci-challenges

git checkout -b bci-functions bcifunctions/main
git pull origin main --allow-unrelated-histories
git remote remove bcifunctions
# changes
git push origin bci-functions
```

Merge individual branches into main branch (will have conflicts)

```
git checkout main
git merge --allow-unrelated-histories bci-challenges
git merge --allow-unrelated-histories bci-functions
```

