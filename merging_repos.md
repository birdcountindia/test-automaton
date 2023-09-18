```
git remote add bcichallenges https://github.com/birdcountindia/BCI-challenges
git remote add bcifunctions https://github.com/birdcountindia/bci-functions

git fetch --all
```

# Create a new branch for the source repository
# Merge the source repository's branch into the destination repository
# Make necessary changes to structure (move to subfolder)

```
git checkout -b bci-challenges bcichallenges/master
git merge --allow-unrelated-histories bci-challenges

git checkout -b bci-functions bcifunctions/main
git merge --allow-unrelated-histories bci-functions
```

###


# Push the changes to the destination repository on GitHub
# Merge individual branches into main branch (will have conflicts)

```
git push origin bci-challenges
git push origin bci-functions
```

```
git checkout main
git merge --allow-unrelated-histories bci-challenges
git merge --allow-unrelated-histories bci-functions
```
