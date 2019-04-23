
### Execute Solutions

If you like, run GHCi with Docker Compose:
```sh
docker-compose run haskell ghci
```

Load a Haskell file to play around with the defined functions:
```sh
Prelude> :load exercise1/Solution.hs
```

### Generate Submissions

Create a file called `mat-numbers.env`.
It should store the matriculation number of each exercise group member:
```env
MAT1=111111
MAT2=222222
MAT3=333333
MAT4=444444
```
Then run:
```sh
docker-compose run submit
```
Dedicated ZIP files for each exercise are created in `submissions/`.
