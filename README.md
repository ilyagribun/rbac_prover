# RBAC prover

## Пример использования

Пример использования прувера для предоставления доступа сотрудников ФКН к чтению или изменению оценок, описанный в статье.

Для начала необходимо запустить интерактивный режим командой `cabal repl`

Затем необходимо импортировать библиотеки Test, RBACPolicies и Permissions:

```haskell
ghci> import Test
ghci> import RBACPolicies
ghci> import Permission
```
После этого можно проверять наличие доступов с помощью запросов вида:
```haskell
ghci> makeSingleDecision policies' ("Evgeniy", "CS", r)
True
ghci> makeSingleDecision policies' ("Evgeniy", "Ten", r \/ w)
True
ghci> makeSingleDecision policies' ("Evgeniy", "CAOS", w)
False
ghci> makeSingleDecision policies' ("Evgeniy", "CS", w)
False
```

