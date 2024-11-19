# Lists
```bind ??? = (lambda x:TNum in 
				lambda y: TNum in 
					(lambda x:TBool in if c then x else y))

	bind r = ((cons 1) 2)
	lambda c:TBool in if c then 1 else 2



```
THis is a record!, if false then will print the first elem if true then will print 2

Lists are records where the first elem is the value the second elem is a record so lets daisy chain them
```
bind hd = ??? in 
	bind tl = ??? in 
		bind cons = ((cons 2) ((conds 1) 0))

	
```
# Simply typed lambda calc
Functions and function types

```
STC ::= id | lambda id in STC | STC STC
```



