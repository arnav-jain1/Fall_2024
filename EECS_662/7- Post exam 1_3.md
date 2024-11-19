Set returns the original value

```
bind m = new 5 in                [(m, ->5)]
	bind n = m in                [(n, ->5)]
		set m = 6 ; deref n      
== 6
```
m will store the address of 5, n is the same as m so it also stores the address of 5
They *point* to the same place
When m is set to 6 and n is referenced, the value is 6


```
bind inc = (lambda in (set l ((deref l) + 1)))
	bind n = new 5
		inc n; deref n
==
```
Note that inc does not return a value 

