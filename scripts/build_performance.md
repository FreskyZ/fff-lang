# fff-lang Build Performace Log

## Legacy info

[17/1/29 15:38 at tag 0.1.1]  
Nearly exactly the same as 0.1.0  
Change one char in src\main.rs  
Build time: 25.63s, 21.51s, 24.24s  
Test build time: 34.29s, 37.84s, 34.94s  

[17/1/30 20:20]  
Add codemap lib and pass compile  
Change one char in src\main.rs  
Build time: 22.53s, 22.38s, 21.23s > 22.05s  
Test build ti   me: 34.75s, 33.91s, 33.99s > 34.22s  

[17/2/6 16:13]
Directly after lexical, common(util), messages are moved out, at 989457ba  
Rebuild driver  
    Build time: 21.31s, 20.68s, 20.93s > 20.97s  
    Test build time: 22.56s, 21.64s, 21.77s > 21.99s  
Rebuild   
    Build time: 3.19s, 3.14s, 3.43s > 3.32s  
    Test build time: 11.66s, 11.58s, 11.47s > 11.57s  

[17/2/24 23:20]  
After error format refactoring in lexical/v1  
Rebuild lexical  
    Build time: 3.57s, 3.51s, 3.54s > 3.54s    
    Test build time: 13.77s, 16.56s, 12.83s > 14.39s  
Rebuild driver  
    Build time: 20.37s, 20.18s, 20.75s > 20.43s  
    Test Build time: 26.25s, 26.89s, 29.10s > 27.41s  
Analyze  
    nearly all time is longer because error handling is more complex  

[17/2/27 16:38 @ b80ba02]  
After lexical big refactor(remove v3)  
Rebuild lexical  
    Build time: 2.83s, 3.17s, 3.17s > 3.06s  
    Test build time: 12.24s, 10.84s, 11.50s > 11.53s  
Rebuild driver  
    Build time: 16.72s, 15.65s, 18.19s > 16.85s  
    Test build time: 25.24s, 25.38, 25.28s > 25.30s  
Analyze  
    lexical is better because v3 is removed and v2 is standarlized  
    driver is better because main and config is moved out  

[17/3/21 11:48 @ 78a2c86]  
After lexical/num_lit_parser huge refactor  
Rebuild lexical    
    Build time: 4.45s, 4.56s, 4.51s > 4.50s  
    Test build time: 14.45s, 12.72s, 12.77s > 13.31s  
    With trace_num_lit_parse test build time: 15.53s, 15.74s, 17.0s > 16.12s  
driver has not changed  
Analyze  
    lexical/num_lit_parser is too complex, you can see if trace it then time will change to 16.12s  

[17/3/25 19:08 @ 657c858]  
After codemap added, lexical based on codemap, lexical new driver, syntax moved out, syntax new driver, ffc new driver  
Rebuild lexical  
    Build time: 3.51s, 3.99s, 3.48s > 3.66s  
    Test build time: 12.66s, 12.82s, 12.94s > 12.80s  
Rebuild syntax  
    Build time: 3.71s, 3.69s, 3.64s > 3.68s  
    Test build time: 6.86s, 6.84s, 7.48s > 7.06s  
Rebuild driver  
    Build time: 13.48s, 12.50s, 12.64s > 12.90s 
    Test build time: 19.35s, 18.68s, 19.84s > 19.29s   
Analyze  
    lexical is better because v0 is moved to codemap and partial of v4(tokenstream) is moved to syntax  
    syntax + driver is nearly the same as previous driver  

## Build perform report at `2017/03/26 16:28:56` at `657c858`

<table><tr class='table-header'><td>project name</td><td>build type</td><td>build times</td></tr>
    <tr class='row-project-start'><td>codemap</td><td>test</td><td>1.87s</td><td>1.81s, 1.90s, 1.86s, 1.93s, 1.87s</td>
</tr>
    <tr><td></td><td>build</td><td>0.82s</td><td>0.80s, 0.78s, 0.79s, 0.81s, 0.91s</td>
</tr>
</table>

## Build perform report at `2017/03/26 16:32:19` at `657c858`

<table>
<tr class='table-header'><td>project name</td><td>build type</td><td>avg build time</td><td>build times</td></tr>
    <tr class='row-project-start'><td>codemap</td><td>test</td><td>1.86s</td><td>1.86s, 1.86s, 1.88s, 1.85s, 1.84s</td>
</tr>
    <tr><td></td><td>build</td><td>1.04s</td><td>0.97s, 0.97s, 1.40s, 0.97s, 0.88s</td>
</tr>
</table>

## Build perform report at `2017/03/26 16:34:49` at `657c858`

<table>
<tr class='table-header'><td>project name</td><td>build type</td><td>avg build time</td><td>build times</td></tr>
    <tr class='row-project-start'><td>lexical</td><td>test</td><td>13.56s</td><td>12.80s, 12.84s, 12.71s, 14.25s, 15.18s</td>
</tr>
    <tr><td></td><td>build</td><td>3.62s</td><td>3.66s, 3.54s, 3.81s, 3.53s, 3.55s</td>
</tr>
    <tr class='row-project-start'><td>syntax</td><td>test</td><td>7.41s</td><td>6.99s, 7.56s, 7.81s, 7.37s, 7.33s</td>
</tr>
    <tr><td></td><td>build</td><td>4.02s</td><td>4.10s, 4.10s, 3.93s, 3.99s, 3.96s</td>
</tr>
    <tr class='row-project-start'><td>driver</td><td>test</td><td>19.27s</td><td>18.78s, 18.77s, 20.36s, 19.46s, 18.99s</td>
</tr>
    <tr><td></td><td>build</td><td>13.60s</td><td>14.18s, 12.95s, 13.74s, 13.27s, 13.84s</td>
</tr>
</table>

## Build perform report at `2017/03/28 03:28:54` at `8c7ad9d`

<table>
<tr class='table-header'><td>project name</td><td>build type</td><td>avg build time</td><td>build times</td></tr>
    <tr class='row-project-start'><td>lexical</td><td>test</td><td>14.29s</td><td>14.83s, 13.99s, 14.93s, 13.92s, 13.80s</td>
</tr>
    <tr><td></td><td>build</td><td>3.61s</td><td>3.63s, 3.59s, 3.56s, 3.65s, 3.62s</td>
</tr>
    <tr class='row-project-start'><td>syntax</td><td>test</td><td>7.23s</td><td>7.21s, 7.20s, 7.50s, 7.10s, 7.12s</td>
</tr>
    <tr><td></td><td>build</td><td>3.66s</td><td>3.12s, 3.90s, 3.60s, 3.90s, 3.80s</td>
</tr>
    <tr class='row-project-start'><td>driver</td><td>test</td><td>21.49s</td><td>21.50s, 22.69s, 20.57s, 21.24s, 21.44s</td>
</tr>
    <tr><td></td><td>build</td><td>13.58s</td><td>12.86s, 12.82s, 12.86s, 15.39s, 13.97s</td>
</tr>
</table>

