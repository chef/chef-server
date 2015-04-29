## role data pulled from prod 02-03-2012
## role data collection script crashed.
fname <- "role-data.txt.gz"
roles <- read.table(fname, header=TRUE,
                    colClasses=c("factor", "character", "integer"))

## orgs with roles
length(levels(roles$org_name))

## overall role count
length(roles$role_size)

## summary of role sizes in bytes
summary(roles$role_size)

## wow, that's a largish role!
roles[which.max(roles$role_size), ]

## role count by org and summary of number of roles per org
role_count_by_org <- table(roles$org_name)
summary(as.integer(role_count_by_org))

## they love their roles!
role_count_by_org[which.max(role_count_by_org)]

## total role bytes by org and summary
role_bytes_by_org <- tapply(roles$role_size, roles$org_name, sum)
summary(as.integer(role_bytes_by_org))

## heaviest roles winner!
role_bytes_by_org[which.max(role_bytes_by_org)]
