127.0.0.1 localhost localhost.localdomain localhost4 localhost4.localdomain4

# The following lines are desirable for IPv6 capable hosts
::1       localhost.localdomain localhost6 localhost6.localdomain6 ip6-localhost ip6-loopback
fe00::0 ip6-localnet
ff00::0 ip6-mcastprefix
ff02::1 ip6-allnodes
ff02::2 ip6-allrouters
ff02::3 ip6-allhosts

${back_end_ipv6} backend.internal

${front_end_ipv6} frontend.internal
