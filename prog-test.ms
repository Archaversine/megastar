# 0 0 0 0 0 0 0 0 0 0

&init<from, to, value>(@a << [$from]-> {{+| {-| $to, $from}}~$value ^a)
&ones<from, to>*init<$from, $to, 1>
&twos<from, to>*init<$from, $to, 2>

*ones<4, 6>

<< {}(<% ~10 <=)

