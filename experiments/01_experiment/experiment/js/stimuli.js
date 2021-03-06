var items = [
    {item: "senator", condition: "passive", sentence: "The senator was awed by the statue."},
    {item: "senator", condition: "active", sentence: "The senator unveiled the statue."},
    {item: "senator", condition: "by-locative", sentence: "The senator was speaking by the statue."},
    {item: "senator", condition: "non-by-locative", sentence: "The senator has spoken about the statue."},
    {item: "woman", condition: "passive", sentence: "The woman was stung by the jellyfish."},
    {item: "woman", condition: "active", sentence: "The woman caught the jellyfish."},
    {item: "woman", condition: "by-locative", sentence: "The woman was swimming by the jellyfish."},
    {item: "woman", condition: "non-by-locative", sentence: "The woman has swum into the jellyfish."},
    {item: "prisoner", condition: "passive", sentence: "The escaping prisoner was illuminated by the guard tower."},
    {item: "prisoner", condition: "active", sentence: "The escaping prisoner avoided the guard tower."},
    {item: "prisoner", condition: "by-locative", sentence: "The escaping prisoner was hiding by the guard tower."},
    {item: "prisoner", condition: "non-by-locative", sentence: "The escaping prisoner has hidden below the guard tower."},
    {item: "foreigner", condition: "passive", sentence: "The foreigner was confused by the blinking traffic light."},
    {item: "foreigner", condition: "active", sentence: "The foreigner misunderstood the blinking traffic light."},
    {item: "foreigner", condition: "by-locative", sentence: "The foreigner was loitering by the blinking traffic light."},
    {item: "foreigner", condition: "non-by-locative", sentence: "The foreigner has loitered at the blinking traffic light."},
    {item: "Dalmatian", condition: "passive", sentence: "The Dalmatian was pursued by the fire truck."},
    {item: "Dalmatian", condition: "active", sentence: "The Dalmatian chased the fire truck."},
    {item: "Dalmatian", condition: "by-locative", sentence: "The Dalmatian was running by the fire truck."},
    {item: "Dalmatian", condition: "non-by-locative", sentence: "The Dalmatian has run around the fire truck."},
    {item: "secretary", condition: "passive", sentence: "The secretary was splashed by the drinking fountain."},
    {item: "secretary", condition: "active", sentence: "The secretary cleaned the drinking fountain."},
    {item: "secretary", condition: "by-locative", sentence: "The secretary was tripping by the drinking fountain."},
    {item: "secretary", condition: "non-by-locative", sentence: "The secretary has tripped near the drinking fountain."},
    {item: "worker", condition: "passive", sentence: "The construction worker was hit by the bulldozer."},
    {item: "worker", condition: "active", sentence: "The construction worker drove the bulldozer."},
    {item: "worker", condition: "by-locative", sentence: "The construction worker was digging by the bulldozer."},
    {item: "worker", condition: "non-by-locative", sentence: "The construction worker has dug with the bulldozer."},
    {item: "graduate", condition: "passive", sentence: "The new graduate was hired by the software company."},
    {item: "graduate", condition: "active", sentence: "The new graduate joined the software company."},
    {item: "graduate", condition: "by-locative", sentence: "The new graduate was driving by the software company."},
    {item: "graduate", condition: "non-by-locative", sentence: "The new graduate has driven around the software company."},
    {item: "ship", condition: "passive", sentence: "The ship was damaged by the pier."},
    {item: "ship", condition: "active", sentence: "The ship approached the pier."},
    {item: "ship", condition: "by-locative", sentence: "The ship was docking by the pier."},
    {item: "ship", condition: "non-by-locative", sentence: "The ship has docked at the pier."},
    {item: "minister", condition: "passive", sentence: "The minister was cut by the broken stained glass window."},
    {item: "minister", condition: "active", sentence: "The minister fixed the broken stained glass window." },
    {item: "minister", condition: "by-locative", sentence: "The minister was praying by the broken stained glass window."},
    {item: "minister", condition: "non-by-locative", sentence: "The minister has prayed below the broken stained glass wind"},
    {item: "engineers", condition: "passive", sentence: "The engineers were appalled by the monument."},
    {item: "engineers", condition: "active", sentence: "The engineers criticized the monument."},
    {item: "engineers", condition: "by-locative", sentence: "The engineers were conferring by the monument."},
    {item: "engineers", condition: "non-by-locative", sentence: "The engineers have conferred at the monument."},
    {item: "lumberjack", condition: "passive", sentence: "The lumberjack was struck by the giant redwood tree."},
    {item: "lumberjack", condition: "active", sentence: "The lumberjack struck the giant redwood tree."},
    {item: "lumberjack", condition: "by-locative", sentence: "The lumberjack was resting by the giant redwood tree."},
    {item: "lumberjack", condition: "non-by-locative", sentence: "The lumberjack has rested inside the giant redwood tree."},
    {item: "students", condition: "passive", sentence: "The students were bankrupted by the new sports complex."},
    {item: "students", condition: "active", sentence: "The students tried the new sports complex."},
    {item: "students", condition: "by-locative", sentence: "The students were working by the new sports complex."},
    {item: "students", condition: "non-by-locative", sentence: "The students have worked in the new sports complex."},
    {item: "747", condition: "passive", sentence: "The 747 was radioed by the airport control tower."},
    {item: "747", condition: "active", sentence: "The 747 radioed the airport control tower."},
    {item: "747", condition: "by-locative", sentence: "The 747 was landing by the airport control tower."},
    {item: "747", condition: "non-by-locative", sentence: "The 747 has landed near the airport control tower."},
    {item: "geologist", condition: "passive", sentence: "The missing geologist was smothered by the volcano."},
    {item: "geologist", condition: "active", sentence: "The missing geologist underestimated the volcano."},
    {item: "geologist", condition: "by-locative", sentence: "The missing geologist was wandering by the volcano."},
    {item: "geologist", condition: "non-by-locative", sentence: "The missing geologist has wandered into the volcano."},
    {item: "scouts", condition: "passive", sentence: "The Cub Scouts were warmed by the campfire."},
    {item: "scouts", condition: "active", sentence: "The Cub Scouts enjoyed the camp fire."},
    {item: "scouts", condition: "by-locative", sentence: "The Cub Scouts were singing by the campfire."},
    {item: "scouts", condition: "non-by-locative", sentence: "The Cub Scouts have sung around the campfire."},
    {item: "princess", condition: "passive", sentence: "The princess was delighted by the palace's old gate."},
    {item: "princess", condition: "active", sentence: "The princess renovated the palace's old gate."},
    {item: "princess", condition: "by-locative", sentence: "The princess was daydreaming by the palace's old gate."},
    {item: "princess", condition: "non-by-locative", sentence: "The princess has daydreamed under the palace's old gate."},
    {item: "stockbroker", condition: "passive", sentence: "The stockbroker was sued by the client."},
    {item: "stockbroker", condition: "active", sentence: "The stockbroker impressed the client."},
    {item: "stockbroker", condition: "by-locative", sentence: "The stockbroker was sitting by the client."},
    {item: "stockbroker", condition: "non-by-locative", sentence: "The stockbroker has sat opposite the client."},
    {item: "businessman", condition: "passive", sentence: "The businessman was paged by the airline ticket counter."},
    {item: "businessman", condition: "active", sentence: "The businessman left the airline ticket counter."},
    {item: "businessman", condition: "by-locative", sentence: "The businessman was waiting by the airline ticket counter."},
    {item: "businessman", condition: "non-by-locative", sentence: "The businessman has waited behind the airline ticket count."},
    {item: "scientist", condition: "passive", sentence: "The scientist was inspired by the apple tree."},
    {item: "scientist", condition: "active", sentence: "The scientist examined the apple tree."},
    {item: "scientist", condition: "by-locative", sentence: "The scientist was sleeping by the apple tree."},
    {item: "scientist", condition: "non-by-locative", sentence: "The scientist has slept under the apple tree."},
    {item: "surfer", condition: "passive", sentence: "The surfer was excited by the stormy sea."},
    {item: "surfer", condition: "active", sentence: "The surfer watched the stormy sea."},
    {item: "surfer", condition: "by-locative", sentence: "The surfer was sprinting by the stormy sea."},
    {item: "surfer", condition: "non-by-locative", sentence: "The surfer has sprinted along the stormy sea."},
    {item: "patron", condition: "passive", sentence: "The patron was annoyed by the jukebox in the bar."},
    {item: "patron", condition: "active", sentence: "The patron destroyed the jukebox in the bar."},
    {item: "patron", condition: "by-locative", sentence: "The patron was drinking by the jukebox in the bar."},
    {item: "patron", condition: "non-by-locative", sentence: "The patron has drunk at the jukebox in the bar."},
    {item: "lady", condition: "passive", sentence: "The bag lady was caught by the revolving door."},
    {item: "lady", condition: "active", sentence: "The bag lady stopped the revolving door."},
    {item: "lady", condition: "by-locative", sentence: "The bag lady was falling by the revolving door."},
    {item: "lady", condition: "non-by-locative", sentence: "The bag lady has fallen in the revolving door."},
    {item: "dictator", condition: "passive", sentence: "The dictator was overthrown by the general."},
    {item: "dictator", condition: "active", sentence: "The dictator trusted the general."},
    {item: "dictator", condition: "by-locative", sentence: "The dictator was standing by the general."},
    {item: "dictator", condition: "non-by-locative", sentence: "The dictator has stood behind the general."},
    {item: "children", condition: "passive", sentence: "The children were deafened by the church organ."},
    {item: "children", condition: "active", sentence: "The children disliked the church organ."},
    {item: "children", condition: "by-locative", sentence: "The children were playing by the church organ."},
    {item: "children", condition: "non-by-locative", sentence: "The children have played beside the church organ."},
    {item: "fishermen", condition: "passive", sentence: "The fishermen were startled by the buoy."},
    {item: "fishermen", condition: "active", sentence: "The fishermen damaged the buoy."},
    {item: "fishermen", condition: "by-locative", sentence: "The fishermen were fishing by the buoy."},
    {item: "fishermen", condition: "non-by-locative", sentence: "The fishermen have fished at the buoy."},
    {item: "young_woman", condition: "passive", sentence: "The young woman was calmed by the lake."},
    {item: "young_woman", condition: "active", sentence: "The young woman admired the lake."},
    {item: "young_woman", condition: "by-locative", sentence: "The young woman was walking by the lake."},
    {item: "young_woman", condition: "non-by-locative", sentence: "The young woman has walked along the lake."},
    {item: "bum", condition: "passive", sentence: "The bum was scratched by the bushes."},
    {item: "bum", condition: "active", sentence: "The bum circled the bushes."},
    {item: "bum", condition: "by-locative", sentence: "The bum was napping by the bushes."},
    {item: "bum", condition: "non-by-locative", sentence: "The bum has napped in the bushes."},
    {item: "dog", condition: "passive", sentence: "The dog was protected by the fence."},
    {item: "dog", condition: "active", sentence: "The dog jumped the fence."},
    {item: "dog", condition: "by-locative", sentence: "The dog was barking by the fence."},
    {item: "dog", condition: "non-by-locative", sentence: "The dog has barked behind the fence."},
    {item: "grandmother", condition: "passive", sentence: "The grandmother was pleased by the flowers."},
    {item: "grandmother", condition: "active", sentence: "The grandmother liked the flowers."},
    {item: "grandmother", condition: "by-locative", sentence: "The grandmother was sketching by the flowers."},
    {item: "grandmother", condition: "non-by-locative", sentence: "The grandmother has sketched near the flowers."},
    {item: "councilman", condition: "passive", sentence: "The councilman was impressed by the new building."},
    {item: "councilman", condition: "active", sentence: "The councilman opened the new building."},
    {item: "councilman", condition: "by-locative", sentence: "The councilman was strolling by the new building."},
    {item: "councilman", condition: "non-by-locative", sentence: "The councilman has strolled past the new building."},
    {item: "nymphs", condition: "passive", sentence: "The nymphs were soaked by the waterfall."},
    {item: "nymphs", condition: "active", sentence: "The nymphs saw the waterfall."},
    {item: "nymphs", condition: "by-locative", sentence: "The nymphs were bathing by the waterfall."},
    {item: "nymphs", condition: "non-by-locative", sentence: "The nymphs have bathed under the waterfall."}
  ];

