function make_slides(f) {
  var   slides = {};

slides.bot = slide({
name: "bot",
start: function () {
  $('.err_msg').hide();
  exp.speaker = _.shuffle(["James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles"])[0];
  exp.listener = _.shuffle(["Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Margaret"])[0];
  exp.lives = 0;
  var story = exp.speaker + ' says to ' + exp.listener + ': "It\'s a beautiful day, isn\'t it?"'
  var question = 'Who does ' + exp.speaker + ' talk to?';
  document.getElementById("s").innerHTML = story;
  document.getElementById("q").innerHTML = question;
},
button: function () {
  var textInput = document.getElementById("text_box").value;
  var listenerLowerCase = exp.listener.toLowerCase();
  var textInputLowerCase = textInput.toLowerCase();

  exp.data_trials.push({
    "slide_number_in_experiment": "bot_check",
    "stim": exp.lives,
    "response": textInput
  });

  if ((exp.lives < 3) && (textInputLowerCase === listenerLowerCase)) {
    exp.go();
  }
  else {
    $('.err_msg').hide();
    switch (exp.lives) {
      case 0:
        $('#err1').show();
        break;
      case 1:
        $('#err2').show();
        break;
      case 2:
        $('#disq').show();
        $('.button').hide();
        break;
      default:
        break;
    }
    exp.lives++;
  }
},
});

  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions = slide({
    name : "instructions",
    start: function() {
      $(".practice_err_text").hide();
    },
    button : function() {
      practice_text_response = $("#practice_response").val();
      if (practice_text_response.length < 5) {
        $(".practice_err_text").show();
      } else {
        exp.practice_trial = practice_text_response; 
        exp.go(); //use exp.go() if and only if there is no "present" data.
      }
    }
  });

  slides.instructions2 = slide({
    name: "instructions2",
    button: function() {
      exp.go();
    }
  });

  slides.trial = slide({
    name: "trial",
    start: function() {

    },
    present: exp.stims,
    present_handle: function(stim) {
      $('input[name="sentence_seen"]').prop("checked", false);
      $("#sentence_response").val(''); //j-query 
      $('input[name="picture_seen"]').prop("checked", false);
      $("#picture_response").val('');
      $("#sentence_trial").show();
      $(".sentence_err_text").hide();
      $(".sentence_err_button").hide();
      
      this.stim = stim;
      $(".sentence_prompt").html(stim.sentence.bold());
      if (this.stim.trial === "target") {
        $("#target_picture").attr("src","pictures/target_pictures/T" + stim.picture_id.toString() + ".jpg");
      } else {
        $("#target_picture").attr("src","pictures/filler_pictures/F" + stim.picture_id.toString() + ".jpg");
      }
      $("#picture_trial").hide();
    },

    second_slide : function() {
      exp.sent_text_response = $("#sentence_response").val();
      exp.sent_button_response = $('input[name="sentence_seen"]:checked').val();
      if (exp.sent_text_response.length < 5) {
        $(".sentence_err_text").show();
      } else if (exp.sent_button_response == null)  {
        $(".sentence_err_button").show();
      } else {
        $("#sentence_trial").hide();
        $("#picture_trial").show();
        $(".picture_err_text").hide();
        $(".picture_err_button").hide();
      }
    }, 

    button : function() {
      pic_text_response = $("#picture_response").val();
      pic_button_response = $('input[name="picture_seen"]:checked').val();
      if (pic_text_response.length < 5) {
        $(".picture_err_text").show();
      } else if(pic_button_response == null) {
        $(".picture_err_button").show();
      } else {
        exp.data_trials.push({
          "trial_type" : this.stim.trial,
          "slide_number" : exp.phase, 
          "condition" : this.stim.condition,
          "item": this.stim.item,
          "picture": this.stim.picture_id,
          "sentence" : this.stim.sentence,
          "response" : [exp.sent_text_response, 
                        exp.sent_button_response, 
                        pic_text_response, 
                        pic_button_response] 
        });
        _stream.apply(this);
        //exp.go();
      }
    },

  });

  
  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        asses : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        comments : $("#comments").val(),
        problems: $("#problems").val(),
        fairprice: $("#fairprice").val()
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "practice": exp.practice_trial,
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {
   $(document).ready(function(){
   var ut_id = "epetsen-ziegleretal2019replication";
   if (UTWorkerLimitReached(ut_id)) {
     $(".slide").hide();
     $("body").html("You have already completed the maximum number of HITs allowed by this requester. Please click 'Return HIT' to avoid any impact on your approval rating.");
}});

  exp.trials = [];
  exp.catch_trials = [];
  exp.condition = _.sample(["passive", "by-locative", "non-by-locative"]); //can randomize between subject conditions here
  
  var items = [
    {trial: "target", item: "senator", condition: "passive", sentence: "The senator was awed by the statue."},
    {trial: "target", item: "senator", condition: "active", sentence: "The senator unveiled the statue."},
    {trial: "target", item: "senator", condition: "by-locative", sentence: "The senator was speaking by the statue."},
    {trial: "target", item: "senator", condition: "non-by-locative", sentence: "The senator has spoken about the statue."},
    {trial: "target", item: "woman", condition: "passive", sentence: "The woman was stung by the jellyfish."},
    {trial: "target", item: "woman", condition: "active", sentence: "The woman caught the jellyfish."},
    {trial: "target", item: "woman", condition: "by-locative", sentence: "The woman was swimming by the jellyfish."},
    {trial: "target", item: "woman", condition: "non-by-locative", sentence: "The woman has swum into the jellyfish."},
    {trial: "target", item: "prisoner", condition: "passive", sentence: "The escaping prisoner was illuminated by the guard tower."},
    {trial: "target", item: "prisoner", condition: "active", sentence: "The escaping prisoner avoided the guard tower."},
    {trial: "target", item: "prisoner", condition: "by-locative", sentence: "The escaping prisoner was hiding by the guard tower."},
    {trial: "target", item: "prisoner", condition: "non-by-locative", sentence: "The escaping prisoner has hidden below the guard tower."},
    {trial: "target", item: "foreigner", condition: "passive", sentence: "The foreigner was confused by the blinking traffic light."},
    {trial: "target", item: "foreigner", condition: "active", sentence: "The foreigner misunderstood the blinking traffic light."},
    {trial: "target", item: "foreigner", condition: "by-locative", sentence: "The foreigner was loitering by the blinking traffic light."},
    {trial: "target", item: "foreigner", condition: "non-by-locative", sentence: "The foreigner has loitered at the blinking traffic light."},
    {trial: "target", item: "Dalmatian", condition: "passive", sentence: "The Dalmatian was pursued by the fire truck."},
    {trial: "target", item: "Dalmatian", condition: "active", sentence: "The Dalmatian chased the fire truck."},
    {trial: "target", item: "Dalmatian", condition: "by-locative", sentence: "The Dalmatian was running by the fire truck."},
    {trial: "target", item: "Dalmatian", condition: "non-by-locative", sentence: "The Dalmatian has run around the fire truck."},
    {trial: "target", item: "secretary", condition: "passive", sentence: "The secretary was splashed by the drinking fountain."},
    {trial: "target", item: "secretary", condition: "active", sentence: "The secretary cleaned the drinking fountain."},
    {trial: "target", item: "secretary", condition: "by-locative", sentence: "The secretary was tripping by the drinking fountain."},
    {trial: "target", item: "secretary", condition: "non-by-locative", sentence: "The secretary has tripped near the drinking fountain."},
    {trial: "target", item: "worker", condition: "passive", sentence: "The construction worker was hit by the bulldozer."},
    {trial: "target", item: "worker", condition: "active", sentence: "The construction worker drove the bulldozer."},
    {trial: "target", item: "worker", condition: "by-locative", sentence: "The construction worker was digging by the bulldozer."},
    {trial: "target", item: "worker", condition: "non-by-locative", sentence: "The construction worker has dug with the bulldozer."},
    {trial: "target", item: "graduate", condition: "passive", sentence: "The new graduate was hired by the software company."},
    {trial: "target", item: "graduate", condition: "active", sentence: "The new graduate joined the software company."},
    {trial: "target", item: "graduate", condition: "by-locative", sentence: "The new graduate was driving by the software company."},
    {trial: "target", item: "graduate", condition: "non-by-locative", sentence: "The new graduate has driven around the software company."},
    {trial: "target", item: "ship", condition: "passive", sentence: "The ship was damaged by the pier."},
    {trial: "target", item: "ship", condition: "active", sentence: "The ship approached the pier."},
    {trial: "target", item: "ship", condition: "by-locative", sentence: "The ship was docking by the pier."},
    {trial: "target", item: "ship", condition: "non-by-locative", sentence: "The ship has docked at the pier."},
    {trial: "target", item: "minister", condition: "passive", sentence: "The minister was cut by the broken stained glass window."},
    {trial: "target", item: "minister", condition: "active", sentence: "The minister fixed the broken stained glass window." },
    {trial: "target", item: "minister", condition: "by-locative", sentence: "The minister was praying by the broken stained glass window."},
    {trial: "target", item: "minister", condition: "non-by-locative", sentence: "The minister has prayed below the broken stained glass wind"},
    {trial: "target", item: "engineers", condition: "passive", sentence: "The engineers were appalled by the monument."},
    {trial: "target", item: "engineers", condition: "active", sentence: "The engineers criticized the monument."},
    {trial: "target", item: "engineers", condition: "by-locative", sentence: "The engineers were conferring by the monument."},
    {trial: "target", item: "engineers", condition: "non-by-locative", sentence: "The engineers have conferred at the monument."},
    {trial: "target", item: "lumberjack", condition: "passive", sentence: "The lumberjack was struck by the giant redwood tree."},
    {trial: "target", item: "lumberjack", condition: "active", sentence: "The lumberjack struck the giant redwood tree."},
    {trial: "target", item: "lumberjack", condition: "by-locative", sentence: "The lumberjack was resting by the giant redwood tree."},
    {trial: "target", item: "lumberjack", condition: "non-by-locative", sentence: "The lumberjack has rested inside the giant redwood tree."},
    {trial: "target", item: "students", condition: "passive", sentence: "The students were bankrupted by the new sports complex."},
    {trial: "target", item: "students", condition: "active", sentence: "The students tried the new sports complex."},
    {trial: "target", item: "students", condition: "by-locative", sentence: "The students were working by the new sports complex."},
    {trial: "target", item: "students", condition: "non-by-locative", sentence: "The students have worked in the new sports complex."},
    {trial: "target", item: "747", condition: "passive", sentence: "The 747 was radioed by the airport control tower."},
    {trial: "target", item: "747", condition: "active", sentence: "The 747 radioed the airport control tower."},
    {trial: "target", item: "747", condition: "by-locative", sentence: "The 747 was landing by the airport control tower."},
    {trial: "target", item: "747", condition: "non-by-locative", sentence: "The 747 has landed near the airport control tower."},
    {trial: "target", item: "geologist", condition: "passive", sentence: "The missing geologist was smothered by the volcano."},
    {trial: "target", item: "geologist", condition: "active", sentence: "The missing geologist underestimated the volcano."},
    {trial: "target", item: "geologist", condition: "by-locative", sentence: "The missing geologist was wandering by the volcano."},
    {trial: "target", item: "geologist", condition: "non-by-locative", sentence: "The missing geologist has wandered into the volcano."},
    {trial: "target", item: "scouts", condition: "passive", sentence: "The Cub Scouts were warmed by the campfire."},
    {trial: "target", item: "scouts", condition: "active", sentence: "The Cub Scouts enjoyed the camp fire."},
    {trial: "target", item: "scouts", condition: "by-locative", sentence: "The Cub Scouts were singing by the campfire."},
    {trial: "target", item: "scouts", condition: "non-by-locative", sentence: "The Cub Scouts have sung around the campfire."},
    {trial: "target", item: "princess", condition: "passive", sentence: "The princess was delighted by the palace's old gate."},
    {trial: "target", item: "princess", condition: "active", sentence: "The princess renovated the palace's old gate."},
    {trial: "target", item: "princess", condition: "by-locative", sentence: "The princess was daydreaming by the palace's old gate."},
    {trial: "target", item: "princess", condition: "non-by-locative", sentence: "The princess has daydreamed under the palace's old gate."},
    {trial: "target", item: "stockbroker", condition: "passive", sentence: "The stockbroker was sued by the client."},
    {trial: "target", item: "stockbroker", condition: "active", sentence: "The stockbroker impressed the client."},
    {trial: "target", item: "stockbroker", condition: "by-locative", sentence: "The stockbroker was sitting by the client."},
    {trial: "target", item: "stockbroker", condition: "non-by-locative", sentence: "The stockbroker has sat opposite the client."},
    {trial: "target", item: "businessman", condition: "passive", sentence: "The businessman was paged by the airline ticket counter."},
    {trial: "target", item: "businessman", condition: "active", sentence: "The businessman left the airline ticket counter."},
    {trial: "target", item: "businessman", condition: "by-locative", sentence: "The businessman was waiting by the airline ticket counter."},
    {trial: "target", item: "businessman", condition: "non-by-locative", sentence: "The businessman has waited behind the airline ticket count."},
    {trial: "target", item: "scientist", condition: "passive", sentence: "The scientist was inspired by the apple tree."},
    {trial: "target", item: "scientist", condition: "active", sentence: "The scientist examined the apple tree."},
    {trial: "target", item: "scientist", condition: "by-locative", sentence: "The scientist was sleeping by the apple tree."},
    {trial: "target", item: "scientist", condition: "non-by-locative", sentence: "The scientist has slept under the apple tree."},
    {trial: "target", item: "surfer", condition: "passive", sentence: "The surfer was excited by the stormy sea."},
    {trial: "target", item: "surfer", condition: "active", sentence: "The surfer watched the stormy sea."},
    {trial: "target", item: "surfer", condition: "by-locative", sentence: "The surfer was sprinting by the stormy sea."},
    {trial: "target", item: "surfer", condition: "non-by-locative", sentence: "The surfer has sprinted along the stormy sea."},
    {trial: "target", item: "patron", condition: "passive", sentence: "The patron was annoyed by the jukebox in the bar."},
    {trial: "target", item: "patron", condition: "active", sentence: "The patron destroyed the jukebox in the bar."},
    {trial: "target", item: "patron", condition: "by-locative", sentence: "The patron was drinking by the jukebox in the bar."},
    {trial: "target", item: "patron", condition: "non-by-locative", sentence: "The patron has drunk at the jukebox in the bar."},
    {trial: "target", item: "lady", condition: "passive", sentence: "The bag lady was caught by the revolving door."},
    {trial: "target", item: "lady", condition: "active", sentence: "The bag lady stopped the revolving door."},
    {trial: "target", item: "lady", condition: "by-locative", sentence: "The bag lady was falling by the revolving door."},
    {trial: "target", item: "lady", condition: "non-by-locative", sentence: "The bag lady has fallen in the revolving door."},
    {trial: "target", item: "dictator", condition: "passive", sentence: "The dictator was overthrown by the general."},
    {trial: "target", item: "dictator", condition: "active", sentence: "The dictator trusted the general."},
    {trial: "target", item: "dictator", condition: "by-locative", sentence: "The dictator was standing by the general."},
    {trial: "target", item: "dictator", condition: "non-by-locative", sentence: "The dictator has stood behind the general."},
    {trial: "target", item: "children", condition: "passive", sentence: "The children were deafened by the church organ."},
    {trial: "target", item: "children", condition: "active", sentence: "The children disliked the church organ."},
    {trial: "target", item: "children", condition: "by-locative", sentence: "The children were playing by the church organ."},
    {trial: "target", item: "children", condition: "non-by-locative", sentence: "The children have played beside the church organ."},
    {trial: "target", item: "fishermen", condition: "passive", sentence: "The fishermen were startled by the buoy."},
    {trial: "target", item: "fishermen", condition: "active", sentence: "The fishermen damaged the buoy."},
    {trial: "target", item: "fishermen", condition: "by-locative", sentence: "The fishermen were fishing by the buoy."},
    {trial: "target", item: "fishermen", condition: "non-by-locative", sentence: "The fishermen have fished at the buoy."},
    {trial: "target", item: "young_woman", condition: "passive", sentence: "The young woman was calmed by the lake."},
    {trial: "target", item: "young_woman", condition: "active", sentence: "The young woman admired the lake."},
    {trial: "target", item: "young_woman", condition: "by-locative", sentence: "The young woman was walking by the lake."},
    {trial: "target", item: "young_woman", condition: "non-by-locative", sentence: "The young woman has walked along the lake."},
    {trial: "target", item: "bum", condition: "passive", sentence: "The bum was scratched by the bushes."},
    {trial: "target", item: "bum", condition: "active", sentence: "The bum circled the bushes."},
    {trial: "target", item: "bum", condition: "by-locative", sentence: "The bum was napping by the bushes."},
    {trial: "target", item: "bum", condition: "non-by-locative", sentence: "The bum has napped in the bushes."},
    {trial: "target", item: "dog", condition: "passive", sentence: "The dog was protected by the fence."},
    {trial: "target", item: "dog", condition: "active", sentence: "The dog jumped the fence."},
    {trial: "target", item: "dog", condition: "by-locative", sentence: "The dog was barking by the fence."},
    {trial: "target", item: "dog", condition: "non-by-locative", sentence: "The dog has barked behind the fence."},
    {trial: "target", item: "grandmother", condition: "passive", sentence: "The grandmother was pleased by the flowers."},
    {trial: "target", item: "grandmother", condition: "active", sentence: "The grandmother liked the flowers."},
    {trial: "target", item: "grandmother", condition: "by-locative", sentence: "The grandmother was sketching by the flowers."},
    {trial: "target", item: "grandmother", condition: "non-by-locative", sentence: "The grandmother has sketched near the flowers."},
    {trial: "target", item: "councilman", condition: "passive", sentence: "The councilman was impressed by the new building."},
    {trial: "target", item: "councilman", condition: "active", sentence: "The councilman opened the new building."},
    {trial: "target", item: "councilman", condition: "by-locative", sentence: "The councilman was strolling by the new building."},
    {trial: "target", item: "councilman", condition: "non-by-locative", sentence: "The councilman has strolled past the new building."},
    {trial: "target", item: "nymphs", condition: "passive", sentence: "The nymphs were soaked by the waterfall."},
    {trial: "target", item: "nymphs", condition: "active", sentence: "The nymphs saw the waterfall."},
    {trial: "target", item: "nymphs", condition: "by-locative", sentence: "The nymphs were bathing by the waterfall."},
    {trial: "target", item: "nymphs", condition: "non-by-locative", sentence: "The nymphs have bathed under the waterfall."}
  ]; 

  var item_names = _.shuffle(["senator", "woman", "prisoner", "foreigner", "Dalmatian", 
                              "secretary", "worker", "graduate", "ship", "minister", 
                              "engineers", "lumberjack", "students", "747", "geologist",
                              "scouts", "princess", "stockbroker", "businessman", "scientist",
                              "surfer", "patron", "lady", "dictator", "children", "fishermen",
                              "young_woman", "bum", "dog", "grandmother", "councilman", "nymphs"]);

  var active_condition = item_names.slice(0,16);

  var non_active_condition = item_names.slice(16,);
  
  var number_sequence = $.map($(Array(32)), function(v, i) {return i+1;})
  
  var image_number = _.shuffle(number_sequence);

  var target_stims = [];

  for(item_entry of items) {
    if (active_condition.includes(item_entry.item) && item_entry.condition === "active") {
      item_entry.picture_id = image_number.pop();
      target_stims.push(item_entry);
    } else if (non_active_condition.includes(item_entry.item) && item_entry.condition === exp.condition) {
      item_entry.picture_id = image_number.pop();
      target_stims.push(item_entry);
    } 
  }

  target_stims = _.shuffle(target_stims);
  //exp.stims = _.shuffle(target_stims);

  var filler_sentences = [    
    {trial: "filler", item: "man", sentence:"The man looked at himself in the mirror.", type: "reflexive1", source: "orig"},
    {trial: "filler", item: "basketball_players", sentence: "They basketball players tripped themselves on the court.", type: "reflexive2", source: "orig"},
    {trial: "filler", item: "little_girl", sentence: "The little girl splashed herself in the puddle.", type: "reflexive3", source: "orig"},
    {trial: "filler", item: "freezing_rain", sentence: "The freezing rain made the streets slippery.", type: "causative1", source: "orig"},
    {trial: "filler", item: "thinking", sentence: "Thinking too hard makes me tired.", type: "causative2", source: "orig"},
    {trial: "filler", item: "old_lady", sentence: "It was an old lady who discovered the weapon.", type: "cleft1", source: "orig"},
    {trial: "filler", item: "stray_cat", sentence: "It was a stray cat that was on the fire escape.", type: "cleft2", source: "orig"},
    {trial: "filler", item: "old_friend", sentence: "It was my old friend who was in the parade.", type: "cleft3", source: "orig"},
    {trial: "filler", item: "red_spot", sentence: "There is a red spot on Jupiter.", type: "existential1", source: "orig"},
    {trial: "filler", item: "seven_days", sentence: "There are seven days in a week.", type: "existential2", source: "orig"},
    {trial: "filler", item: "life", sentence: "There is no life on Mars.", type: "existential3", source: "orig"},
    {trial: "filler", item: "girl", sentence: "The girl laughed herself silly.", type: "resultative-reflexive1", source: "orig"},
    {trial: "filler", item: "little_boy", sentence: "The little boy scrubbed himself clean.", type: "resultative-reflexive2", source: "orig"},
    {trial: "filler", item: "women", sentence: "The women sat themselves down at the far table.", type: "resultative-reflexive3", source: "orig"},
    {trial: "filler", item: "humans", sentence: "All humans are animals.", type: "generic1", source: "orig"},
    {trial: "filler", item: "Argentina", sentence: "Argentina is in South America.", type: "generic2", source: "orig"},
    {trial: "filler", item: "Mercury", sentence: "Mercury is the closest planet to the sun.", type: "generic3", source: "orig"},
    {trial: "filler", item: "running", sentence: "Running is a form of exercise.", type: "generic4", source: "orig"},
    {trial: "filler", item: "mechanic", sentence: "The mechanic brought the driver a jumper cable.", type: "ditransitive1", source: "orig"},
    {trial: "filler", item: "coach", sentence: "The coach handed the player a towel.", type: "ditransitive2", source: "orig"},
    {trial: "filler", item: "man2", sentence: "The man looked at himself in the mirror.", type: "reflexive1", source: "rep"},
    {trial: "filler", item: "basketball_players2", sentence: "They basketball players tripped themselves on the court.", type: "reflexive2", source: "rep"},
    {trial: "filler", item: "freezing_rain2", sentence: "The freezing rain made the streets slippery.", type: "causative1", source: "rep"},
    {trial: "filler", item: "old_lady2", sentence: "It was an old lady who discovered the weapon.", type: "cleft1", source: "rep"},
    {trial: "filler", item: "stray_cat2", sentence: "It was a stray cat that was on the fire escape.", type: "cleft2", source: "rep"},
    {trial: "filler", item: "red_spot2", sentence: "There is a red spot on Jupiter.", type: "existential1", source: "rep"},
    {trial: "filler", item: "seven_days2", sentence: "There are seven days in a week.", type: "existential2", source: "rep"},
    {trial: "filler", item: "girl2", sentence: "The girl laughed herself silly.", type: "resultative-reflexive1", source: "rep"},
    {trial: "filler", item: "little_boy2", sentence: "The little boy scrubbed himself clean.", type: "resultative-reflexive2", source: "rep"},
    {trial: "filler", item: "humans2", sentence: "All humans are animals.", type: "generic1", source: "rep"},
    {trial: "filler", item: "Argentina2", sentence: "Argentina is in South America.", type: "generic2", source: "rep"},
    {trial: "filler", item: "mechanic2", sentence: "The mechanic brought the driver a jumper cable.", type: "ditransitive1", source: "rep"}
];

var number_sequence_fillers = $.map($(Array(32)), function(v, i) {return i+1;});
//console.log(number_sequence_fillers);

var image_number_fillers = _.shuffle(number_sequence_fillers);

var filler_stims = [];

for (item_entry of filler_sentences) {
	item_entry.picture_id = image_number_fillers.pop();
  filler_stims.push(item_entry);
}

filler_stims = _.shuffle(filler_stims);

//length
var sentences_length = [];
for (item of items) {
  var l = item.sentence.length;
  sentences_length.push(l);
}

//console.log(sentences_length);
//console.log(Math.min(...sentences_length));

for (stim of filler_sentences) {
  var l = stim.sentence.length;
  sentences_length.push(l);
}

console.log(sentences_length);
console.log(Math.min(...sentences_length));

//five_fillers = filler_stims.splice(0, 5);

//another file 
//8 lists 

//var number_fillers = [];

//for (i = 0; i < 32; i++) {
 // var random_number = Math.random() < 0.5 ? 1 : 2;
 // number_fillers.push(random_number);
//}

var target_filler_stims = [];

for (stim of target_stims) {
  //var n = number_fillers.pop();
  var filler = filler_stims.pop();
  target_filler_stims.push(filler,stim);
  //target_filler_stims.push(stim);
  //target_filler_stims = target_filler_stims.concat(fillers);
}

//console.log(target_filler_stims);

//target_filler_stims = five_fillers.concat(target_filler_stims, filler_stims);
exp.stims = target_filler_stims;
console.log(exp.stims);
//exp.stims = _.shuffle(target_filler_stims);

  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["bot", "i0", "instructions", "instructions2", "trial", 'subj_info', 'thanks'];

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}

// from Github page go to Settings, github pages, non -> master branch, wait, refresh page, git hub pages,  
