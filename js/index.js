function make_slides(f) {
  var   slides = {};

  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.one_slider = slide({
    name : "one_slider",

    /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.) */
    present :_.shuffle([
      {item: "horse", condition: "ambiguous", sentence: "The horse raced past the barn fell."},
      {item: "horse", condition: "unambiguous", sentence: "The horse that raced past the barn fell."},
      {item: "practice", condition: "ambiguous", sentence: "practice1"},
      {item: "practice", condition: "unambiguous", sentence: "practice2"},
    ]),

    /* It might be the case that the array of things you want to present depends
    on the condition. A solution is to define the array when the condition
    is determined, e.g., in init().*/
    // present: _.shuffle(exp.conditionalStims),

    //this gets run only at the beginning of the block
    present_handle : function(stim) {
      $(".err").hide();

      //console.log(exp.condition)
     
      this.stim = stim; //I like to store this information in the slide so I can record it later.
      $(".prompt").html(stim.sentence);

      this.init_sliders();
      exp.sliderPost = null; //erase current slider value
    },

    button : function() {
      if (exp.sliderPost == null) {
        $(".err").show();
      } else {
        this.log_responses();

        /* use _stream.apply(this); if and only if there is
        "present" data. (and only *after* responses are logged) */
        _stream.apply(this);
      }
    },

    init_sliders : function() {
      utils.make_slider("#single_slider", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },

    log_responses : function() {
      exp.data_trials.push({
        "item" : this.stim.item,
        "condition": this.stim.condition,
        "response" : exp.sliderPost
      });
    }
  });

  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
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
  exp.trials = [];
  exp.catch_trials = [];
  exp.condition = _.sample(["condition 1", "condition 2"]); //can randomize between subject conditions here

  // exp.conditionalStims = {"condition 1": [item: "horse", condition: "ambiguous", sentence: "The horse raced past the barn fell.",
  //                                       item: "horse", condition: "unambiguous", sentence: "The horse that raced past the barn fell."],
  //                         "condition 2": [item: "practice", condition: "ambiguous", sentence: "practice1",
  //                                       item: "practice", condition: "unambiguous", sentence: "practice2"]
  //                 }[exp.condition];

  // exp.conditionalStims = {"condition 1": ["The horse raced past the barn fell.",
  //                                         "The horse that raced past the barn fell."],
  //                         "condition 2": ["practice1",
  //                                         "practice2"]
  //                         }[exp.condition];

  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["i0", "instructions", "one_slider", 'subj_info', 'thanks'];
  // exp.structure=["i0", "instructions", "single_trial", "one_slider", 'subj_info', 'thanks'];

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  //exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
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
