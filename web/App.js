var app = new Vue({
    el: '#covid19model',
    data: {
        latest_updates: {},
        interventions: {},
        selected_country: ""
    },
    computed: {
        // a computed getter
        countries: function () {
            return Object.keys(this.latest_updates).sort();
        }
    }
});

// Use async ajax in this case since the whole pages doesn't make sense if that data isn't loaded yet
$.ajaxSetup({
    async: false
});

$.getJSON("data/latest-updates.json", function (json) {
    app.latest_updates = json;
    app.selected_country = "Austria";
});

$.getJSON("data/timeline-of-interventions.json", function (json) {
    app.interventions = json;
});