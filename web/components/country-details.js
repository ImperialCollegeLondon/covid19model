const CountryDetails = Vue.component('country-details', {
    props: ['selected_country'],
    data() {
        return {
            interventions: null,
            latest_updates: null,
            isLoading: true
        }
    },
    created() {
        this.fetchData()
    },
    methods: {
        async fetchData() {
            this.latest_updates = await fetch('data/latest-updates.json')
                .then((response) => {
                    return response.json();
                });

            this.interventions = await fetch('data/timeline-of-interventions.json')
                .then((response) => {
                    return response.json();
                });

            this.isLoading = false
        }
    },
    template: `
    <section id="country-details" v-if="!isLoading">
    <h1 class="text-center mb-4 mt-5" id="country-title">{{ selected_country | pretty-name }}</h1 class="text-center">

    <div class="container">
        <div class="row mt-4">
            <div class="col-md">
                <h4 class="text-center mb-3">Daily number of infections</h4>
                <responsive-figure v-bind:country="selected_country" type="infections" alt="Daily number of infections"></responsive-figure>
                <p class="small">
                    Daily number of infections, brown bars are reported infections, blue bands are predicted infections,
                    dark blue 50% credible interval
                    (CI), light blue 95% CI.
                </p>
                <p class="small">
                    The number of daily infections estimated by our model drops immediately after an
                    intervention, as we assume that all infected people become immediately less infectious through the
                    intervention. Afterwards, if the R<sub>t</sub> is above 1, the number of infections will starts
                    growing again.
                </p>
            </div>
            <div class="col-md">
                <h4 class="text-center mb-3">Deaths</h4>
                <responsive-figure v-bind:country="selected_country" type="deaths" alt="Deaths"></responsive-figure>
                <p class="small">Daily number of deaths, brown bars are reported deaths, blue bands are predicted
                    deaths, dark blue 50% credible interval (CI), light blue 95% CI.</p>
            </div>
        </div>
        <div class="row mt-5">
            <div class="col-md">
                <h4 class="text-center mb-3">Reproduction number R<sub>t</sub></h4>
                <responsive-figure v-bind:country="selected_country" type="rt" alt="Reproduction number Rt"></responsive-figure>
                
                <p class="small">Time-varying reproduction number R<sub>t</sub>
                    , dark green 50% CI, light green 95% CI.
                    Icons are interventions shown at the time they occurred.
                </p>
            </div>
        </div>
        <div class="row mt-5">
            <div class="col-md-8 offset-md-2">
                <h4 class="text-center mb-3">Forecast</h4>
                <responsive-figure v-bind:country="selected_country" type="forecast" alt="Forecast"></responsive-figure>
                <p class="small">The lag between deaths and infections means that
                    it takes time for information to propagate backwards from deaths to infections, and ultimately to
                    R<sub>t</sub>.
                    A conclusion of this report is the prediction of a slowing of R<sub>t</sub>
                    in response to major interventions. To
                    gain intuition that this is data driven and not simply a consequence of highly constrained model
                    assumptions, we show death forecasts on a log-linear scale. On this scale a line which curves below
                    a linear trend is indicative of slowing in the growth of the epidemic.
                </p>
            </div>
        </div>
        <div class="row justify-content-center mt-5" v-if="interventions[selected_country]">
            <div class="col-md-8">
                <h2 class="text-center w-100 mb-5 mt-4">Timeline of interventions</h2>
                <timeline-card v-for="intervention in interventions[selected_country]"
                    v-bind:date="intervention.date"
                    v-bind:title="intervention.name"
                    v-bind:text="intervention.description"
                    v-bind:sources="intervention.sources"
                    v-bind:key="intervention.type"></timeline-card>
            </div>
        </div>
        <div class="row mt-4">
            <div class="col text-center">
                <p>
                    Last updated <span id="last_update">{{ latest_updates[selected_country] }}</span>.
                    <br>
                    The results are updated daily. Any comments or suggestions, please contact the authors of the
                    <a target="_blank"
                        href="https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-Europe-estimates-and-NPI-impact-30-03-2020.pdf">paper</a>.
                </p>
            </div>
        </div>
    </div>
    </section>`
}); 