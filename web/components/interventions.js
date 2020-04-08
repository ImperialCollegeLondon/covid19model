const Interventions = Vue.component("interventions", {
    data() {
        return {
            interventions: null,
            isLoading: true
        }
    },
    computed: {
        countries: function () {
            return Object.keys(this.interventions).sort();
        }
    },
    created() {
        this.fetchData()
    },
    methods: {
        async fetchData() {
            this.interventions = await fetch('data/timeline-of-interventions.json')
                .then((response) => {
                    return response.json();
                });

            this.isLoading = false
        }
    },
    template: `
    <div class="container" v-if="!isLoading">
        <h1 class="text-center mb-4 mt-5">Timeline of Interventions</h1>
        <section v-for="country in countries" class="mt-3">
            <div class="row mb-md-0 mb-1">
                <div class="col bg-primary text-white pt-3 pb-2 pl-3 pr-3">
                    <h3>{{ country }}</h3>
                </div>
            </div>
            <div class="row border-top border-primary" v-for="intervention in interventions[country]"
            v-bind:key="intervention.type">
                <div class="col-md-1 bg-primary text-white p-2">
                    <p class="my-auto">{{ intervention.date }}</p>
                </div>
                <div class="col-md-3 p-2">
                    <p><strong>{{ intervention.name }}</strong></p>
                </div>
                <div class="col-md-8 p-2">
                    {{ intervention.description }}
                    <p class="card-text mb-0">
                        <a data-toggle="collapse" v-bind:href="'#timeline-card-' + country + intervention.type" role="button"
                            aria-expanded="false" aria-controls="collapseExample">
                            <small>Source</small>
                        </a>
                    </p>
                    <div class="collapse" v-bind:id="'timeline-card-' + country + intervention.type">
                        <p class="card-text" v-for="source in intervention.sources">
                            <small class="text-muted">{{ source }}</small>
                        </p>
                    </div>
                </div>
            </div>
        </section>
    </div>
    `
});