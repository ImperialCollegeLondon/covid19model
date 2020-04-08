// ROUTING
const routes = [
    { // Default Route
        path: "/",
        redirect: "/details/Austria"
    },
    { path: '/details/:selected_country', component: CountryDetails, props: true },
    { path: '/interventions', component: Interventions }
  ]
  
  const router = new VueRouter({
    routes 
  })

// APP
var app = new Vue({
    router: router,
    el: '#covid19model',
    data: {
        latest_updates: {},
        interventions: {},
        selected_country: ""
    },
    computed: {
        countries: function () {
            return Object.keys(this.latest_updates).sort();
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

            this.isLoading = false
        }
    },
});