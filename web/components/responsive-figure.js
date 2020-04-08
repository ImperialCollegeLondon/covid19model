Vue.component('responsive-figure', {
    props: ['type', 'country', 'alt'],
    template: `<picture v-bind:id="type + '-picture'">
                    <source media="(max-width: 720px)" v-bind:id="type + '-mobile'"
                        v-bind:srcset="'figures/mobile/' + country + '_' + type + '.svg'">
                    <img v-bind:src="'figures/desktop/' + country + '_' + type + '.svg'" 
                    v-bind:id="type + '-desktop'" class="img-fluid" v-bind:alt=alt>
                </picture>`
});