Vue.component('responsive-figure', {
    props: ['type', 'country', 'alt'],
    template: `<div>
                    <picture v-bind:id="type + '-picture'">
                        <source media="(max-width: 720px)" v-bind:id="type + '-mobile'"
                            v-bind:srcset="'figures/mobile/' + country + '_' + type + '.svg'">
                        <img v-bind:src="'figures/desktop/' + country + '_' + type + '.svg'" 
                        v-bind:id="type + '-desktop'" class="img-fluid" v-bind:alt=alt>
                    </picture>
                    <div v-bind:id="type + '-overlay'" class="overlay">
                        <div class="w-100 d-flex justify-content-center align-items-center">
                            <div class="spinner"></div>
                        </div>
                    </div>
                </div>`,
    watch: {
        country: function(val) {
            function setSpinner(name) {
                // Ensures that no stale images are ever shown, even if the network is down 
                $("#" + name + "-overlay").css("display", "flex");
                $("#" + name + "-picture").imagesLoaded()
                    .done(function (instance) {
                        $("#" + name + "-overlay").css("display", "none")
                    });
            };
            console.log(this.type);
            setSpinner(this.type);
        }
    }
});