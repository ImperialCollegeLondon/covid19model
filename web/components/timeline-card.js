Vue.component('timeline-card', {
    props: ['id', 'date', 'title', 'text', 'sources'],
    template: `<div class="card mb-4">
                <div class="card-header bg-primary text-white">{{ date }}</div>
                <div class="card-body">
                    <h5 class="card-title">{{ title }}</h5>
                    <p class="card-text">{{ text }}</p>
                </div>
                <div class="card-footer text-muted">
                    <p class="card-text mb-0">
                        <a data-toggle="collapse" v-bind:href="'#timeline-card-' + this._uid" role="button"
                            aria-expanded="false" aria-controls="collapseExample">
                            <small>Source</small>
                        </a>
                    </p>
                    <div class="collapse" v-bind:id="'timeline-card-' + this._uid">
                        <p class="card-text" v-for="source in sources">
                            <small class="text-muted">{{ source }}</small>
                        </p>
                    </div>
                </div>
            </div>`
});