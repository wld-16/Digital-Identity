import Vue from 'vue'
import App from './App.vue'
import VueCamera from './components/vue-camera.vue'

Vue.config.productionTip = false
Vue.component('camera-component', VueCamera)

new Vue({
  render: h => h(App),
}).$mount('#app')
