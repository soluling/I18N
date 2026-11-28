import { createApp } from 'vue';
import App from './App.vue';
import i18n from './i18n';
import './index.css';

createApp(App).use(i18n).mount('#app');
