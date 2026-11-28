<script setup>
import { computed, ref, watchEffect } from 'vue';
import { useI18n } from 'vue-i18n';
import { STORAGE_KEY } from './i18n';

const { t, locale } = useI18n();

const distance = ref(220);
const speed = ref(88);
const result = ref(null);

const canCalculate = computed(() => (distance.value > 0) && (speed.value > 0));

// Strings in code: the page title and the language of the document follow the active language
watchEffect(() => {
  document.title = t('pageTitle');
  document.documentElement.lang = locale.value;
});

function calculate() {
  const totalMinutes = Math.round(60*distance.value/speed.value);

  result.value = { hours: Math.floor(totalMinutes/60), minutes: totalMinutes % 60 };
}

// Plural forms: t(key, count) selects the form, and {n} is the count
const resultText = computed(() => {
  const minutes = t('duration.minutes', result.value.minutes);

  if (result.value.hours === 0)
    return t('resultMinutes', { minutes });
  else
    return t('result', { hours: t('duration.hours', result.value.hours), minutes });
});

function changeLanguage(value) {
  localStorage.setItem(STORAGE_KEY, value);
  locale.value = value;
}
</script>

<template>
  <form @submit.prevent="calculate">
    <h1>{{ t('drivingTime') }}</h1>

    <label>
      {{ t('distance') }}
      <input type="number" :title="t('distanceHint')" v-model.number="distance" />
    </label>

    <label>
      {{ t('speed') }}
      <input type="number" :title="t('speedHint')" v-model.number="speed" />
    </label>

    <button type="submit" :disabled="!canCalculate">{{ t('calculate') }}</button>

    <p v-if="result">{{ resultText }}</p>

    <label>
      {{ t('language') }}
      <select :value="locale" @change="changeLanguage($event.target.value)">
        <option value="en">English</option>
        <option value="fi">suomi</option>
        <option value="de">Deutsch</option>
        <option value="ja">日本語</option>
      </select>
    </label>
  </form>
</template>
