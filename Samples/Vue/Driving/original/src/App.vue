<!-- The original English application that has not been internationalized -->
<script setup>
import { computed, ref } from 'vue';

const distance = ref(220);
const speed = ref(88);
const result = ref(null);

const canCalculate = computed(() => (distance.value > 0) && (speed.value > 0));

function calculate() {
  const totalMinutes = Math.round(60*distance.value/speed.value);

  result.value = { hours: Math.floor(totalMinutes/60), minutes: totalMinutes % 60 };
}
</script>

<template>
  <form @submit.prevent="calculate">
    <h1>Driving Time</h1>

    <label>
      Driving distance
      <input type="number" title="Distance in kilometers" v-model.number="distance" />
    </label>

    <label>
      Driving speed
      <input type="number" title="Speed in kilometers per hour" v-model.number="speed" />
    </label>

    <button type="submit" :disabled="!canCalculate">Calculate</button>

    <p v-if="result">Driving time is {{ result.hours }} hour(s) {{ result.minutes }} minute(s)</p>
  </form>
</template>
