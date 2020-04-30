<template>
  <div id="app">
    <Sports />
  </div>
</template>

<script>
import Sports from './components/Sports.vue'

export default {
  name: 'App',
  components: {
    Sports
  }
}
</script>

<style>
#app {
  margin: 20px;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', 'Oxygen',
    'Ubuntu', 'Cantarell', 'Fira Sans', 'Droid Sans', 'Helvetica Neue',
    sans-serif;
}
</style>
