<template>
  <div>
    <h1>{{ $t('Sports') }}</h1>
    <b-table striped hover :items="items" :fields="fields">></b-table>
  </div>
</template>

<script>
export default 
{
  created() 
  {
    fetch(`https://soluling.com/sportapi/sports`)
      .then(res => res.json())
      .then(result => {
        this.items = [];

        for (let sport of result)
        {
          if (sport.olympic === "Summer")
            sport.olympicAsString = this.$t("Summer");
          else if (sport.olympic === "Winter")
            sport.olympicAsString = this.$t("Winter");
          else  
            sport.olympicAsString = "";

         if (sport.goalie)
           sport.goalieAsString = this.$t("Yes");
         else if (sport.goalie === false)
           sport.goalieAsString = this.$t("No");

          this.items.push(sport);
        }

        return this.items;
      });
  },

  data() 
  {
    return {
      fields: 
      [
        { key: 'id', label: this.$t('Id') }, 
        { key: 'languages[0].name', label: this.$t('Sport name') }, 
        { key: 'olympicAsString', label: this.$t('Olympic') },
        { key: 'fieldPlayers', label: this.$t('Number of payers') },
        { key: 'goalieAsString', label: this.$t('Goalkeeper') },
        { key: 'languages[0].origin', label: this.$t('Origin') },
        { key: 'languages[0].description', label: this.$t('Description') }
      ],
      items: []
    }
  }  
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
</style>
