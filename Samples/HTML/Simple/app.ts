class Sample
{
  value: string;
  count: number;

  constructor(value: string, count: number) 
  {
    this.value = value;
    this.count = count;
  }

  start() 
  {
      var str: string = this.value;

      for (var i = 0; i < this.count; i++)
        str = this.value;
  }

  stop() 
  {
    this.value = "";
  }
}