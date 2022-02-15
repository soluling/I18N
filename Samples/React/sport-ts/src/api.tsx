import { createApi, fetchBaseQuery } from '@reduxjs/toolkit/query/react'
import { Sport } from './sport'

export const sportApi = createApi({
  reducerPath: 'sportApi',
  baseQuery: fetchBaseQuery({ baseUrl: 'https://soluling.com/SportAPI' }),
  endpoints: (builder) => ({
    sports: builder.query<Sport[], void>({
      query: () => '/sports'
    })
  })
})

export const { useSportsQuery } = sportApi;